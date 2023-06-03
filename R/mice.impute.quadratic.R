#' Imputation of quadratic terms
#'
#' Imputes incomplete variable that appears as both
#' main effect and quadratic effect in the complete-data model.
#'
#' @aliases mice.impute.quadratic quadratic
#' @inheritParams mice.impute.pmm
#' @param quad.outcome The name of the outcome in the quadratic analysis as a
#' character string. For example, if the substantive model of interest is
#' \code{y ~ x + xx}, then \code{"y"} would be the \code{quad.outcome}
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' This function implements the "polynomial combination" method.
#' First, the polynomial
#' combination \eqn{Z = Y \beta_1 + Y^2 \beta_2} is formed.
#' \eqn{Z} is imputed by
#' predictive mean matching, followed by a decomposition of the imputed
#' data \eqn{Z}
#' into components \eqn{Y} and \eqn{Y^2}.
#' See Van Buuren (2012, pp. 139-141) and Vink
#' et al (2012) for more details. The method ensures that 1) the imputed data
#' for \eqn{Y} and \eqn{Y^2} are mutually consistent, and 2) that provides unbiased
#' estimates of the regression weights in a complete-data linear regression that
#' use both \eqn{Y} and \eqn{Y^2}.
#'
#' @note There are two situations to consider. If only the linear term \code{Y}
#' is present in the data, calculate the quadratic term \code{YY} after
#' imputation. If both the linear term \code{Y} and the the quadratic term
#' \code{YY} are variables in the data, then first impute \code{Y} by calling
#' \code{mice.impute.quadratic()} on \code{Y}, and then impute \code{YY} by
#' passive imputation as \code{meth["YY"] <- "~I(Y^2)"}.  See example section
#' for details.  Generally, we would like \code{YY} to be present in the data if
#' we need to preserve quadratic relations between \code{YY} and any third
#' variables in the multivariate incomplete data that we might wish to impute.
#' @author Mingyang Cai and Gerko Vink
#' @seealso \code{\link{mice.impute.pmm}}
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-knowledge.html#sec:quadratic}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Vink, G., van Buuren, S. (2013). Multiple Imputation of Squared Terms.
#' \emph{Sociological Methods & Research}, 42:598-607.
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' # Create Data
#' B1 <- .5
#' B2 <- .5
#' X <- rnorm(1000)
#' XX <- X^2
#' e <- rnorm(1000, 0, 1)
#' Y <- B1 * X + B2 * XX + e
#' dat <- data.frame(x = X, xx = XX, y = Y)
#'
#' # Impose 25 percent MCAR Missingness
#' dat[0 == rbinom(1000, 1, 1 - .25), 1:2] <- NA
#'
#' # Prepare data for imputation
#' ini <- mice(dat, maxit = 0)
#' meth <- c("quadratic", "~I(x^2)", "")
#' pred <- ini$pred
#' pred[, "xx"] <- 0
#'
#' # Impute data
#' imp <- mice(dat, meth = meth, pred = pred, quad.outcome = "y")
#'
#' # Pool results
#' pool(with(imp, lm(y ~ x + xx)))
#'
#' # Plot results
#' stripplot(imp)
#' plot(dat$x, dat$xx, col = mdc(1), xlab = "x", ylab = "xx")
#' cmp <- complete(imp)
#' points(cmp$x[is.na(dat$x)], cmp$xx[is.na(dat$x)], col = mdc(2))
#' @export
mice.impute.quadratic <- function(y, ry, x, wy = NULL, quad.outcome = NULL, ...) {
  if (is.null(quad.outcome)) stop("Argument 'quad.outcome' for mice.impute.quadratic has not been specified")
  if (!quad.outcome %in% colnames(x)) stop("The name specified for the outcome in 'quad.outcome' can not be found in the data")
  if (is.null(wy)) {
    wy <- !ry
  }
  x <- cbind(1, as.matrix(x))
  # create the square of y
  y2 <- y^2
  # create z based on B1 * y + B2 * y^2
  parm <- .norm.draw(x[, quad.outcome], ry, cbind(1, y, y2))
  zobs <- cbind(y, y2) %*% parm$coef[-1]
  # impute z
  zmis <- mice.impute.pmm(zobs, ry, x[, -1])
  zstar <- zobs
  zstar[!ry] <- zmis
  zstar <- as.vector(zstar)
  # decompositions of z into roots
  b1 <- parm$coef[2]
  b2 <- parm$coef[3]
  y.low <- -(1 / (2 * b2)) * (sqrt(4 * b2 * zstar + b1^2) + b1)
  y.up <- (1 / (2 * b2)) * (sqrt(4 * b2 * zstar + b1^2) - b1)
  # calculate the abscissa at the parabolic minimum/maximum
  y.min <- -b1 / (2 * b2)
  # data augmentation
  data.augment <- data.frame(
    V = c((y > y.min)[ry] * 1, 1, 1, 0, 0, 1, 1, 0, 0),
    q = c(
      x[ry, quad.outcome],
      mean(x[ry, quad.outcome]) + sd(x[ry, quad.outcome]),
      mean(x[ry, quad.outcome]) - sd(x[ry, quad.outcome]),
      mean(x[ry, quad.outcome]) + sd(x[ry, quad.outcome]),
      mean(x[ry, quad.outcome]) - sd(x[ry, quad.outcome]),
      mean(x[ry, quad.outcome]), mean(x[ry, quad.outcome]),
      mean(x[ry, quad.outcome]), mean(x[ry, quad.outcome])
    ),
    zstar = c(
      zstar[ry],
      mean(zstar[ry]), mean(zstar[ry]),
      mean(zstar[ry]), mean(zstar[ry]),
      mean(zstar[ry]) + sd(zstar[ry]),
      mean(zstar[ry]) - sd(zstar[ry]),
      mean(zstar[ry]) + sd(zstar[ry]),
      mean(zstar[ry]) - sd(zstar[ry])
    )
  )
  w <- c(rep(1, nrow(data.augment) - 8), rep(3 / 8, 8))
  # calculate regression parameters for
  vobs <- glm(V ~ q + zstar + q * zstar,
    family = quasibinomial,
    data = data.augment, weights = w
  )
  # impute Vmis
  newdata <- data.frame(q = x[wy, quad.outcome], zstar = zstar[wy])
  prob <- predict(vobs,
    newdata = newdata, type = "response",
    na.action = na.exclude
  )
  idy <- rbinom(sum(wy), 1, prob = prob)
  # create final imputation
  ystar <- y.low[wy]
  ystar[idy == 1] <- y.up[wy][idy == 1]
  return(ystar)
}
