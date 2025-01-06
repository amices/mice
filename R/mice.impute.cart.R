#' Imputation by classification and regression trees
#'
#' Imputes univariate missing data using classification and regression trees.
#'
#' @aliases mice.impute.cart cart
#'
#' @inheritParams mice.impute.pmm
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @param minbucket The minimum number of observations in any terminal node used.
#' See \code{\link[rpart]{rpart.control}} for details.
#' @param cp Complexity parameter. Any split that does not decrease the overall
#' lack of fit by a factor of cp is not attempted. See
#' \code{\link[rpart]{rpart.control}} for details.
#' @param ... Other named arguments passed down to \code{rpart()}.
#' @return Numeric vector of length \code{sum(!ry)} with imputations
#' @details
#' Imputation of \code{y} by classification and regression trees. The procedure
#' is as follows:
#' \enumerate{
#' \item Fit a classification or regression tree by recursive partitioning;
#' \item For each \code{ymis}, find the terminal node they end up according to the fitted tree;
#' \item Make a random draw among the member in the node, and take the observed value from that
#' draw as the imputation.
#' }
#' @seealso \code{\link{mice}}, \code{\link{mice.impute.rf}},
#' \code{\link[rpart]{rpart}}, \code{\link[rpart]{rpart.control}}
#' @author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012
#' @references
#'
#' Doove, L.L., van Buuren, S., Dusseldorp, E. (2014), Recursive partitioning
#' for missing data imputation in the presence of interaction Effects.
#' Computational Statistics & Data Analysis, 72, 92-104.
#'
#' Breiman, L., Friedman, J. H., Olshen, R. A., and Stone, C. J.
#' (1984), Classification and regression trees, Monterey, CA: Wadsworth &
#' Brooks/Cole Advanced Books & Software.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-cart.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' @family univariate imputation functions
#' @examples
#' imp <- mice(nhanes2, meth = "cart", minbucket = 4)
#' plot(imp)
#' @keywords datagen
#' @export
mice.impute.cart <- function(y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04,
                             ...) {
  install.on.demand("rpart")

  if (is.null(wy)) {
    wy <- !ry
  }
  minbucket <- max(1, minbucket)
  if (dim(x)[2] == 0) {
    x <- cbind(x, 1)
    dimnames(x) <- list(NULL, "int")
  }

  xobs <- data.frame(x[ry, , drop = FALSE])
  xmis <- data.frame(x[wy, , drop = FALSE])
  yobs <- y[ry]
  if (!is.factor(yobs)) {
    fit <- rpart::rpart(yobs ~ .,
      data = cbind(yobs, xobs), method = "anova",
      control = rpart::rpart.control(minbucket = minbucket, cp = cp, ...)
    )
    leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, ])))
    fit$frame$yval <- as.numeric(row.names(fit$frame))
    nodes <- predict(object = fit, newdata = xmis)
    donor <- lapply(nodes, function(s) yobs[leafnr == s])
    impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 1), numeric(1))
  } else {
    # escape with same impute if the dependent does not vary
    cat.has.all.obs <- table(yobs) == sum(ry)
    if (any(cat.has.all.obs)) {
      return(rep(levels(yobs)[cat.has.all.obs], sum(wy)))
    }

    xy <- cbind(yobs, xobs)
    xy <- droplevels(xy)
    # FIXME: rpart fails to runs on empty categories in yobs,
    # droplevels() removes empty levels, and this is
    # likely to present problems further down the road
    # potential problem case: table(yobs): 0 10 15, then
    # droplevels may forget about category 1
    fit <- rpart::rpart(yobs ~ .,
      data = xy, method = "class",
      control = rpart::rpart.control(minbucket = minbucket, cp = cp, ...)
    )
    nodes <- predict(object = fit, newdata = xmis)
    impute <- apply(nodes,
      MARGIN = 1,
      FUN = function(s) {
        sample(colnames(nodes),
          size = 1, prob = s
        )
      }
    )
  }
  impute
}
