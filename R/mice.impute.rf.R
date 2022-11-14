#' Imputation by random forests
#'
#' Imputes univariate missing data using random forests.
#'
#' @aliases mice.impute.rf
#' @inheritParams mice.impute.pmm
#' @param ntree The number of trees to grow. The default is 10.
#' @param rfPackage A single string specifying the backend for estimating the
#' random forest. The default backend is the \code{ranger} package. The only
#' alternative currently implemented is the \code{randomForest} package, which
#' used to be the default in mice 3.13.10 and earlier.
#' @param \dots Other named arguments passed down to
#' \code{mice:::install.on.demand()}, \code{randomForest::randomForest()} and
#' \code{randomForest:::randomForest.default()}.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Imputation of \code{y} by random forests. The method
#' calls \code{randomForrest()} which implements Breiman's random forest
#' algorithm (based on Breiman and Cutler's original Fortran code)
#' for classification and regression. See Appendix A.1 of Doove et al.
#' (2014) for the definition of the algorithm used.
#' @note An alternative implementation was independently
#' developed by Shah et al (2014). This were available as
#' functions \code{CALIBERrfimpute::mice.impute.rfcat} and
#' \code{CALIBERrfimpute::mice.impute.rfcont} (now archived).
#' Simulations by Shah (Feb 13, 2014) suggested that
#' the quality of the imputation for 10 and 100 trees was identical,
#' so mice 2.22 changed the default number of trees from \code{ntree = 100} to
#' \code{ntree = 10}.
#' @author Lisa Doove, Stef van Buuren, Elise Dusseldorp, 2012; Patrick Rockenschaub, 2021
#' @references
#'
#' Doove, L.L., van Buuren, S., Dusseldorp, E. (2014), Recursive partitioning
#' for missing data imputation in the presence of interaction Effects.
#' Computational Statistics & Data Analysis, 72, 92-104.
#'
#' Shah, A.D., Bartlett, J.W., Carpenter, J., Nicholas, O., Hemingway, H. (2014),
#' Comparison of random forest and parametric imputation models for
#' imputing missing data using MICE: A CALIBER study. American Journal
#' of Epidemiology, doi: 10.1093/aje/kwt312.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-cart.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' @seealso \code{\link{mice}}, \code{\link{mice.impute.cart}},
#' \code{\link[randomForest]{randomForest}}
#' \code{\link[ranger]{ranger}}
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' library("lattice")
#'
#' imp <- mice(nhanes2, meth = "rf", ntree = 3)
#' plot(imp)
#' @export
mice.impute.rf <- function(y, ry, x, wy = NULL, ntree = 10,
                           rfPackage = c("ranger", "randomForest"), ...) {
  rfPackage <- match.arg(rfPackage)

  if (is.null(wy)) wy <- !ry

  ntree <- max(1, ntree) # safety
  nmis <- sum(wy)
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- y[ry]

  # Find eligible donors
  f <- switch(rfPackage,
    randomForest = .randomForest.donors,
    ranger = .ranger.donors
  )

  forest <- f(xobs, xmis, yobs, ntree, ...)

  # Sample from donors
  if (nmis == 1) forest <- array(forest, dim = c(1, ntree))
  apply(forest, MARGIN = 1, FUN = function(s) sample(unlist(s), 1))
}

# Find eligible donors using the randomForest package (default)
.randomForest.donors <- function(xobs, xmis, yobs, ntree, ...) {
  install.on.demand("randomForest", ...)

  onetree <- function(xobs, xmis, yobs, ...) {
    # Function to fit a single tree
    fit <- randomForest::randomForest(
      x = xobs,
      y = yobs,
      ntree = 1, ...
    )
    leafnr <- predict(object = fit, newdata = xobs, nodes = TRUE)
    leafnr <- as.vector(attr(leafnr, "nodes"))
    nodes <- predict(object = fit, newdata = xmis, nodes = TRUE)
    nodes <- as.vector(attr(nodes, "nodes"))
    donor <- lapply(nodes, function(s) yobs[leafnr == s])
    return(donor)
  }

  sapply(seq_len(ntree), FUN = function(s) onetree(xobs, xmis, yobs, ...))
}

# Find eligible donors using the ranger package
.ranger.donors <- function(xobs, xmis, yobs, ntree, ...) {
  install.on.demand("ranger", ...)

  # Fit all trees at once
  fit <- ranger::ranger(x = xobs, y = yobs, num.trees = ntree)

  nodes <- predict(
    object = fit, data = rbind(xobs, xmis),
    type = "terminalNodes", predict.all = TRUE
  )
  nodes <- ranger::predictions(nodes)
  nodes_obs <- nodes[1:nrow(xobs), , drop = FALSE]
  nodes_mis <- nodes[(nrow(xobs) + 1):nrow(nodes), , drop = FALSE]

  select_donors <- function(i) {
    # Function to extract all eligible donors for each missing value
    donors <- split(yobs, nodes_obs[, i])
    donors[as.character(nodes_mis[, i])]
  }

  sapply(seq_len(ntree), FUN = select_donors)
}
