#' Imputation by random forests
#'
#' Imputes univariate missing data using random forests.
#'
#' @aliases mice.impute.rf
#' @inheritParams mice.impute.pmm
#' @param ntree The number of trees to grow. The default is 10.
#' @param rfPackage A single string specifying the backend for estimating the
#' random forest. The default backend is the \code{ranger} package. An
#' alternative is \code{literanger} which predicts faster but does not support
#' all forest types and split rules from \code{ranger}. Also implemented as
#' an alternative is the \code{randomForest} package, which used to be the
#' default in mice 3.13.10 and earlier.
#' @param \dots Other named arguments passed down to
#' \code{mice:::install.on.demand()}, \code{randomForest::randomForest()},
#' \code{randomForest:::randomForest.default()}, \code{ranger::ranger()}, and
#' \code{literanger::train()}.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details
#' Imputation of \code{y} by random forests. The method
#' calls \code{randomForrest()} which implements Breiman's random forest
#' algorithm (based on Breiman and Cutler's original Fortran code)
#' for classification and regression. See Appendix A.1 of Doove et al.
#' (2014) for the definition of the algorithm used.
#'
#' The \pkg{ranger} and \pkg{literanger} backends are multithreaded. By
#' default, \code{mice.impute.rf()} caps their number of threads at
#' \code{parallelly::availableCores()}: \pkg{ranger} uses at most its own
#' default of 2 threads (or \code{getOption("Ncpus")}), and \pkg{literanger}
#' at most all available cores. This respects the limits of schedulers
#' such as Slurm and of containers, and gives one thread per chain on the
#' workers of \code{mice(..., future = TRUE)}, where
#' \code{availableCores()} is 1. The number of threads does not change the
#' imputations. To set it yourself, pass \code{num.threads} (\pkg{ranger})
#' or \code{n_thread} (\pkg{literanger}), e.g.
#' \code{mice(data, method = "rf", num.threads = 4)}. For \pkg{ranger},
#' the environment variable \code{R_RANGER_NUM_THREADS} and the option
#' \code{ranger.num.threads} are also respected. The number of threads is
#' used both to fit the forest and to predict.
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
#' of Epidemiology, \doi{10.1093/aje/kwt312}.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/sec-cart.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' @seealso \code{\link{mice}}, \code{\link{mice.impute.cart}},
#' \code{\link[randomForest]{randomForest}},
#' \code{\link[ranger]{ranger}},
#' \code{\link[literanger]{train}}
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' \dontrun{
#' imp <- mice(nhanes2, meth = "rf", ntree = 3)
#' plot(imp)
#' }
#' @export
mice.impute.rf <- function(
  y,
  ry,
  x,
  wy = NULL,
  ntree = 10,
  rfPackage = c("ranger", "randomForest", "literanger"),
  ...
) {
  rfPackage <- match.arg(rfPackage)

  if (is.null(wy)) {
    wy <- !ry
  }

  ntree <- max(1, ntree) # safety
  nmis <- sum(wy)
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[wy, , drop = FALSE]
  yobs <- y[ry]

  # Find eligible donors
  f <- switch(
    rfPackage,
    randomForest = .randomForest.donors,
    ranger = .ranger.donors,
    literanger = .literanger.donor
  )

  forest <- f(xobs, xmis, yobs, ntree, ...)

  # Short-circuit when using literanger interface
  if (rfPackage == "literanger") {
    return(forest)
  }
  # Sample from donors
  if (nmis == 1) {
    forest <- array(forest, dim = c(1, ntree))
  }
  apply(forest, MARGIN = 1, FUN = function(s) sample(unlist(s), 1))
}

# Find eligible donors using the randomForest package
.randomForest.donors <- function(xobs, xmis, yobs, ntree, ...) {
  install.on.demand("randomForest", ...)

  onetree <- function(xobs, xmis, yobs, ...) {
    # Function to fit a single tree
    fit <- randomForest::randomForest(
      x = xobs,
      y = yobs,
      ntree = 1,
      ...
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

# Find eligible donors using the ranger package (default)
.ranger.donors <- function(xobs, xmis, yobs, ntree, ...) {
  install.on.demand("ranger", ...)

  # Fit all trees at once
  args <- list(x = xobs, y = yobs, num.trees = ntree, ...)
  args$num.threads <- rf.threads(args[["num.threads"]], "ranger")
  fit <- suppressWarnings(do.call(ranger::ranger, args))

  nodes <- predict(
    object = fit,
    data = rbind(xobs, xmis),
    type = "terminalNodes",
    predict.all = TRUE,
    num.threads = args$num.threads
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

# Find eligible donors using the literanger package
.literanger.donor <- function(xobs, xmis, yobs, ntree, ...) {
  install.on.demand("literanger", ...)

  lr_formals <- names(formals(literanger::train))
  dots <- list(...)
  dots <- dots[intersect(names(dots), setdiff(lr_formals, c('x', 'y')))]
  dots$n_thread <- rf.threads(dots[["n_thread"]], "literanger")

  fit <- do.call(
    literanger::train,
    c(list(x = xobs, y = yobs, n_tree = ntree), dots)
  )
  predict(
    object = fit,
    newdata = xmis,
    prediction_type = "inbag",
    n_thread = if (is.null(dots$n_thread)) 0 else dots$n_thread
  )$values
}

# Number of threads for ranger and literanger. NULL means the default of
# the package. A value given by the user always wins, and for ranger, so
# do the R_RANGER_NUM_THREADS variable and the ranger.num.threads option.
# Otherwise, the default of the package (2 threads for ranger, or the
# Ncpus option; all cores for literanger) is capped at
# parallelly::availableCores(). This respects the limits of schedulers
# such as Slurm and of containers, and on future workers, where
# availableCores() is 1 to prevent nested parallelism, each chain then
# uses one thread.
rf.threads <- function(user = NULL, package = c("ranger", "literanger")) {
  package <- match.arg(package)
  if (!is.null(user)) {
    return(user)
  }
  if (
    package == "ranger" &&
      (nzchar(Sys.getenv("R_RANGER_NUM_THREADS")) ||
        !is.null(getOption("ranger.num.threads")))
  ) {
    return(NULL)
  }
  if (!requireNamespace("parallelly", quietly = TRUE)) {
    return(NULL)
  }
  cores <- unname(parallelly::availableCores())
  if (package == "ranger") {
    return(as.integer(min(getOption("Ncpus", 2L), cores)))
  }
  as.integer(cores)
}
