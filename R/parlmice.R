#' Wrapper function that runs MICE in parallel
#'
#' This function is included for backward compatibility. The function
#' is superseded by [futuremice()].
#'
#' This function relies on package [parallel()], which is a base
#' package for R versions 2.14.0 and later. We have chosen to use parallel function
#' `parLapply` to allow the use of `parlmice` on Mac, Linux and Windows
#' systems. For the same reason, we use the Parallel Socket Cluster (PSOCK) type by default.
#'
#' On systems other than Windows, it can be hugely beneficial to change the cluster type to
#' `FORK`, as it generally results in improved memory handling. When memory issues
#' arise on a Windows system, we advise to store the multiply imputed datasets,
#' clean the memory by using [rm()] and [gc()] and make another
#' run using the same settings.
#'
#' This wrapper function combines the output of [parLapply()] with
#' function [ibind()] in [mice()]. A `mids` object is returned
#' and can be used for further analyses.
#'
#' Note that if a seed value is desired, the seed should be entered to this function
#' with argument `seed`. Seed values outside the wrapper function (in an
#' R-script or passed to [mice()]) will not result to reproducible results.
#' We refer to the manual of [parallel()] for an explanation on this matter.
#'
#' @aliases parlmice
#' @param data A data frame or matrix containing the incomplete data. Similar to
#' the first argument of [mice()].
#' @param m The number of desired imputated datasets. By default $m=5$ as with `mice`
#' @param seed A scalar to be used as the seed value for the mice algorithm within
#' each parallel stream. Please note that the imputations will be the same for all
#' streams and, hence, this should be used if and only if `n.core = 1` and
#' if it is desired to obtain the same output as under `mice`.
#' @param n.core A scalar indicating the number of cores that should be used.
#' @param n.imp.core A scalar indicating the number of imputations per core.
#' @param cluster.seed A scalar to be used as the seed value. It is recommended to put the
#' seed value here and not outside this function, as otherwise the parallel processes
#' will be performed with separate, random seeds.
#' @param cl.type The cluster type. Default value is `"PSOCK"`. Posix machines (linux, Mac)
#' generally benefit from much faster cluster computation if `type` is set to `type = "FORK"`.
#' @param ... Named arguments that are passed down to function [mice()] or
#' [makeCluster()].
#'
#' @return A mids object as defined by [mids-class()]
#'
#' @author Gerko Vink, Rianne Schouten
#' @seealso [parallel()], [parLapply()], [makeCluster()],
#' [mice()], [mids-class()]
#' @references
#' Schouten, R. and Vink, G. (2017). parlmice: faster, paraleller, micer.
#' <https://www.gerkovink.com/parlMICE/Vignette_parlMICE.html>
#'
#' #'Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/parallel-computation.html)
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' @examples
#' # 150 imputations in dataset nhanes, performed by 3 cores
#' \dontrun{
#' imp1 <- parlmice(data = nhanes, n.core = 3, n.imp.core = 50)
#' # Making use of arguments in mice.
#' imp2 <- parlmice(data = nhanes, method = "norm.nob", m = 100)
#' imp2$method
#' fit <- with(imp2, lm(bmi ~ hyp))
#' pool(fit)
#' }
#'
#' @export
parlmice <- function(data, m = 5, seed = NA, cluster.seed = NA, n.core = NULL,
                     n.imp.core = NULL, cl.type = "PSOCK", ...) {
  .Deprecated("futuremice")
  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)

  # check if data complete
  if (sum(is.na(data)) == 0) {
    stop("Data has no missing values")
  }

  # check if arguments match CPU specifications
  if (!is.null(n.core)) {
    if (n.core > parallel::detectCores()) {
      stop("Number of cores specified is greater than the number of logical cores in your CPU")
    }
  }

  # determine course of action when not all arguments specified
  if (!is.null(n.core) & is.null(n.imp.core)) {
    n.imp.core <- m
    warning(paste("Number of imputations per core not specified: n.imp.core = m =", m, "has been used"))
  }
  if (is.null(n.core) & !is.null(n.imp.core)) {
    n.core <- parallel::detectCores() - 1
    warning(paste("Number of cores not specified. Based on your machine a value of n.core =", parallel::detectCores() - 1, "is chosen"))
  }
  if (is.null(n.core) & is.null(n.imp.core)) {
    specs <- match.cluster(n.core = parallel::detectCores() - 1, m = m)
    n.core <- specs$cores
    n.imp.core <- specs$imps
  }
  if (!is.na(seed)) {
    if (n.core > 1) {
      warning("Be careful; the specified seed is equal for all imputations. Please consider specifying cluster.seed instead.")
    }
  }

  # create arguments to export to cluster
  args <- match.call(mice, expand.dots = TRUE)
  args[[1]] <- NULL
  args$m <- n.imp.core

  # make computing cluster
  cl <- parallel::makeCluster(n.core, type = cl.type)
  parallel::clusterExport(cl,
    varlist = c(
      "data", "m", "seed", "cluster.seed",
      "n.core", "n.imp.core", "cl.type",
      ls(parent.frame())
    ),
    envir = environment()
  )
  parallel::clusterExport(cl,
    varlist = "do.call"
  )
  parallel::clusterEvalQ(cl, library(mice))
  if (!is.na(cluster.seed)) {
    parallel::clusterSetRNGStream(cl, cluster.seed)
  }

  # generate imputations
  imps <- parallel::parLapply(cl = cl, X = 1:n.core, function(x) do.call(mice, as.list(args), envir = environment()))
  parallel::stopCluster(cl)

  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  # let imputation matrix correspond to grand m
  for (i in 1:length(imp$imp)) {
    colnames(imp$imp[[i]]) <- 1:imp$m
  }

  imp
}

match.cluster <- function(n.core, m) {
  cores <- 1:n.core
  imps <- 1:m
  out <- data.frame(
    results = as.vector(cores %*% t(imps)),
    cores = cores,
    imps = rep(imps, each = n.core)
  )
  which <- out[out[, "results"] == m, ]
  which[order(which$cores, decreasing = T), ][1, 2:3]
}
