#' Wrapper function that runs MICE in parallel
#'
#' This is a wrapper function for \code{\link{mice}}, using multiple cores to
#' execute \code{\link{mice}} in parallel. As a result, the imputation
#' procedure can be sped up, which may be useful in general. By default,
#' \code{\link{futuremice}} distributes the number of imputations \code{m}
#' about equally over the cores.
#'
#' This function relies on package \code{\link[furrr]{furrr}}, which is a
#' package for R versions 3.2.0 and later. We have chosen to use furrr function
#' \code{future_map} to allow the use of \code{futuremice} on Mac, Linux and
#' Windows systems.
#'
#'
#' This wrapper function combines the output of \code{\link[furrr]{future_map}} with
#' function \code{\link{ibind}} from the \code{\link{mice}} package. A
#' \code{mids} object is returned and can be used for further analyses.
#'
#' A seed value can be specified in the global environment, which will yield
#' reproducible results. A seed value can also be specified within the
#' \code{\link{futuremice}} call, through specifying the argument
#' \code{parallelseed}. If \code{parallelseed} is not specified, a seed value is
#' drawn randomly by default, and accessible through \code{$parallelseed} in the
#' output object. Hence, results will always be reproducible, regardless of
#' whether the seed is specified in the global environment, or by setting the
#' same seed within the function (potentially by extracting the seed from the
#' \code{futuremice} output object.
#'
#' @aliases futuremice
#' @param data A data frame or matrix containing the incomplete data. Similar to
#' the first argument of \code{\link{mice}}.
#' @param m The number of desired imputated datasets. By default $m=5$ as with
#' \code{mice}
#' @param parallelseed A scalar to be used to obtain reproducible results over
#' the futures. The default \code{parallelseed = NA} will result in a seed value
#' that is randomly drawn between -999999999 and 999999999.
#' @param n.core A scalar indicating the number of cores that should be used.
#' @param seed A scalar to be used as the seed value for the mice algorithm
#' within each parallel stream. Please note that the imputations will be the
#' same for all streams and, hence, this should be used if and only if
#' \code{n.core = 1} and if it is desired to obtain the same output as under
#' \code{mice}.
#' @param use.logical A logical indicating whether logical (\code{TRUE}) or
#' physical (\code{FALSE}) CPU's on machine should be used.
#' @param future.plan A character indicating how \code{future}s are resolved.
#' The default \code{multisession} resolves futures asynchronously (in parallel)
#' in separate \code{R} sessions running in the background. See
#' \code{\link[future]{plan}} for more information on future plans.
#' @param packages A character vector with additional packages to be used in 
#' \code{mice} (e.g., for using external imputation functions).
#' @param globals A character string with additional functions to be exported to
#' each future (e.g., user-written imputation functions).
#' @param ... Named arguments that are passed down to function \code{\link{mice}}.
#'
#' @return A mids object as defined by \code{\link{mids-class}}
#'
#' @author Thom Benjamin Volker, Gerko Vink
#' @seealso \code{\link[future]{future}}, \code{\link[furrr]{furrr}}, \code{\link[furrr]{future_map}},
#' \code{\link[future]{plan}}, \code{\link{mice}}, \code{\link{mids-class}}
#' @references
#' Volker, T.B. and Vink, G. (2022). futuremice: The future starts today.
#' \url{https://www.gerkovink.com/miceVignettes/futuremice/Vignette_futuremice.html}
#'
#' #'Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/parallel-computation.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' @examples
#' # 150 imputations in dataset nhanes, performed by 3 cores
#' \dontrun{
#' imp1 <- futuremice(data = nhanes, m = 150, n.core = 3)
#' # Making use of arguments in mice.
#' imp2 <- futuremice(data = nhanes, m = 100, method = "norm.nob")
#' imp2$method
#' fit <- with(imp2, lm(bmi ~ hyp))
#' pool(fit)
#' }
#'
#' @export
futuremice <- function(data, m = 5, parallelseed = NA, n.core = NULL, seed = NA,
                       use.logical = TRUE, future.plan = "multisession", 
                       packages = NULL, globals = NULL, ...) {
  # check if pacakages available
  install.on.demand("parallelly", ...)
  install.on.demand("furrr", ...)
  install.on.demand("future", ...)

  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)

  # check if data complete
  if (sum(is.na(data)) == 0) {
    stop("Data has no missing values")
  }

  # number of available cores
  available <- parallelly::availableCores(logical = use.logical)

  # set the number of cores
  n.core <- check.cores(n.core, available, m)

  if (n.core > 1) {
    dist.core <- cut(1:m, n.core, labels = paste0("core", 1:n.core))
  } else {
    dist.core <- rep("core1", m)
  }

  n.imp.core <- as.vector(table(dist.core))

  if (!is.na(seed)) {
    if (n.core > 1) {
      if (interactive()) {
        msg <- "Be careful; specifying seed rather than parallelseed results in duplicate imputations.\nDo you want to continue?\n"
        ask <- askYesNo(msg, prompts = getOption("askYesNo", gettext(c("Yes", "No, ignore seed", "Cancel"))))

        if (isTRUE(ask)) {
          seed <- seed
          warning("Be careful; the imputations will be the same over the cores.")
        } else if (isFALSE(ask)) {
          seed <- NA
          message("Parallelseed is specified for you, and is accessible in the output object under $parallelseed.")
        } else if (is.na(ask)) {
          stop("You stopped futuremice. To obtain unique, but reproducible imputations, specify parallelseed.")
        }
      } else {
        warning("Be careful; the imputations will be identical over the cores. Perhaps you want to specify parallelseed, for unique, but reproducible results.")
      }
    }
  }
  if (!is.na(parallelseed)) {
    set.seed(parallelseed)
  } else {
    if(!exists(".Random.seed")) {
      set.seed(NULL)
    }
    parallelseed <- get(
      ".Random.seed",
      envir = globalenv(), 
      mode = "integer",
      inherits = FALSE
    )
  }

  # start multisession
  future::plan(future.plan,
    workers = n.core
  )

  # begin future
  imps <- furrr::future_map(
    n.imp.core, 
    function(x) {
      mice(data = data,
           m = x,
           printFlag = FALSE,
           seed = seed,
           ...
      )},
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = globals,
      packages = packages
    )
  )

  # end multisession
  future::plan(future::sequential)

  # stitch future into mids

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

  imp$parallelseed <- parallelseed

  return(imp)
}

check.cores <- function(n.core, available, m) {
  if (is.null(n.core)) {
    n.core <- min(available - 1, m)
  } else {
    if (n.core > available | n.core > m) {
      warning(paste("'n.core' exceeds the maximum number of available cores on your machine or the number of imputations, and is set to", min(available - 1, m)))
    }
    n.core <- min(available - 1, m, n.core)
  }
  n.core
}