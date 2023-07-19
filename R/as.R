#' Converts an imputed dataset (long format) into a \code{mids} object
#'
#' This function converts imputed data stored in long format into
#' an object of class \code{mids}. The original incomplete dataset
#' needs to be available so that we know where the missing data are.
#' The function is useful to convert back operations applied to
#' the imputed data back in a \code{mids} object. It may also be
#' used to store multiply imputed data sets from other software
#' into the format used by \code{mice}.
#' @note The function expects the input data \code{long} to be sorted by
#' imputation number (variable \code{".imp"} by default), and in the
#' same sequence within each imputation block.
#' @param long A multiply imputed data set in long format, for example
#' produced by a call to \code{complete(..., action = 'long', include = TRUE)},
#' or by other software.
#' @param .imp An optional column number or column name in \code{long},
#' indicating the imputation index. The values are assumed to be consecutive
#' integers between 0 and \code{m}. Values \code{1} through \code{m}
#' correspond to the imputation index, value \code{0} indicates
#' the original data (with missings).
#' By default, the procedure will search for a variable named \code{".imp"}.
#' @param .id An optional column number or column name in \code{long},
#' indicating the subject identification. If not specified, then the
#' function searches for a variable named \code{".id"}. If this variable
#' is found, the values in the column will define the row names in
#' the \code{data} element of the resulting \code{mids} object.
#' @inheritParams mice
#' @return An object of class \code{mids}
#' @author Gerko Vink
#' @examples
#' # impute the nhanes dataset
#' imp <- mice(nhanes, print = FALSE)
#' # extract the data in long format
#' X <- complete(imp, action = "long", include = TRUE)
#' # create dataset with .imp variable as numeric
#' X2 <- X
#'
#' # nhanes example without .id
#' test1 <- as.mids(X)
#' is.mids(test1)
#' identical(complete(test1, action = "long", include = TRUE), X)
#'
#' # nhanes example without .id where .imp is numeric
#' test2 <- as.mids(X2)
#' is.mids(test2)
#' identical(complete(test2, action = "long", include = TRUE), X)
#'
#' # nhanes example, where we explicitly specify .id as column 2
#' test3 <- as.mids(X, .id = ".id")
#' is.mids(test3)
#' identical(complete(test3, action = "long", include = TRUE), X)
#'
#' # nhanes example with .id where .imp is numeric
#' test4 <- as.mids(X2, .id = 6)
#' is.mids(test4)
#' identical(complete(test4, action = "long", include = TRUE), X)
#'
#' # example without an .id variable
#' # variable .id not preserved
#' X3 <- X[, -6]
#' test5 <- as.mids(X3)
#' is.mids(test5)
#' identical(complete(test5, action = "long", include = TRUE)[, -6], X[, -6])
#'
#' # as() syntax has fewer options
#' test7 <- as(X, "mids")
#' test8 <- as(X2, "mids")
#' test9 <- as(X2[, -6], "mids")
#' rev <- ncol(X):1
#' test10 <- as(X[, rev], "mids")
#'
#' # where argument copies also observed data into $imp element
#' where <- matrix(TRUE, nrow = nrow(nhanes), ncol = ncol(nhanes))
#' colnames(where) <- colnames(nhanes)
#' test11 <- as.mids(X, where = where)
#' identical(complete(test11, action = "long", include = TRUE), X)
#' @keywords mids
#' @export
as.mids <- function(long, where = NULL, .imp = ".imp", .id = ".id") {
  if (is.numeric(.imp)) .imp <- names(long)[.imp]
  if (is.numeric(.id)) .id <- names(long)[.id]
  if (!.imp %in% names(long)) stop("Imputation index `.imp` not found")

  # no missings allowed in .imp
  imps <- unlist(long[, .imp], use.names = FALSE)
  if (anyNA(imps)) stop("Missing values in imputation index `.imp`")

  # number of records within .imp should be the same
  if (any(diff(table(imps))) != 0) {
    stop("Unequal group sizes in imputation index `.imp`")
  }

  # get original data part
  keep <- setdiff(names(long), na.omit(c(.imp, .id)))
  data <- long[imps == 0, keep, drop = FALSE]
  n <- nrow(data)
  if (n == 0) {
    stop("Original data not found.\n Use `complete(..., action = 'long', include = TRUE)` to save original data.")
  }

  # determine m
  m <- length(unique(imps)) - 1

  # use mice to get info on data
  if (is.null(where)) where <- is.na(data)
  ini <- mice(data,
    m = m, where = where, maxit = 0,
    remove.collinear = FALSE, allow.na = TRUE
  )

  # create default .id when .id using type from input data
  # otherwise store provided .id as row names
  if (!.id %in% names(long)) {
    if (typeof(attr(long, "row.names")) == "integer") {
      row.names(ini$data) <- seq_len(nrow(ini$data))
    } else {
      row.names(ini$data) <- as.character(seq_len(nrow(ini$data)))
    }
  } else {
    row.names(ini$data) <- unlist(long[imps == 0, .id], use.names = FALSE)
  }

  # copy imputations from long into proper ini$imp elements
  names <- names(ini$imp)
  for (i in seq_along(names)) {
    varname <- names[i]
    if (!is.null(ini$imp[[varname]])) {
      for (j in seq_len(m)) {
        idx <- imps == j & where[, varname]
        ini$imp[[varname]][j] <- long[idx, varname]
      }
    }
  }
  ini
}

#' Create a \code{mira} object from repeated analyses
#'
#' The \code{as.mira()} function takes the results of repeated
#' complete-data analysis stored as a list, and turns it
#' into a \code{mira} object that can be pooled.
#' @param fitlist A list containing $m$ fitted analysis objects
#' @return An S3 object of class \code{mira}.
#' @seealso \code{\link[=mira-class]{mira}}
#' @author Stef van Buuren
#' @export
as.mira <- function(fitlist) {
  if (is.mira(fitlist)) {
    return(fitlist)
  }
  if (is.mids(fitlist)) {
    stop("as.mira() cannot convert class 'mids' into 'mira'. Use with() instead.")
  }
  call <- match.call()
  if (!is.list(fitlist)) {
    stop("Argument 'fitlist' is not a list")
  }
  class(fitlist) <- "list"
  object <- list(call = call, call1 = NULL, nmis = NULL, analyses = fitlist)
  oldClass(object) <- c("mira", "matrix")
  object
}

#' Converts into a \code{mitml.result} object
#'
#' The \code{as.mitml.result()} function takes the results of repeated
#' complete-data analysis stored as a list, and turns it
#' into an object of class \code{mitml.result}.
#' @param x An object of class \code{mira}
#' @return An S3 object of class \code{mitml.result}, a list
#' containing $m$ fitted analysis objects.
#' @seealso \code{\link[mitml]{with.mitml.list}}
#' @author Stef van Buuren
#' @export
as.mitml.result <- function(x) {
  if (inherits(x, "mitml.result")) {
    return(x)
  }
  z <- NULL
  if (is.mira(x)) {
    z <- getfit(x)
  } else if (is.list(x)) z <- x
  class(z) <- c("mitml.result", "list")
  z
}


setOldClass(c("mids", "mira"))
setAs(
  from = "data.frame", to = "mids",
  def = function(from) {
    as.mids(from)
  }
)

setAs(
  from = "list", to = "mira",
  def = function(from) {
    as.mira(from)
  }
)
