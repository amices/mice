#' Converts an imputed dataset (long format) into a \code{mids} object
#'
#' The \code{as.mids()} function converts imputed data stored in long
#' format into a \code{mids} object. The original incomplete dataset
#' needs to be available so that the function knows where the missing data are.
#' The function is useful to convert back operations applied to
#' the imputed data back in a \code{mids} object. It may also be
#' used to store multiply imputed data sets from other software
#' into the format used by \code{mice}.
#'
#' @details The function expects the input rows of \code{long} to be sorted by
#' the imputation index. By default, the imputation index is stored in
#' variable named \code{".imp"}), but you can change its name through the
#' \code{.imp} argument. Within each block, the function expects
#' the rows to be ordered in same sequence. If an \code{.id} variable is present,
#' the function will use it to resort the rows per imputation block before
#' creating the \code{mids} object.
#'
#' The column names \code{".imp"} and \code{".id"} are reserved for the
#' imputation index and row identifier, respectively. If these columns are
#' present in \code{long}, they will be used as such, but they are not
#' included in the \code{mids} object. If you wish to preserve their values,
#' gives them a different name and specify the new name in the \code{.imp}
#' and \code{.id} arguments.
#'
#' @param long A multiply imputed data set in long format, including the
#' original dataset with missing values. For example, we can create such a
#' dataset by a call to \code{complete(..., action = 'long', include = TRUE)}.
#' @param .imp A column number or column name in \code{long},
#' indicating the imputation index. The values are assumed to be consecutive
#' integers between 0 and \code{m}. Values \code{1} through \code{m}
#' correspond to the imputation index, value \code{0} indicates
#' the original data (with missing data). The default column name is
#' \code{".imp"}.
#' @param .id An optional column number or column name in \code{long},
#' containing a row identifier. The default column name is
#' \code{".id"}.
#' @param sort A logical flag indicating whether the data should be sorted
#'  by \code{.imp} and/or \code{.id} before creating the \code{mids} object.
#'  Set `sort = FALSE` to skip sorting.
#' @param warn A logical flag indicating whether a warning should be issued
#'   when the data is found to be unsorted and subsequently sorted. If set to
#'   `TRUE`, a warning is displayed whenever sorting is performed. If set to
#'   `FALSE`, no warning is issued. By default, the value is retrieved from
#'   the global option \code{"mice.sort.warn"} using
#'   \code{getOption("mice.sort.warn", TRUE)}. Users can globally enable or
#'   disable this warning by setting the option
#'   with \code{options(mice.sort.warn = TRUE)} or
#'   \code{options(mice.sort.warn = FALSE)}.
#' @inheritParams mice
#' @return An object of class \code{mids}
#' @author Gerko Vink, Stef van Buuren
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
#' # where argument copies also observed data into $imp element
#' where <- matrix(TRUE, nrow = nrow(nhanes), ncol = ncol(nhanes))
#' colnames(where) <- colnames(nhanes)
#' test11 <- as.mids(X, where = where)
#' identical(complete(test11, action = "long", include = TRUE), X)
#' @keywords mids
#' @export
as.mids <- function(long, where = NULL, .imp = ".imp", .id = ".id",
                    sort = TRUE, warn = getOption("mice.sort.warn", TRUE)) {
  # Convert position to column name
  if (is.numeric(.imp)) .imp <- names(long)[.imp]
  if (is.numeric(.id)) .id <- names(long)[.id]

  # Check for .imp column
  if (!.imp %in% names(long)) {
    stop("Imputation index `", .imp, "` not found")
  }

  # No missing values allowed in .imp
  imps <- if (is.data.table(long)) long[[.imp]] else extract.column(long, .imp)
  if (anyNA(imps)) stop("Missing values in imputation index `", .imp, "`")

  # Ensure equal group sizes
  if (any(diff(table(imps))) != 0L) {
    stop("Unequal group sizes in imputation index `", .imp, "`")
  }

  # Sort data if needed
  if (sort) {
    long <- sort_long_data(long, .imp, .id, warn)
  }

  # Extract original data
  keep <- setdiff(names(long), na.omit(c(.imp, .id)))
  if (is.data.table(long)) {
    data <- long[imps == 0L, keep, with = FALSE]
  } else {
    data <- long[imps == 0L, keep, drop = FALSE]
  }
  n <- nrow(data)
  if (n == 0L) {
    stop("Original data not found.\n",
         "Use `complete(..., action = 'long', include = TRUE)` ",
         "to save the original data.")
  }

  # Determine number of imputations
  m <- length(unique(imps)) - 1L

  # Generate initial `mids` object
  if (is.null(where)) where <- is.na(data)
  ini <- mice(data, m = m, where = where, maxit = 0L,
              remove.collinear = FALSE, allow.na = TRUE)

  # Populate imputations
  for (j in names(ini$imp)) {
    if (nrow(ini$imp[[j]])) {
      val <- lapply(seq_len(m), function(i) {
        idx <- imps == i & extract.column(where, j)
        long[[j]][idx]
      })
      set(ini$imp[[j]], j = as.character(seq_len(m)), value = as.data.table(val))
    }
  }

  # Add metadata for reproducibility
  ini$metadata <- list(
    original_imp_col = .imp,
    original_id_col = .id,
    sorted = sort
  )

  return(ini)
}

sort_long_data <- function(long, .imp, .id, warn) {
  nosort.imp.id <- .id %in% names(long) && !is.sorted(long, keys = c(.imp, .id))
  nosort.imp <- !is.sorted(long, keys = .imp)

  if (nosort.imp.id) {
    if (is.data.table(long)) {
      setkeyv(long, cols = c(.imp, .id))
    } else {
      long <- long[order(long[[.imp]], long[[.id]], na.last = FALSE), ]
    }
    if (warn) warning("Data not sorted by imputation index and .id. Sorting data.")
  } else if (nosort.imp) {
    if (is.data.table(long)) {
      setkeyv(long, cols = .imp)
    } else {
      long <- long[order(long[[.imp]]), ]
    }
    if (warn) warning("Data not sorted by imputation index. Sorting data.")
  }

  return(long)
}


#' Create a \code{mira} object from repeated analyses
#'
#' The \code{as.mira()} function takes the results of repeated
#' complete-data analysis stored as a list, and turns it
#' into a \code{mira} object that can be pooled.
#' @param fitlist A list containing $m$ fitted analysis objects
#' @return An S3 object of class \code{mira}.
#' @seealso \code{\link{mira}}
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
  object <- mira(call = call, analyses = fitlist)
  return(object)
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
