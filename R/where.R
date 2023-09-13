#' Creates a `where` argument
#'
#' This helper function creates a valid `where` matrix. The
#' `where` matrix is an argument to the `mice` function.
#' It has the same size as `data` and specifies which values
#' are to be imputed (`TRUE`) or nor (`FALSE`).
#' @param data A `data.frame` with the source data
#' @param keyword An optional keyword, one of `"missing"` (missing
#' values are imputed), `"observed"` (observed values are imputed),
#' `"all"` and `"none"`. The default
#' is `keyword = "missing"`
#' @return A matrix with logical
#' @seealso [make.blocks()], [make.predictorMatrix()]
#' @examples
#' head(make.where(nhanes), 3)
#'
#' # create & analyse synthetic data
#' where <- make.where(nhanes2, "all")
#' imp <- mice(nhanes2,
#'   m = 10, where = where,
#'   print = FALSE, seed = 123
#' )
#' fit <- with(imp, lm(chl ~ bmi + age + hyp))
#' summary(pool.syn(fit))
#' @export
make.where <- function(data,
                       keyword = c("missing", "all", "none", "observed")) {
  keyword <- match.arg(keyword)

  data <- check.dataform(data)
  where <- switch(keyword,
    missing = is.na(data),
    all = matrix(TRUE, nrow = nrow(data), ncol = ncol(data)),
    none = matrix(FALSE, nrow = nrow(data), ncol = ncol(data)),
    observed = !is.na(data)
  )

  dimnames(where) <- dimnames(data)
  where
}


check.where <- function(where, data, blocks) {
  if (is.null(where)) {
    where <- make.where(data, keyword = "missing")
  }

  if (!(is.matrix(where) || is.data.frame(where))) {
    if (is.character(where)) {
      return(make.where(data, keyword = where))
    } else {
      stop("Argument `where` not a matrix or data frame", call. = FALSE)
    }
  }
  if (!all(dim(data) == dim(where))) {
    stop("Arguments `data` and `where` not of same size", call. = FALSE)
  }

  where <- as.logical(as.matrix(where))
  if (anyNA(where)) {
    stop("Argument `where` contains missing values", call. = FALSE)
  }

  where <- matrix(where, nrow = nrow(data), ncol = ncol(data))
  dimnames(where) <- dimnames(data)
  # #583
  # where[, !colnames(where) %in% unlist(blocks)] <- FALSE
  where[, !colnames(where) %in% unlist(blocks)] <- FALSE
  where
}
