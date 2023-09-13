#' Extracts the completed data from a `mids` object
#'
#' Takes an object of class `mids`, fills in the missing data, and returns
#' the completed data in a specified format.
#'
#' @aliases complete
#' @param data An object of class `mids` as created by the function
#' `mice()`.
#' @param action A numeric vector or a keyword. Numeric
#' values between 1 and `data$m` return the data with
#' imputation number `action` filled in. The value of `action = 0`
#' return the original data, with missing values. `action` can
#' also be one of the following keywords: `"all"`, `"long"`,
#' `"broad"` and `"repeated"`. See the Details section
#' for the interpretation.
#' The default is `action = 1L` returns the first imputed data set.
#' @param include A logical to indicate whether the original data with the missing
#' values should be included.
#' @param mild A logical indicating whether the return value should
#' always be an object of class `mild`. Setting `mild = TRUE`
#' overrides `action` keywords `"long"`, `"broad"`
#' and `"repeated"`. The default is `FALSE`.
#' @param order Either `"first"` or `"last"`. Only relevant when
#' `action == "long"`. Writes the `".imp"` and `".id"`
#' in columns 1 and 2. The default is `order = "last"`.
#' Included for backward compatibility with `"< mice 3.16.0"`.
#' @param \dots Additional arguments. Not used.
#' @return Complete data set with missing values replaced by imputations.
#' A `data.frame`, or a list of data frames of class `mild`.
#' @details
#' The argument `action` can be length-1 character, which is
#' matched to one of the following keywords:
#' \describe{
#' \item{`"all"`}{produces a `mild` object of imputed data sets. When
#' `include = TRUE`, then the original data are appended as the first list
#' element;}
#' \item{`"long"`}{ produces a data set where imputed data sets
#' are stacked vertically. The columns are added: 1) `.imp`, integer,
#' referring the imputation number, and 2) `.id`, character, the row
#' names of `data$data`;}
#' \item{`"stacked"`}{ same as `"long"` but without the two
#' additional columns;}
#' \item{`"broad"`}{ produces a data set with where imputed data sets
#' are stacked horizontally. Columns are ordered as in the original data.
#' The imputation number is appended to each column name;}
#' \item{`"repeated"`}{ same as `"broad"`, but with
#' columns in a different order.}
#' }
#' @note
#' Technical note: `mice 3.7.5` renamed the `complete()` function
#' to `complete.mids()` and exported it as an S3 method of the
#' generic `tidyr::complete()`. Name clashes between
#' `mice::complete()` and `tidyr::complete()` should no
#' longer occur.
#' @seealso [mice()], [`mids()`][mids-class]
#' @keywords manip
#' @examples
#'
#' # obtain first imputed data set
#' sum(is.na(nhanes2))
#' imp <- mice(nhanes2, print = FALSE, maxit = 1)
#' dat <- complete(imp)
#' sum(is.na(dat))
#'
#' # obtain stacked third and fifth imputation
#' dat <- complete(imp, c(3, 5))
#'
#' # obtain all datasets, with additional identifiers
#' head(complete(imp, "long"))
#'
#' # same, but now as list, mild object
#' dslist <- complete(imp, "all")
#' length(dslist)
#'
#' # same, but also include the original data
#' dslist <- complete(imp, "all", include = TRUE)
#' length(dslist)
#'
#' # select original + 3 + 5, store as mild
#' dslist <- complete(imp, c(0, 3, 5), mild = TRUE)
#' names(dslist)
#' @export
complete.mids <- function(data, action = 1L, include = FALSE,
                          mild = FALSE, order = c("last", "first"),
                          ...) {
  if (!is.mids(data)) stop("'data' not of class 'mids'")
  order <- match.arg(order)

  m <- as.integer(data$m)
  if (is.numeric(action)) {
    action <- as.integer(action)
    idx <- action[action >= 0L & action <= m]
    if (include && all(idx != 0L)) idx <- c(0L, idx)
    shape <- ifelse(mild, "mild", "stacked")
  } else if (is.character(action)) {
    if (include) idx <- 0L:m else idx <- 1L:m
    shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
    shape <- ifelse(shape == "all" || mild, "mild", shape)
  } else {
    stop("'action' not recognized")
  }

  mylist <- vector("list", length = length(idx))
  for (j in seq_along(idx)) {
    mylist[[j]] <- single.complete(data$data, data$where, data$imp, idx[j])
  }

  if (shape == "stacked") {
    return(bind_rows(mylist))
  }
  if (shape == "mild") {
    names(mylist) <- as.character(idx)
    class(mylist) <- c("mild", "list")
    return(mylist)
  }
  if (shape == "long") {
    cmp <- mylist %>%
      bind_rows() %>%
      mutate(
        .imp = rep(idx, each = nrow(data$data)),
        .id = rep.int(attr(data$data, "row.names"), length(idx)),
      )
    if (order == "first") {
      cmp <- relocate(cmp, any_of(c(".imp", ".id")))
    }
    if (typeof(attr(data$data, "row.names")) == "integer") {
      row.names(cmp) <- seq_len(nrow(cmp))
    } else {
      row.names(cmp) <- as.character(seq_len(nrow(cmp)))
    }
    return(cmp)
  }
  # must be broad or repeated
  cmp <- bind_cols(mylist)
  names(cmp) <- paste(rep.int(names(data$data), m),
                      rep.int(idx, rep.int(ncol(data$data), length(idx))),
                      sep = "."
  )
  if (shape == "broad") {
    return(cmp)
  } else {
    return(cmp[, order(rep.int(seq_len(ncol(data$data)), length(idx)))])
  }
}

single.complete <- function(data, where, imp, ell) {
  if (ell == 0L) {
    return(data)
  }
  if (is.null(where)) {
    where <- is.na(data)
  }
  idx <- seq_len(ncol(data))[apply(where, 2, any)]
  for (j in idx) {
    if (is.null(imp[[j]])) {
      data[where[, j], j] <- NA
    } else {
      if (sum(where[, j]) == nrow(imp[[j]])) {
        # assume equal length
        data[where[, j], j] <- imp[[j]][, ell]
      } else {
        # index by rowname
        data[as.numeric(rownames(imp[[j]])), j] <- imp[[j]][, ell]
      }
    }
  }
  data
}
