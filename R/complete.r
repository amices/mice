#'Extracts imputed data sets from a \code{mids} object
#'
#'Takes an object of class \code{mids}, fills in the missing data, and returns
#'the completed data in a specified format.
#'
#'@param data An object of class \code{mids} as created by the function
#'\code{mice()}.
#'@param action A numeric vector or a length-1 character vector. Numeric 
#'values between 1 and \code{data$m} return the data with 
#'imputation number \code{action} filled in. The value of \code{action = 0} 
#'return the original data, with missing values. \code{action} can
#'also be one of the following keywords: \code{"all"}, \code{"long"}, 
#'\code{"broad"} and \code{"repeated"}. See 'Details' for the interpretation.
#'The default (\code{action = "all"}) returns an object of class \code{milc}.
#'a list of \code{m} completed data sets.
#'@param include A logical to indicate whether the orginal data with the missing
#'values should be included.
#'@param milc A logical indicating whether the return value should 
#'always be an object of class \code{milc}. Setting \code{milc = TRUE} 
#'overrides \code{action} keywords \code{"long"}, \code{"broad"} 
#'and \code{"repeated"}. The default is \code{FALSE}.
#'@param \dots Additional arguments. Not used.
#'@return Either a \code{data.frame} with the imputed values filled in,
#'or a list of data frames with the imputed values filled in.
#'@details
#'The argument \code{action} can be a string, which is partially matched
#'as follows: 
#'\describe{ 
#'\item{\code{"all"}}{produces a \code{milc} object of imputed data sets. When 
#'\code{include = TRUE}, then the original data are appended as the first list 
#'element.}
#'\item{\code{"long"}}{ produces a long data frame of
#'vertically stacked imputed data sets with \code{nrow(data$data)} * \code{data$m}
#'rows and \code{ncol(data$data)+2} columns. The two additional columns are
#'labeled \code{.id} containing the row names of \code{data$data}, and \code{.imp}
#'containing the imputation number.  If \code{include=TRUE} then
#'\code{nrow(data$data)} additional rows with the original data are appended with
#'\code{.imp} set equal to \code{0}.}
#'\item{\code{"broad"}}{ produces a broad data frame with
#'\code{nrow(data$data)} rows and \code{ncol(data$data)} * \code{data$m} columns.
#'Columns are ordered such that the first \code{ncol(data$data)} columns
#'corresponds to the first imputed data frame. The imputation number is
#'appended to each column name.  If \code{include=TRUE} then
#'\code{ncol(x$data)} additional columns with the original data are appended.
#'The number \code{.0} is appended to the column names.  }
#'\item{\code{"repeated"}}{ produces a broad data frame with
#'\code{nrow(x$data)} rows and \code{ncol(data$data)} * \code{data$m} columns.
#'Columns are ordered such that the first \code{data$m} columns correspond to the
#'\code{data$m} imputed versions of the first column in \code{data$data}. The
#'imputation number is appended to each column name.  If \code{include=TRUE}
#'then \code{ncol(data$data)} additional columns with the original data are
#'appended.  The number \code{.0} is appended to the column names} 
#'}
#'@seealso \code{\link{mice}}, \code{\link[=mids-class]{mids}}
#'@keywords manip
#'@examples
#'
#'
#'# do default multiple imputation on a numeric matrix
#'imp <- mice(nhanes)
#'
#'# obtain first imputated matrix
#'mat <- complete(imp)
#'
#'# fill in the third imputation
#'mat <- complete(imp, 3)
#'
#'# long matrix with stacked complete data
#'mat <- complete(imp, 'long')
#'
#'# long matrix with stacked complete data, including the original data
#'mat <- complete(imp, 'long', inc=TRUE)
#'
#'# repeated matrix with complete data
#'mat <- complete(imp, 'r')
#'
#'# for numeric data, produces a blocked correlation matrix, where
#'# each block contains of the same variable pair over different
#'# multiple imputations.
#'cor(mat)
#'@export
complete.mids <- function(data, action = 1L, include = FALSE, 
                          milc = FALSE, ...) {
  if (!is.mids(data)) stop("'data' not of class 'mids'")

  m <- as.integer(data$m)
  if (is.numeric(action)) {
    idx <- action[action >= 0L & action <= m]
    if (include && all(idx != 0L)) idx <- c(0L, idx) 
    shape <- ifelse(milc, "milc", "stacked")
  }
  else if (is.character(action)) {
    if (include) idx <- 0L:m else idx <- 1L:m
    shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
    shape <- ifelse(shape == "all" || milc, "milc", shape)
  }
  else stop("'action' not recognized")
  
  mylist <- vector("list", length = length(idx))
  for (j in seq_along(idx))
    mylist[[j]] <- complete.one(data$data, data$where, data$imp, idx[j])
  
  if (shape == "stacked")
    return(bind_rows(mylist))
  if (shape == "milc") {
    class(mylist) <- c("milc", "list")
    return(mylist)
  }
  if (shape == "long") {
    cmp <- bind_rows(mylist)
    cmp <- data.frame(.imp = as.factor(rep(idx, each = nrow(data$data))), 
                      .id = rep.int(row.names(data$data), length(idx)), 
                      cmp)
    row.names(cmp) <- seq_len(nrow(cmp))
    return(cmp)
  }
  # must be broad or repeated
  cmp <- bind_cols(mylist)
  names(cmp) <- paste(rep.int(names(data$data), m), 
                           rep.int(idx, rep.int(ncol(data$data), length(idx))), 
                           sep = ".")
  if (shape == "broad") return(cmp)
  else return(cmp[, order(rep.int(seq_len(ncol(data$data)), length(idx)))])
}


complete.one <- function(data, where, imp, ell) {
  if (ell == 0L) return(data)
  if (is.null(where))
    where <- is.na(data)
  ind <- seq_len(ncol(data))[colSums(where) > 0L]
  for (j in ind) {
    if (is.null(imp[[j]])) data[where[, j], j] <- NA
    else data[where[, j], j] <- imp[[j]][, ell]
  }
  data
}
