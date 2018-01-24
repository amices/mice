#'Extracts imputed data sets from a \code{mids} object
#'
#'Takes an object of class \code{mids}, fills in the missing data, and returns
#'the completed data in a specified format.
#'
#'@param x An object of class \code{mids} as created by the function
#'\code{mice()}.
#'@param action If action is a scalar between 1 and \code{x$m}, the function
#'returns the data with imputation number \code{action} filled in. Thus,
#'\code{action=1} returns the first completed data set, \code{action=2} returns
#'the second completed data set, and so on.  The value of \code{action} can
#'also be one of the following strings: \code{'long'}, \code{'broad'},
#'\code{'repeated'}.  See 'Details' for the interpretation.
#'@param include Flag to indicate whether the orginal data with the missing
#'values should be included. This is only relevant only if \code{action} is 
#'specified as \code{"long"}, \code{"broad"} or \code{"repeated"}.
#'@return A data frame with the imputed values filled in. Optionally, the
#'original data are appended.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn
#'@details
#'The argument \code{action} can be a string, which is partially matched
#'as follows: 
#'\describe{ 
#'\item{\code{"long"}}{ produces a long data frame of
#'vertically stacked imputed data sets with \code{nrow(x$data)} * \code{x$m}
#'rows and \code{ncol(x$data)+2} columns. The two additional columns are
#'labeled \code{.id} containing the row names of \code{x$data}, and \code{.imp}
#'containing the imputation number.  If \code{include=TRUE} then
#'\code{nrow(x$data)} additional rows with the original data are appended with
#'\code{.imp} set equal to \code{0}.}
#'\item{\code{"broad"}}{ produces a broad data frame with
#'\code{nrow(x$data)} rows and \code{ncol(x$data)} * \code{x$m} columns.
#'Columns are ordered such that the first \code{ncol(x$data)} columns
#'corresponds to the first imputed data matrix. The imputation number is
#'appended to each column name.  If \code{include=TRUE} then
#'\code{ncol(x$data)} additional columns with the original data are appended.
#'The number \code{.0} is appended to the column names.  }
#'\item{\code{"repeated"}}{ produces a broad data frame with
#'\code{nrow(x$data)} rows and \code{ncol(x$data)} * \code{x$m} columns.
#'Columns are ordered such that the first \code{x$m} columns correspond to the
#'\code{x$m} imputed versions of the first column in \code{x$data}. The
#'imputation number is appended to each column name.  If \code{include=TRUE}
#'then \code{ncol(x$data)} additional columns with the original data are
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
#'
#'@export
complete <- function(x, action = 1, include = FALSE) {
  if (!is.mids(x)) 
    stop("Input data must have class 'mids'.")
  if (!is.logical(include)) 
    stop("Argument 'include' should be either TRUE or FALSE.")
  if (is.numeric(action) && action == 0) 
    return(x$data)
  if (is.numeric(action) && action >= 1 && action <= x$m) {
    data <- x$data
    where <- x$where
    if (is.null(where))
      where <- is.na(data)
    ind <- seq_len(ncol(data))[colSums(where) > 0]
    for (j in ind) {
      if (is.null(x$imp[[j]]))
        data[where[, j], j] <- NA
      else data[where[, j], j] <- x$imp[[j]][, action]
    }
    return(data)
  }
  code <- pmatch(action, c("long", "broad", "repeated"))
  if (!is.na(code) && code >= 1 && code <= 3) {
    m <- x$m
    nr <- nrow(x$data)
    nc <- ncol(x$data)
    add <- as.numeric(include)
    mylist <- vector("list", length = m + add)
    # recursive call
    for (j in seq_along(mylist)) mylist[[j]] <- Recall(x, j - add)
    if (code == 1) {
      # long
      data <- do.call(rbind, mylist)
      data <- data.frame(.imp = as.factor(rep((1 - add):m, each = nr)), 
                         .id = rep.int(row.names(x$data), m + add), 
                         data)
      row.names(data) <- seq_len(nrow(data))
    }
    if (code >= 2 && code <= 3) {
      # broad or repeated
      data <- do.call(cbind, mylist)
      names(data) <- paste(rep.int(names(x$data), m), 
                           rep.int((1 - add):m, rep.int(nc, m + add)), 
                           sep = ".")
    }
    if (code == 3) {
      # repeated
      data <- data[, order(rep.int(seq_len(nc), m + add))]
    }
    return(data)
  }
  stop("Argument action not recognized. \n")
}
