#' Creates a \code{method} argument
#'
#' This helper function creates a valid \code{method} vector. The 
#' \code{method} vector is an argument to the \code{mice} function that 
#' specifies the method for each block.
#' @inheritParams mice
#' @return Vector of \code{length(blocks)} element with method names
#' @seealso \code{\link{mice}}
#' @examples
#' make.method(nhanes2)
#' @export
make.method <- function(data, where = make.where(data), blocks = make.blocks(data),
                        defaultMethod = c("pmm", "logreg", "polyreg", "polr")) {
  
  assign.method <- function(y) {
    if (is.numeric(y)) return(1)
    if (nlevels(y) == 2) return(2)
    if (is.ordered(y) && nlevels(y) > 2) return(4)
    if (nlevels(y) > 2) return(3)
    if (is.logical(y)) return(2)
    return(1)
  }
  
  # assign methods based on type, 
  # use method 1 if there is no single method within the block
  method <- rep("", length(blocks))
  names(method) <- names(blocks)
  for (j in names(blocks)) {
    yvar <- blocks[[j]]
    y <- data[, yvar]
    def <- sapply(y, assign.method)
    k <- ifelse(all(diff(def) == 0), k <- def[1], 1)
    method[j] <- defaultMethod[k]
  }
  nimp <- nimp(where, blocks)
  method[nimp == 0] <- ""
  method
}

check.method <- function(method, data, where, blocks, defaultMethod) {

  if (is.null(method)) return(make.method(data = data, 
                                          where = where,
                                          blocks = blocks,
                                          defaultMethod = defaultMethod))
  nimp <- nimp(where, blocks)

  # expand user's imputation method to all visited columns
  # single string supplied by user (implicit assumption of two columns)
  if (length(method) == 1) {
    if (is.passive(method))
      stop("Cannot have a passive imputation method for every column.")
    method <- rep(method, length(blocks))
    method[nimp == 0] <- ""
  }
  
  # check the length of the argument
  if (length(method) != length(blocks))
    stop("Length of method differs from number of blocks", call. = FALSE)
  
  # add names to method
  names(method) <- names(blocks)
  
  # check whether the requested imputation methods are on the search path
  active.check <- !is.passive(method) & nimp > 0 & method != ""
  passive.check <- is.passive(method) & nimp > 0 & method != ""
  check <- all(active.check) & any(passive.check)
  if (check) {
    fullNames <- rep.int("mice.impute.passive", length(method[passive.check]))
  } else {
    fullNames <- paste("mice.impute", method[active.check], sep = ".")
    if (length(method[active.check]) == 0) fullNames <- character(0)
  }
  notFound <- !vapply(fullNames, exists, logical(1), 
                      mode = "function", inherits = TRUE)
  if (any(notFound)) {
    stop(paste("The following functions were not found:",
               paste(fullNames[notFound], collapse = ", ")))
  }
  
  # type checks on built-in imputation methods
  for (j in names(blocks)) {
    vname <- blocks[[j]]
    y <- data[, vname, drop = FALSE]
    mj <- method[j]
    mlist <- list(m1 = c("logreg", "logreg.boot", "polyreg", "lda", "polr"), 
                  m2 = c("norm", "norm.nob", "norm.predict", "norm.boot",
                         "mean", "2l.norm", "2l.pan",
                         "2lonly.pan", "quadratic", "ri"), 
                  m3 = c("norm", "norm.nob", "norm.predict", "norm.boot",
                         "mean", "2l.norm", "2l.pan", 
                         "2lonly.pan", "quadratic", "logreg", "logreg.boot"))
    cond1 <- sapply(y, is.numeric)
    cond2 <- sapply(y, is.factor) & sapply(y, nlevels) == 2 
    cond3 <- sapply(y, is.factor) & sapply(y, nlevels) > 2
    if (any(cond1) && mj %in% mlist$m1)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond1], collapse = ", "),
              "\nImputation method ", mj, " is for categorical data.",
              call. = FALSE)
    if (any(cond2) && mj %in% mlist$m2)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond2], collapse = ", "),
              "\nImputation method ", mj, " is not for factors.", 
              call. = FALSE)
    if (any(cond3) && mj %in% mlist$m3)
      warning("Type mismatch for variable(s): ", 
              paste(vname[cond3], collapse = ", "),
              "\nImputation method ", mj, " is not for factors with >2 levels.",
              call. = FALSE)
  }
  method[nimp == 0] <- ""
  unlist(method)
}
