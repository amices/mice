#' Creates a `method` argument
#'
#' This helper function creates a valid `method` vector. The
#' `method` vector is an argument to the `mice` function that
#' specifies the method for each block.
#' @param ynames vector of names of variables to be imputed
#' @inheritParams mice
#' @return Vector of `length(blocks)` element with method names
#' @seealso [mice()]
#' @examples
#' make.method(nhanes2)
#' @export
make.method <- function(data,
                        where = make.where(data),
                        blocks = make.blocks(data),
                        defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                        ynames = NULL) {
  # support tiny predictorMatrix, blocks and formulas
  if (is.null(ynames)) {
    ynames <- colnames(data)
  }
  # FIXME colnames(data) may be too large if user specifies blocks argument
  #       to make.method()

  # if (!is.null(user.predictorMatrix)) {
  #   if (!is.null(dimnames(user.predictorMatrix))) {
  #     include <- colnames(user.predictorMatrix)
  #   } else {
  #     include1 <- colnames(data)
  #   }
  # }
  # # support tiny blocks
  # if (!is.null(user.blocks)) {
  #   include2 <- unique(as.vector(unname(unlist(user.blocks))))
  # }
  #
  # support tiny formulas
  # if (!is.null(user.formulas)) {
  #   include <- unique(as.vector(sapply(user.formulas, all.vars)))
  # }
  # support tiny formulas
  # if (!is.null(formulas)) {
  #   include3 <- attr(formulas, "ynames")
  # }

  method <- rep("", length(blocks))
  names(method) <- names(blocks)
  for (j in seq_along(blocks)) {
    yvar <- blocks[[j]]
    y <- data[, yvar, drop = FALSE]
    k <- assign.method(y)
    if (all(yvar %in% ynames)) {
      method[j] <- defaultMethod[k]
    }
  }

  # FIXME do we really need this here?
  nimp <- nimp(where = where, blocks = blocks)
  method[nimp == 0L] <- ""
  method
}


check.method <- function(method, data, where, blocks, defaultMethod,
                         ynames) {
  if (is.null(method)) {
    method <- make.method(
      data = data,
      where = where,
      blocks = blocks,
      defaultMethod = defaultMethod,
      ynames = ynames)
    return(method)
  }
  nimp <- nimp(where = where, blocks = blocks)

  # expand user's imputation method to all visited columns
  # single string supplied by user (implicit assumption of two columns)
  if (length(method) == 1L) {
    if (is.passive(method)) {
      stop("Cannot have a passive imputation method for every column.")
    }
    method <- rep(method, length(blocks))
    method[nimp == 0L] <- ""
  }

  # check the length of the argument
  if (length(method) != length(blocks)) {
    stop("Length of method differs from number of blocks", call. = FALSE)
  }

  # add names to method
  names(method) <- names(blocks)

  # check whether the requested imputation methods are on the search path
  active.check <- !is.passive(method) & nimp > 0L & method != ""
  passive.check <- is.passive(method) & nimp > 0L & method != ""
  check <- all(active.check) & any(passive.check)
  if (check) {
    fullNames <- rep.int("mice.impute.passive", length(method[passive.check]))
  } else {
    fullNames <- paste("mice.impute", method[active.check], sep = ".")
    if (length(method[active.check]) == 0L) fullNames <- character(0)
  }

  # type checks on built-in imputation methods
  for (j in names(blocks)) {
    vname <- blocks[[j]]
    y <- data[, vname, drop = FALSE]
    mj <- method[j]
    mlist <- list(
      m1 = c("logreg", "logreg.boot", "polyreg", "lda", "polr"),
      m2 = c(
        "norm", "norm.nob", "norm.predict", "norm.boot",
        "mean", "2l.norm", "2l.pan",
        "2lonly.norm", "2lonly.pan",
        "quadratic", "ri"
      ),
      m3 = c(
        "norm", "norm.nob", "norm.predict", "norm.boot",
        "mean", "2l.norm", "2l.pan",
        "2lonly.norm", "2lonly.pan",
        "quadratic", "logreg", "logreg.boot"
      )
    )
    cond1 <- sapply(y, is.numeric)
    cond2 <- sapply(y, is.factor) & sapply(y, nlevels) == 2L
    cond3 <- sapply(y, is.factor) & sapply(y, nlevels) > 2L
    if (any(cond1) && mj %in% mlist$m1) {
      warning("Type mismatch for variable(s): ",
              paste(vname[cond1], collapse = ", "),
              "\nImputation method ", mj, " is for categorical data.",
              call. = FALSE
      )
    }
    if (any(cond2) && mj %in% mlist$m2) {
      warning("Type mismatch for variable(s): ",
              paste(vname[cond2], collapse = ", "),
              "\nImputation method ", mj, " is not for factors.",
              call. = FALSE
      )
    }
    if (any(cond3) && mj %in% mlist$m3) {
      warning("Type mismatch for variable(s): ",
              paste(vname[cond3], collapse = ", "),
              "\nImputation method ", mj, " is not for factors with >2 levels.",
              call. = FALSE
      )
    }
  }
  method[nimp == 0L] <- ""
  unlist(method)
}


# assign methods based on type,
# use method 1 if block is of heterogeneous type
assign.method <- function(y) {
  if (all(sapply(y, is.numeric))) {
    return(1L)
  }
  if (all(sapply(y, is.factor)) && all(sapply(y, nlevels) == 2L)) {
    return(2L)
  }
  if (all(sapply(y, is.ordered)) && all(sapply(y, nlevels) > 2L)) {
    return(4L)
  }
  if (all(sapply(y, nlevels) > 2L)) {
    return(3L)
  }
  if (all(sapply(y, is.logical))) {
    return(2L)
  }
  return(1L)
}
