#' Convert predictorMatrix to formalas
#'
#' @rdname convertmodels
#' @param silent Logical for additional diagnostics
#' @inheritParams mice
#' @export
p2f <- function(predictorMatrix, blocks = NULL, silent = TRUE) {
  # converts predictorMatrix to formulas
  valid <- validate.predictorMatrix(predictorMatrix, silent = silent)
  if (!valid) {
    stop("Malformed predictorMatrix")
  }

  vars <- colnames(predictorMatrix)
  if (is.null(blocks)) {
    blocks <- make.blocks(vars, partition = "scatter")
  }
  formulas <- vector("list", length = length(blocks))
  names(formulas) <- names(blocks)
  for (b in names(blocks)) {
    ynames <- blocks[[b]]
    yname <- ynames[[1L]]
    pred <- predictorMatrix[yname, ]
    xnames <- setdiff(vars[pred != 0], ynames)
    if (length(xnames) > 0L) {
      yn <- paste(ynames, collapse = "+")
      formula <- reformulate(xnames, response = str2lang(yn))
    } else {
      formula <- as.formula(paste0(paste(ynames, collapse = "+"), " ~ 1"))
    }
    formulas[[b]] <- formula
  }
  return(formulas)
}

#' Convert predictorMatrix into roles
#'
#' @rdname convertmodels
p2c <- function(predictorMatrix) {
  # exports special predictorMatrix roles, not 0 or 1
  blks <- row.names(predictorMatrix)
  vars <- colnames(predictorMatrix)
  roles <- vector("list", length = length(blks))
  names(roles) <- blks
  for (b in blks) {
    pred <- predictorMatrix[b, ]
    if (!all(pred %in% c(0, 1))) {
      xnames <- setdiff(vars[pred != 0], b)
      roles[[b]] <- predictorMatrix[b, xnames]
    }
  }
  return(roles)
}

#' Convert formulas into predictorMatrix
#'
#' @rdname convertmodels
#' @param roles A list with `ncol(data)` elements, each with a row of the
#' `predictorMatrix` when it contains values other than 0 or 1.
#' The argument is only needed if the model contains non-standard
#'values in the `predictorMatrix`.
#' @export
f2p <- function(formulas, blocks = NULL, roles = NULL) {
  # converts formulas and roles into predictorMatrix
  blks <- names(formulas)
  vars <- unique(as.vector(unlist(sapply(formulas, all.vars))))
  predictorMatrix <- matrix(0, nrow = length(vars), ncol = length(vars))
  dimnames(predictorMatrix) <- list(vars, vars)
  for (b in blks) {
    f <- formulas[[b]]
    fv <- all.vars(f)
    ynames <- lhs(f)
    for (yname in ynames) {
      xn <- setdiff(fv, yname)
      # xn <- union(setdiff(ynames, yname), xnames)
      if (is.null(roles[[yname]])) {
        # code all variables in same block as 1
        predictorMatrix[yname, xn] <- 1
      } else {
        # use external special roles
        codeb <- roles[[yname]][xn]
        predictorMatrix[yname, xn] <- codeb
      }
    }
  }
  valid <- validate.predictorMatrix(predictorMatrix)
  if (!valid) {
    warning("Malformed predictorMatrix. See ?make.predictorMatrix")
  }
  return(predictorMatrix)
}

n2b <- function(nest, silent = FALSE) {
  # nest to block
  stopifnot(validate.nest(nest, silent = silent))
  if (all(nest == "")) {
    nest[1L:length(nest)] <- names(nest)
  }
  if (any(nest == "")) {
    stop("Cannot convert a partially named nest to blocks")
  }
  nf <- factor(nest, levels = unique(nest))
  blocknames <- levels(nf)
  blocks <- vector("list", length = length(blocknames))
  names(blocks) <- blocknames
  for (b in names(blocks)) {
    blocks[[b]] <- names(nest)[nest == b]
  }
  return(blocks)
}

b2n <- function(blocks, silent = FALSE) {
  # block to nest
  stopifnot(validate.blocks(blocks, silent = silent))
  vars <- unlist(blocks)
  nest <- rep(names(blocks), sapply(blocks, length))
  if (any(duplicated(vars))) {
    warning("Duplicated name(s) removed: ",
            paste(vars[duplicated(vars)], collapse = ", "))
  }
  names(nest) <- vars
  nest <- nest[!duplicated(names(nest))]
  stopifnot(validate.nest(nest))
  return(nest)
}

paste.roles <- function(blots, roles, blocks) {
  # FIXME
  # flat <- unlist(unname(roles))
  # flat[unique(names(flat))]
  return(blots)
}

validate.nest <- function(nest, silent = FALSE) {
  if (!is.vector(nest)) {
    if (!silent) warning("nest is not a vector", call. = FALSE)
    return(FALSE)
  }
  if (!is.character(nest)) {
    if (!silent) warning("nest is not of type character", call. = FALSE)
    return(FALSE)
  }
  if (!length(nest)) {
    if (!silent) warning("nest has length zero", call. = FALSE)
    return(FALSE)
  }
  if (is.null(names(nest))) {
    if (!silent) warning("nest has no names", call. = FALSE)
    return(FALSE)
  }
  if (any(duplicated(names(nest)))) {
    if (!silent) warning(
      "duplicated names in nest: ",
      paste({names(nest)}[duplicated(names(nest))], collapse = ", "),
      call. = FALSE)
    return(FALSE)
  }

  return(TRUE)
}

validate.blocks <- function(blocks, silent = FALSE) {
  if (!is.list(blocks)) {
    if (!silent) warning("blocks is not a list", call. = FALSE)
    return(FALSE)
  }
  if (!length(blocks)) {
    if (!silent) warning("blocks has length zero", call. = FALSE)
    return(FALSE)
  }
  if (is.null(names(blocks))) {
    if (!silent) warning("blocks has no names", call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}

