#' Creates a `nest` argument
#'
#' This helper function generates a character vector for the
#' `nest` argument in the [mice()] function.
#'
#' @param x A `data.frame`, an unnamed character vector, a named
#' character vector or a `list`.
#' @param partition Only relevant if `x` is a `data.frame`. Value
#' `"scatter"` (default) will assign each variable to a separate
#' nest. Value `"collect"` assigns all variables to one nest,
#' whereas `"void"` does not assign any variable to a nest.
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return A character vector of length `ncol(data)` that specifies
#' the nest name per variable
#'
#' @details Choices `"scatter"` and `"collect"` represent to two
#' extreme scenarios for assigning variables to imputation nests.
#' Use `"scatter"` to create an imputation model based on
#' *fully conditionally specification* (FCS). Use `"collect"` to
#' gather all variables to be imputed by a *joint model* (JM).
#'
#' Any variable not listed in the result will not be imputed.
#' Specification `"void"` represents the extreme scenario where
#' nothing is imputed.
#'
#' Unlike blocks, a variable cannot be allocated to multiple nests.
#' @examples
#'
#' # default nest creation (scatter)
#' make.nest(nhanes)
#'
#' # make nest from variable names
#' make.nest(c("age", "sex", "edu"))
#'
#' # put hgt, wgt and bmi into one nest, automatic naming
#' make.nest(list("age", "sex", c("hgt", "wgt", "bmi")))
#'
#' # same, but with custom nest names
#' make.nest(list("age", "sex", anthro = c("hgt", "wgt", "bmi")))
#'
#' # all variables into one nest
#' make.nest(nhanes, partition = "collect", prefix = "myblock")
#' @export
make.nest <- function(x,
                      partition = c("scatter", "collect", "void"),
                      prefix = "b") {

  # unnamed vector
  if (is.vector(x) && is.null(names(x)) && !is.list(x)) {
    nest <- as.character(x)
    names(nest) <- as.character(x)
    return(nest)
  }

  # named vector, preserve name order
  if (is.vector(x) && !is.null(names(x)) && !is.list(x)) {
    nest <- as.character(x)
    names(nest) <- names(x)
    return(nest)
  }

  # unnamed list
  if (is.list(x) && is.null(names(x)) && !is.data.frame(x)) {
    nest <- b2n(name.blocks(x, prefix = prefix))
    return(nest)
  }

  # named list
  if (is.list(x) && !is.null(names(x)) && !is.data.frame(x)) {
    nest <- b2n(x)
    return(nest)
  }

  x <- as.data.frame(x)
  partition <- match.arg(partition)
  switch(partition,
         scatter = {
           nest <- colnames(x)
           names(nest) <- names(x)
         },
         collect = {
           nest <- rep(prefix, ncol(x))
           names(nest) <- names(x)
         },
         void = {
           nest <- rep("", ncol(x))
           names(nest) <- names(x)
         },
         {
           nest <- names(x)
           names(nest) <- names(x)
         }
  )
  return(nest)
}

name.nest <- function(x) x

check.nest <- function(nest, data) {
  data <- check.dataform(data)
  nest <- name.nest(nest)

  # check that all variable names exists in data
  nv <- names(nest)
  notFound <- !nv %in% colnames(data)
  if (any(notFound)) {
    stop(paste(
      "The following names were not found in `data`:",
      paste(nv[notFound], collapse = ", ")
    ))
  }

  nest
}

#' Construct blocks from `formulas` and `predictorMatrix`
#'
#' This helper function attempts to find blocks of variables in the
#' specification of the `formulas` and/or `predictorMatrix`
#' objects. Blocks specified by `formulas` may consist of
#' multiple variables. Blocks specified by `predictorMatrix` are
#' assumed to consist of single variables. Any duplicates in names are
#' removed, and the formula specification is preferred.
#' `predictorMatrix` and `formulas`. When both arguments
#' specify models for the same block, the model for the
#' `predictMatrix` is removed, and priority is given to the
#' specification given in `formulas`.
#' @inheritParams mice
#' @return A `blocks` object.
#' @seealso [make.blocks()], [name.blocks()]
#' @examples
#' form <- list(bmi + hyp ~ chl + age, chl ~ bmi)
#' pred <- make.predictorMatrix(nhanes[, c("age", "chl")])
#' construct.blocks(formulas = form, pred = pred)
#' @export
construct.nest <- function(formulas = NULL, predictorMatrix = NULL) {
  blocks.f <- blocks.p <- NULL
  if (!is.null(formulas)) {
    if (!all(sapply(formulas, is.formula))) {
      return(NULL)
    }
    blocks.f <- name.blocks(lapply(name.formulas(formulas), lhs))
    ct <- rep("formula", length(blocks.f))
    names(ct) <- names(blocks.f)
    attr(blocks.f, "calltype") <- ct
    if (is.null(predictorMatrix)) {
      return(blocks.f)
    }
  }

  if (!is.null(predictorMatrix)) {
    if (is.null(row.names(predictorMatrix))) {
      stop("No row names in predictorMatrix", call. = FALSE)
    }
    blocks.p <- name.blocks(row.names(predictorMatrix))
    ct <- rep("pred", length(blocks.p))
    names(ct) <- names(blocks.p)
    attr(blocks.p, "calltype") <- ct
    if (is.null(formulas)) {
      return(blocks.p)
    }
  }

  # combine into unique blocks
  blocknames <- unique(c(names(blocks.f), names(blocks.p)))
  vars.f <- unlist(lapply(formulas, lhs))
  keep <- setdiff(blocknames, vars.f)
  add.p <- blocks.p[names(blocks.p) %in% keep]
  blocks <- c(blocks.f, add.p)
  ct <- c(
    rep("formula", length(formulas)),
    rep("pred", length(add.p))
  )
  names(ct) <- names(blocks)
  attr(blocks, "calltype") <- ct
  blocks
}


reorder.nest <- function(nest, data) {
  idx <- colnames(data)
  return(nest[idx])
}
