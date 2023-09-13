#' Creates a `blocks` argument
#'
#' This helper function generates a list of the type needed for
#' `blocks` argument in the [mice()] function.
#' @param x A `data.frame`, character vector with
#' variable names, or `list` with variable names.
#' @param partition Only relevant when `x` is a `data.frame`. Value
#' `"scatter"` (default) will assign each column to a separate
#' block. Value `"collect"` assigns all variables to one block,
#' whereas `"void"` produces an empty list.
#' @param calltype A character vector of `length(block)` elements
#' that indicates how the imputation model is specified. If
#' `calltype = "pred"` (the default), the underlying imputation
#' model is called by means of the `type` argument. The
#' `type` argument for block `h` is equivalent to
#' row `h` in the `predictorMatrix`.
#' The alternative is `calltype = "formula"`. This will pass
#' `formulas[[h]]` to the underlying imputation
#' function for block `h`, together with the current data.
#' The `calltype` of a block is set automatically during
#' initialization. Where a choice is possible, calltype
#' `"formula"` is preferred over `"pred"` since this is
#' more flexible and extendable. However, what precisely happens
#' depends also on the capabilities of the imputation
#' function that is called.
#' @return A named list of character vectors with variables names.
#' @details Choices `"scatter"` and `"collect"` represent to two
#' extreme scenarios for assigning variables to imputation blocks.
#' Use `"scatter"` to create an imputation model based on
#' *fully conditionally specification* (FCS). Use `"collect"` to
#' gather all variables to be imputed by a *joint model* (JM).
#' Scenario's in-between these two extremes represent
#' *hybrid* imputation models that combine FCS and JM.
#'
#' Any variable not listed in will not be imputed.
#' Specification `"void"` represents the extreme scenario that
#' skips imputation of all variables.
#'
#' A variable may be a member of multiple blocks. The variable will be
#' re-imputed in each block, so the final imputations for variable
#' will come from the last block that was executed. This scenario
#' may be useful where the same complete background factors appear in
#' multiple imputation blocks.
#'
#' A variable may appear multiple times within a given block. If a univariate
#' imputation model is applied to such a block, then the variable is
#' re-imputed each time as it appears in the block.
#' @examples
#' make.blocks(nhanes)
#' make.blocks(c("age", "sex", "edu"))
#' @export
make.blocks <- function(x,
                        partition = c("scatter", "collect", "void"),
                        calltype = "pred") {
  if (is.vector(x) && !is.list(x)) {
    v <- as.list(as.character(x))
    names(v) <- as.character(x)
    ct <- rep(calltype, length(v))
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
    return(v)
  }
  if (is.list(x) && !is.data.frame(x)) {
    v <- name.blocks(x)
    if (length(calltype) == 1L) {
      ct <- rep(calltype, length(v))
      names(ct) <- names(v)
      attr(v, "calltype") <- ct
    } else {
      ct <- calltype
      names(ct) <- names(v)
      attr(v, "calltype") <- ct
    }
    return(v)
  }
  x <- as.data.frame(x)
  partition <- match.arg(partition)
  switch(partition,
    scatter = {
      v <- as.list(names(x))
      names(v) <- names(x)
    },
    collect = {
      v <- list(names(x))
      names(v) <- "collect"
    },
    void = {
      v <- list()
    },
    {
      v <- as.list(names(x))
      names(v) <- names(x)
    }
  )
  if (length(calltype) == 1L) {
    ct <- rep(calltype, length(v))
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
  } else {
    ct <- calltype
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
  }
  v
}

#' Name imputation blocks
#'
#' This helper function names any unnamed elements in the `blocks`
#' specification. This is a convenience function.
#' @inheritParams mice
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return A named list of character vectors with variables names.
#' @seealso [mice()]
#' @details
#' This function will name any unnamed list elements specified in
#' the optional argument `blocks`. Unnamed blocks
#' consisting of just one variable will be named after this variable.
#' Unnamed blocks containing more than one variables will be named
#' by the `prefix` argument, padded by an integer sequence
#' stating at 1.
#' @examples
#' blocks <- list(c("hyp", "chl"), AGE = "age", c("bmi", "hyp"), "edu")
#' name.blocks(blocks)
#' @export
name.blocks <- function(blocks, prefix = "B") {
  if (!is.list(blocks)) {
    return(make.blocks(blocks))
  }
  if (is.null(names(blocks))) names(blocks) <- rep("", length(blocks))
  inc <- 1
  for (i in seq_along(blocks)) {
    if (names(blocks)[i] != "") next
    if (length(blocks[[i]]) == 1) {
      names(blocks)[i] <- blocks[[i]][1]
    } else {
      names(blocks)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  blocks
}

check.blocks <- function(blocks, data, calltype = "formula") {
  data <- check.dataform(data)
  blocks <- name.blocks(blocks)

  # check that all variable names exists in data
  bv <- unique(unlist(blocks))
  notFound <- !bv %in% colnames(data)
  if (any(notFound)) {
    stop(paste(
      "The following names were not found in `data`:",
      paste(bv[notFound], collapse = ", ")
    ))
  }

  if (length(calltype) == 1L) {
    ct <- rep(calltype, length(blocks))
    names(ct) <- names(blocks)
    attr(blocks, "calltype") <- ct
  } else {
    ct <- calltype
    names(ct) <- names(blocks)
    attr(blocks, "calltype") <- ct
  }

  blocks
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
construct.blocks <- function(formulas = NULL, predictorMatrix = NULL) {
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
