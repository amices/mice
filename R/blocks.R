#' Creates a \code{blocks} argument
#'
#' This helper function generates a list of the type needed for
#' \code{blocks} argument in the \code{[=mice]{mice}} function.
#' @param data A \code{data.frame}, character vector with
#' variable names, or \code{list} with variable names.
#' @param partition A character vector of length 1 used to assign
#' variables to blocks when \code{data} is a \code{data.frame}. Value
#' \code{"scatter"} (default) will assign each column to it own
#' block. Value \code{"collect"} assigns all variables to one block,
#' whereas \code{"void"} produces an empty list.
#' @param calltype A character vector of \code{length(block)} elements
#' that indicates how the imputation model is specified. If
#' \code{calltype = "type"} (the default), the underlying imputation
#' model is called by means of the \code{type} argument. The
#' \code{type} argument for block \code{h} is equivalent to
#' row \code{h} in the \code{predictorMatrix}.
#' The alternative is \code{calltype = "formula"}. This will pass
#' \code{formulas[[h]]} to the underlying imputation
#' function for block \code{h}, together with the current data.
#' The \code{calltype} of a block is set automatically during
#' initialization. Where a choice is possible, calltype
#' \code{"formula"} is preferred over \code{"type"} since this is
#' more flexible and extendable. However, what precisely happens
#' depends also on the capabilities of the imputation
#' function that is called.
#' @return A named list of character vectors with variables names.
#' @details Choices \code{"scatter"} and \code{"collect"} represent to two
#' extreme scenarios for assigning variables to imputation blocks.
#' Use \code{"scatter"} to create an imputation model based on
#' \emph{fully conditionally specification} (FCS). Use \code{"collect"} to
#' gather all variables to be imputed by a \emph{joint model} (JM).
#' Scenario's in-between these two extremes represent
#' \emph{hybrid} imputation models that combine FCS and JM.
#'
#' Any variable not listed in will not be imputed.
#' Specification \code{"void"} represents the extreme scenario that
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
make.blocks <- function(data,
                        partition = c("scatter", "collect", "void"),
                        calltype = "type") {
  if (is.vector(data) && !is.list(data)) {
    v <- as.list(as.character(data))
    names(v) <- as.character(data)
    ct <- rep(calltype, length(v))
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
    return(v)
  }
  if (is.list(data) && !is.data.frame(data)) {
    v <- name.blocks(data)
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
  data <- as.data.frame(data)
  partition <- match.arg(partition)
  switch(partition,
    scatter = {
      v <- as.list(names(data))
      names(v) <- names(data)
    },
    collect = {
      v <- list(names(data))
      names(v) <- "collect"
    },
    void = {
      v <- list()
    },
    {
      v <- as.list(names(data))
      names(v) <- names(data)
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
#' This helper function names any unnamed elements in the \code{blocks}
#' specification. This is a convenience function.
#' @inheritParams mice
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return A named list of character vectors with variables names.
#' @seealso \code{\link{mice}}
#' @details
#' This function will name any unnamed list elements specified in
#' the optional argument \code{blocks}. Unnamed blocks
#' consisting of just one variable will be named after this variable.
#' Unnamed blocks containing more than one variables will be named
#' by the \code{prefix} argument, padded by an integer sequence
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

check.blocks <- function(blocks, data, calltype = "type") {
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

#' Construct blocks from \code{formulas} and \code{predictorMatrix}
#'
#' This helper function attempts to find blocks of variables in the
#' specification of the \code{formulas} and/or \code{predictorMatrix}
#' objects. Blocks specified by \code{formulas} may consist of
#' multiple variables. Blocks specified by \code{predictorMatrix} are
#' assumed to consist of single variables. Any duplicates in names are
#' removed, and the formula specification is preferred.
#' \code{predictorMatrix} and \code{formulas}. When both arguments
#' specify models for the same block, the model for the
#' \code{predictMatrix} is removed, and priority is given to the
#' specification given in \code{formulas}.
#' @inheritParams mice
#' @return A \code{blocks} object.
#' @seealso \code{\link{make.blocks}}, \code{\link{name.blocks}}
#' @examples
#' form <- name.formulas(list(bmi + hyp ~ chl + age, chl ~ bmi))
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
    ct <- rep("type", length(blocks.p))
    names(ct) <- names(blocks.p)
    attr(blocks.p, "calltype") <- ct
    if (is.null(formulas)) {
      return(blocks.p)
    }
  }

  # combine into unique blocks
  blocknames <- unique(c(names(blocks.f), names(blocks.p)))
  keep <- setdiff(blocknames, names(blocks.f))
  blocks <- c(blocks.f, blocks.p[keep])
  ct <- c(
    rep("formula", length(formulas)),
    rep("type", length(keep))
  )
  names(ct) <- names(blocks)
  attr(blocks, "calltype") <- ct
  blocks
}
