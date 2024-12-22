#' Creates a \code{blocks} argument
#'
#' The `make.blocks()` helper function generates a suitable
#' \code{blocks} argument for the \code{[=mice]{mice}} function.
#'
#' @param x A `data.frame`, character vector with
#' variable names, or `list` with variable names.
#' @param partition A character vector of length 1 used to assign
#' variables to blocks when \code{data} is a \code{data.frame}. Value
#' \code{"scatter"} (default) assigns each variable to it own
#' block. Value \code{"collect"} assigns all variables to a single block.
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
make.blocks <- function(x,
                        partition = c("scatter", "collect", "void")) {

  # character: assign each variable to its own block
  if (is.character(x)) {
    v <- as.list(as.character(x))
    names(v) <- make.unique(as.character(x))
    return(v)
  }

  # list: specify (named) list of blocks
  if (is.list(x) && !is.data.frame(x)) {
    v <- name.blocks(x)
    return(v)
  }

  # matrix: convert to data.frame
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }

  if (!is.data.frame(x)) {
    stop("x must be a data.frame, character vector, or list")
  }

  # data.frame: assign each variable to its own block
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
  return(v)
}

#' Name imputation blocks
#'
#' This helper function names any unnamed elements in the \code{blocks}
#' specification. This is a convenience function.
#' @inheritParams make.blocks
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
name.blocks <- function(x, prefix = "B") {
  if (!is.list(x)) {
    return(make.blocks(x))
  }
  if (is.null(names(x))) names(x) <- rep("", length(x))
  inc <- 1
  for (i in seq_along(x)) {
    if (names(x)[i] != "") next
    if (length(x[[i]]) == 1) {
      names(x)[i] <- x[[i]][1]
    } else {
      names(x)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  return(x)
}

check.blocks <- function(blocks, data) {
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
    if (is.null(predictorMatrix)) {
      return(blocks.f)
    }
  }

  if (!is.null(predictorMatrix)) {
    if (is.null(row.names(predictorMatrix))) {
      stop("No row names in predictorMatrix", call. = FALSE)
    }
    blocks.p <- name.blocks(row.names(predictorMatrix))
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
  return(blocks)
}
