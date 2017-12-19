#' Partition variables into imputation blocks
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
#' @return A named list of character vectors with variables names.
#' @details Choices \code{"scatter"} and \code{"collect"} represent to two 
#' extreme scenarios for assigning variables to imputation blocks. 
#' Use \code{"scatter"} to create an imputation model based on 
#' \emph{fully conditionally specification} (FCS). Use \code{"collect"} to 
#' gather all variables to be imputed by a \emph{joint model} (JM). 
#' Scenario's in-between these two extremes respresent 
#' \emph{hybrid} imputation models that combine FCS and JM.
#' 
#' Any variable not listed in will not be imputed. 
#' Specification \code{"void"} reprsents the extreme scenario that 
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
                        partition = c("scatter", "collect", "void")) {
  if (is.vector(data) && !is.list(data)) {
    v <- as.list(as.character(data))
    names(v) <- as.character(data)
    return(v)
  }
  if (is.list(data) && !is.data.frame(data))
    return(name.blocks(data))
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
         })
  v
}

#' Name imputation blocks
#'
#' This helper function names any unnaned elements in the \code{blocks} 
#' specification. This is a convenience function.
#' @inheritParams mice
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return A named list of character vectors with variables names.
#' @seealso \code{\link{mice}}
#' @details 
#' This function will name any unnamed list elements specified in 
#' the optional argument \code{blocks}. Unnanmed blocks 
#' consisting of just one variable will be named after this variable.
#' Unnamed blocks containing more than one variables will be named 
#' by the \code{prefix} argument, padded by an integer sequence 
#' stating at 1.
#' @examples
#' blocks <- list(c("hyp", "chl"), AGE = "age", c("bmi", "hyp"), "edu")
#' name.blocks(blocks)
#' @export
name.blocks <- function(blocks, prefix = "B") {
  if (!is.list(blocks)) return(make.blocks(blocks))
  if (is.null(names(blocks))) names(blocks) <- rep("", length(blocks))
  inc <- 1
  for (i in seq_along(blocks)) {
    if (names(blocks)[i] != "") next
    if (length(blocks[[i]]) == 1) names(blocks)[i] <- blocks[[i]][1]
    else {
      names(blocks)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  blocks
}

check.blocks <- function(blocks, data) {
  
  # for proper workings, name all blocks  
  blocks <- name.blocks(blocks)
  
  # check that all variable names exists in data
  bv <- unique(unlist(blocks))
  notFound <- !bv %in% colnames(data)
  if (any(notFound)) 
    stop(paste("The following names were not found in `data`:",
               paste(bv[notFound], collapse = ", ")))
  blocks
}

extract.blocks <- function(formulas) {
  if (!all(sapply(formulas, is.formula))) return(NULL)
  name.blocks(lapply(formulas, lhs))
}
