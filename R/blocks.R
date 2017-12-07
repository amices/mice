#' Partition variables into imputation blocks
#'
#' This helper function generates a list of the type needed for 
#' \code{blocks} argument in the \code{[=mice]{mice}} function.
#' @inheritParams mice
#' @param partition A character vector of length 1. Value 
#' \code{"fcs"} (default) will assign each column to it own 
#' block (fully conditional specification). Value 
#' \code{"joint"} assigns all variables to on block
#' called \code{"joint"} (joint model), whereas \code{"void"}
#' produces an empty list.
#' @return A named list of character vectors with variables names.
#' @details The \code{"fcs"} and \code{"joint"} represent to two 
#' extreme scenarios for assigning variables to imputation block. 
#' In practice, the user may specify hybrid imputation models that 
#' combine aspects of joint and fully conditionally specified models.
#' 
#' Any variable not listed in will not be imputed. 
#' Specification \code{"void"} reprsents the extreme scenario that 
#' skips imputation of all variables. 
#' 
#' A variable may be a member of multiple blocks. This may be useful 
#' in scenario's where the same (complete) background factors appear in 
#' multiple imputation blocks, but beware that such might introduce 
#' inconsistencies as the same variable is re-imputed in several 
#' different blocks.
#' @examples
#' make.blocks(nhanes)
#' make.blocks(c("age", "sex", "edu"))
#' @export
make.blocks <- function(data, 
                        partition = c("fcs", "joint", "void")) {
  if (is.vector(data)) {
    v <- as.list(as.character(data))
    names(v) <- as.character(data)
    return(v)
  }
  data <- as.data.frame(data)
  partition <- match.arg(partition)
  switch(partition, 
         fcs = {
           v <- as.list(names(data))
           names(v) <- names(data)
         },
         joint = {
           v <- list(names(data))
           names(v) <- "joint"
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
