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
#' @export
make.blocks <- function(data, partition = c("fcs", "joint", "void")) {
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
         })
  v
}
