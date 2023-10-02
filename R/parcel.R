#' Creates a `parcel` argument
#'
#' This helper function generates a character vector for the
#' `parcel` argument in the [mice()] function.
#'
#' @param x A `data.frame`, an unnamed character vector, a named
#' character vector or a `list`.
#' @param partition Only relevant if `x` is a `data.frame`. Value
#' `"scatter"` (default) will assign each variable to a separate
#' parcel. Value `"collect"` assigns all variables to one parcel,
#' whereas `"void"` does not assign any variable to a parcel.
#' @param prefix A character vector of length 1 with the prefix to
#' be using for naming any unnamed blocks with two or more variables.
#' @return A character vector of length `ncol(data)` that specifies
#' the parcel name per variable
#'
#' @details Choices `"scatter"` and `"collect"` represent to two
#' extreme scenarios for assigning variables to imputation parcels.
#' Use `"scatter"` to create an imputation model based on
#' *fully conditionally specification* (FCS). Use `"collect"` to
#' gather all variables to be imputed by a *joint model* (JM).
#'
#' Any variable not listed in the result will not be imputed.
#' Specification `"void"` represents the extreme scenario where
#' nothing is imputed.
#'
#' Unlike blocks, a variable cannot be allocated to multiple parcels.
#' @examples
#'
#' # default parcel creation (scatter)
#' make.parcel(nhanes)
#'
#' # make parcel from variable names
#' make.parcel(c("age", "sex", "edu"))
#'
#' # put hgt, wgt and bmi into one parcel, automatic naming
#' make.parcel(list("age", "sex", c("hgt", "wgt", "bmi")))
#'
#' # same, but with custom parcel names
#' make.parcel(list("age", "sex", anthro = c("hgt", "wgt", "bmi")))
#'
#' # all variables into one parcel
#' make.parcel(nhanes, partition = "collect", prefix = "myblock")
#' @export
make.parcel <- function(x,
                      partition = c("scatter", "collect", "void"),
                      prefix = "b") {

  # unnamed vector
  if (is.vector(x) && is.null(names(x)) && !is.list(x)) {
    parcel <- as.character(x)
    names(parcel) <- as.character(x)
    return(parcel)
  }

  # named vector, preserve name order
  if (is.vector(x) && !is.null(names(x)) && !is.list(x)) {
    parcel <- as.character(x)
    names(parcel) <- names(x)
    return(parcel)
  }

  # unnamed list
  if (is.list(x) && is.null(names(x)) && !is.data.frame(x)) {
    parcel <- b2n(name.blocks(x, prefix = prefix))
    return(parcel)
  }

  # named list
  if (is.list(x) && !is.null(names(x)) && !is.data.frame(x)) {
    parcel <- b2n(x)
    return(parcel)
  }

  x <- as.data.frame(x)
  partition <- match.arg(partition)
  switch(partition,
         scatter = {
           parcel <- colnames(x)
           names(parcel) <- names(x)
         },
         collect = {
           parcel <- rep(prefix, ncol(x))
           names(parcel) <- names(x)
         },
         void = {
           parcel <- rep("", ncol(x))
           names(parcel) <- names(x)
         },
         {
           parcel <- names(x)
           names(parcel) <- names(x)
         }
  )
  return(parcel)
}

name.parcel <- function(x) x

check.parcel <- function(parcel, data) {
  data <- check.dataform(data)
  parcel <- name.parcel(parcel)

  # check that all variable names exists in data
  nv <- names(parcel)
  notFound <- !nv %in% colnames(data)
  if (any(notFound)) {
    stop(paste(
      "The following names were not found in `data`:",
      paste(nv[notFound], collapse = ", ")
    ))
  }

  parcel
}

reorder.parcel <- function(parcel, data) {
  idx <- colnames(data)
  return(parcel[idx])
}
