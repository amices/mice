
# @param x the dataset containing missing values
# @param hmisc the "imputed" object from the 'aregImpute-class' object returned by Hmisc
# eg. hmisc_obj$imputed
# @param mice the "imp" object from the 'mids-class' object returned by mice
# eg. mice_obj$imp
check_missing <- function(x, hmisc= NULL, mice= NULL) {
  if (!is.null(hmisc) & !is.null(mice)) stop("only supply either hmisc OR mice.")
  if (!is.null(hmisc)) {
    return(all.equal(which(is.na(x)), as.integer(attr(hmisc, "dimnames")[[1]])))
  } else if (!is.null(mice)) {
    return(all.equal(which(is.na(x)), as.numeric(rownames(mice))))
  }
}

# @param miss the dataset containing missing values
# @param imp the object containing imputed values from either Hmisc or mice
check_inputs <- function(miss, imp) {
  m_p <- ncol(miss)
  i_p <- length(imp)
  if (m_p != i_p) stop("inputs must have the same number of variables")
  # assume matches for all if 1 matches
  idx <- which(unlist(lapply(imp, function(l) !is.null(l))))
  if (!check_missing(miss[[idx[1] ]], mice= imp[[idx[1] ]]))
    stop("missing data an imputed data do not match")
}

#' @title Extract mice imputations
#' @description Extract the imputed observations from a 'mids-class' object for a 
#' given multiple imputation
#' @param miss_dat the dataset containing missing values
#' @param mice the "imp" object from the 'mids-class' object returned by mice
#' eg. mice_obj$imp
#' @param j For multiple imputations, which imputation to extract. Defaults to 1.
#' @return A an equivalent dataset to \code{miss_dat} with imputed observations
#' @export
extract_imputations <- function(miss_dat, mice, j= 1) {
  check_inputs(miss= miss_dat, imp= mice)
  
  for (i in 1:ncol(miss_dat)) {
    if (sum(is.na(miss_dat[[i]])) == 0) next # no need to replace
    
    i_factor <- is.factor(miss_dat[[i]])
    if (i_factor) miss_dat[[i]] <- as.character(miss_dat[[i]])
    miss_dat[[i]][ which(is.na(miss_dat[[i]])) ] <- get(as.character(j), mice[[i]])
    if (i_factor) miss_dat[[i]] <- factor(miss_dat[[i]])
  } 
  return(miss_dat)
}