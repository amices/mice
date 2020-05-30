#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' Tidy method to extract results from a `mira` object
#'
#' @param x An object of class \code{mira}
#' @param conf.int Logical. Should confidence intervals be returned?
#' @param conf.level Confidence level for intervals. Defaults to .95
#' @param ... extra arguments (not used)
#' @export
#' @keywords internal
#' @return A dataframe withh these columns:
#' \itemize{
#'      \item term
#'      \item estimate
#'      \item ubar
#'      \item b
#'      \item t
#'      \item dfcom
#'      \item df
#'      \item riv
#'      \item lambda
#'      \item fmi
#'      \item p.value
#'      \item conf.low (if called with conf.int = TRUE)
#'      \item conf.high (if called with conf.int = TRUE)
#' }
tidy.mira <- function(x, conf.int = FALSE, conf.level = .95, ...) {

         out <- pool(x)
         out <- summary(out, type = 'all', 
                        conf.int = conf.int, conf.level = conf.level)
         out$term <- as.character(out$term)

         # rename variables if present
         idx <- grepl("%", names(out))
         names(out)[idx] <- c("conf.low", "conf.high")
         idx <- names(out) == 't'
         names(out)[idx] <- 'statistic'

         # order columns
         cols_a <- c('term', 'estimate', 'std.error', 'statistic', 'p.value',
                     'conf.low', 'conf.high')
         cols_a <- base::intersect(cols_a, colnames(out))
         cols_b <- sort(base::setdiff(colnames(out), cols_a))
         out <- out[, c(cols_a, cols_b)]

         return(out)
}

#' Glance method to extract information from a `mira` object
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used)
#' @return a dataframe with one row and the following columns:
#' \itemize{
#'   \item nimp
#'   \item nobs
#' }
#' @note If x contains `lm` models, R2 and Adj.R2 are included in the output
#' @export
#' @keywords internal
#' @family tidiers
glance.mira <- function(x, ...) {

    out <- data.frame('nimp' = length(x$analyses))
	out$nobs <- tryCatch(stats::nobs(x$analyses[[1]]), 
                         error = function(e) NULL)

	if (class(x$analyses[[1]])[1] == "lm") {
	    out$r.squared <- pool.r.squared(x, adjusted = FALSE)[1]
	    out$adj.r.squared <- pool.r.squared(x, adjusted = TRUE)[1]
	}

	return(out)
}
