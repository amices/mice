# # ------------------------------summary.mira-------------------------------
# setMethod("summary", signature(object = "mira"), function(object) {
#     summary.mira(object)
# })
 

#'Summary of a \code{mira} object
#'
#'@rdname summary
#'@param object A \code{mira} object
#'@param type A length-1 character vector indicating the 
#'type of summary. There are three choices: \code{type = "tidy"}
#'return the parameters estimates of each analyses as a data frame.
#'\code{type = "glance"} return the fit statistics of each analysis
#'as a data frame. \code{type = "summary"} returns a list of
#'length \code{m} with the analysis results. The default is 
#'\code{"tidy"}.
#'@param ... Other parameters passed down to \code{print()} and \code{summary()}
#'@return \code{NULL}
#'@seealso \code{\link[=mira-class]{mira}}
#'@method summary mira
#'@export
summary.mira <- function(object, 
                         type = c("tidy", "glance", "summary"), 
                         ...) {
  type <- match.arg(type)
  fitlist <- getfit(object)
  if (type == "tidy")
    v <- lapply(fitlist, tidy, effects = "fixed", parametric = TRUE, ...) %>% bind_rows()
  if (type == "glance") 
    v <- lapply(fitlist, glance, ...) %>% bind_rows()
    # nobs is needed for pool.r.squared
    # not supplied by broom <= 0.5.6
    if (!'nobs' %in% colnames(v)) {
        v$nobs <- tryCatch(length(stats::residuals(getfit(object)[[1]])),
                           error = function(e) NULL)
    }
  if (type == "summary")
    v <- lapply(fitlist, summary, ...)
  v
}


#'Summary of a \code{mids} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link[=mids-class]{mids}}
#'@method summary mids
#'@export
summary.mids <- function(object, ...) {
  print(object, ...)
  invisible()
}


#
# ------------------------------summary.mads-------------------------------
#
#'Summary of a \code{mads} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link[=mads-class]{mads}}
#'@export
summary.mads <- function(object, ...) {
  print(object, ...)
  invisible()
}


#'Print a \code{mice.anova} object
#'
#'@rdname summary
#'@return \code{NULL}
#'@seealso \code{\link{mipo}}
#'@method summary mice.anova
#'@export
summary.mice.anova <- function(object,...) {
  
  # handle objects from anova
  out <- object$out
  # handle objects from D1, D2 and D3
  if (is.null(out))
    out <- list(`1 ~~ 2` = list(result = object$result, 
                                df.com = object$df.com))
  
  test <- names(out)
  df.com <- vapply(out, function(x) x$df.com, numeric(1))
  results <- t(vapply(out, function(x) x$result, numeric(5)))
  rf <- data.frame(test = test, 
                   statistic = results[, 1],
                   df1 = results[, 2], 
                   df2 = results[, 3],
                   df.com = df.com, 
                   p.value = results[, 4],
                   riv = results[, 5],
                   row.names = NULL)
  
  formulas <- object$formulas
  ff <- data.frame(model = names(formulas),
                   formula = as.character(formulas))
  
  structure(list(models = ff, comparisons = rf,
                 m = object$m, method = object$method, use = object$use),
            class = c("mice.anova.summary", class(object)))
}


