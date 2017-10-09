# # ------------------------------summary.mira-------------------------------
# setMethod("summary", signature(object = "mira"), function(object) {
#     summary.mira(object)
# })


#'Summary of a \code{mira} object
#'
#'@rdname summary
#'@param object A \code{mira} object
#'@param ... Other parameters passed down to \code{print()} and \code{summary()}
#'@return \code{NULL}
#'@seealso \code{\link[=mira-class]{mira}}
#'@method summary mira
#'@export
summary.mira <- function(object, ...) {
    # This summary function is for a mira object.  Then the seperate analyses are of class lm (glm), it calls sequentially
    # summary.lm (summary.glm) for all analyses.  KO, 4/2/00
    
    for (i in seq_along(object$analyses)) {
        cat("\n", "## summary of imputation", i, ":\n")
        print(summary(object$analyses[[i]], ...), ...)
    }
}

# # ------------------------------summary.mipo-------------------------------
# setMethod("summary", signature(object = "mipo"), function(object, ...) {
#     summary.mipo(object, ...)
# })
# 

#'Summary of a \code{mipo} object
#'
#'@rdname summary
#'@return A table containing summary statistis of the pooled analysis
#'@seealso \code{\link[=mipo-class]{mipo}}
#'@method summary mipo
#'@export
summary.mipo <- function(object, ...) {
    # summary method for the pooled analysis results
    # 
    # object: object of class mipo
    x <- object
    table <- array(x$qbar, dim = c(length(x$qbar), 10))
    dimnames(table) <- list(labels(x$qbar), c("est", "se", "t", "df", "Pr(>|t|)", "lo 95", "hi 95", "nmis", "fmi", "lambda"))
    table[, 2] <- sqrt(diag(x$t))
    table[, 3] <- table[, 1]/table[, 2]
    table[, 4] <- x$df
    table[, 5] <- if (all(x$df > 0)) 
        2 * (1 - pt(abs(table[, 3]), x$df)) else NA
    table[, 6] <- table[, 1] - qt(0.975, x$df) * table[, 2]
    table[, 7] <- table[, 1] + qt(0.975, x$df) * table[, 2]
    if (is.null(x$nmis) || is.null(names(x$qbar)))
        table[, 8] <- NA else table[, 8] <- x$nmis[names(x$qbar)]
    table[, 9] <- x$fmi
    table[, 10] <- x$lambda
    return(table)
}


# # --------------------------------SUMMARY.MIDS--------------------------------------
# setMethod("summary", signature(object = "mids"), function(object, ...) {
#     summary.mids(object, ...)
# })

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
