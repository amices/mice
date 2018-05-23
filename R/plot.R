#'Plot the trace lines of the MICE algorithm
#' 
#'Trace line plots portray the value of an estimate
#'against the iteration number. The estimate can be anything that you can calculate, but
#'typically are chosen as parameter of scientific interest. The \code{plot} method for
#'a \code{mids} object plots the mean and standard deviation of the imputed (not observed) 
#'values against the iteration number for each of the $m$ replications. By default, 
#'the function plot the development of the mean and standard deviation for each incomplete
#'variable. On convergence, the streams should intermingle and be free of any trend.
#'
#'@param x An object of class \code{mids}
#'@param y A formula that specifies which variables, stream and iterations are plotted. 
#'If omitted, all streams, variables and iterations are plotted.
#'@param theme The trellis theme to applied to the graphs. The default is \code{mice.theme()}.
#'@param layout A vector of length 2 given the number of columns and rows in the plot. 
#'The default is \code{c(2, 3)}.
#'@param type Parameter \code{type} of \code{\link{panel.xyplot}}. 
#'@param col Parameter \code{col} of \code{\link{panel.xyplot}}.
#'@param lty Parameter \code{lty} of \code{\link{panel.xyplot}}.
#'@param ... Extra arguments for \code{\link{xyplot}}. 
#'@return An object of class \code{"trellis"}.
#'@author Stef van Buuren 2011
#'@seealso \code{\link{mice}}, \code{\link[=mids-class]{mids}}, 
#'\code{\link{xyplot}}
#'@method plot mids
#'@export
plot.mids <- function(x, y = NULL, theme = mice.theme(), layout = c(2, 3), 
                      type = "l", col = 1:10, lty = 1, ...) {
    strip.combined <- function(which.given, which.panel, factor.levels, ...) {
        if (which.given == 1) {
            lattice::panel.rect(0, 0, 1, 1, 
                                col = theme$strip.background$col, border = 1)
            lattice::panel.text(x = 0, y = 0.5, pos = 4, 
                                lab = factor.levels[which.panel[which.given]])
        }
        if (which.given == 2) {
            lattice::panel.text(x = 1, y = 0.5, pos = 2, 
                                lab = factor.levels[which.panel[which.given]])
        }
    }
    
    call <- match.call()
    if (!is.mids(x)) 
      stop("argument 'x' must be a 'mids' object", call. = FALSE)
    if (is.null(x$chainMean))
      stop("no convergence diagnostics found", call. = FALSE)
    
    mn <- x$chainMean
    sm <- sqrt(x$chainVar)
    
    # select subset of nonmissing entries
    obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
    varlist <- names(obs)[obs]
    
    ## create formula if not given in y
    if (missing(y)) {
        formula <- as.formula(paste0(paste0(varlist, collapse = "+"), 
                                     "~.it|.ms"))
    } else {
        formula <- NULL
        if (is.null(y)) 
            formula <- as.formula(paste0(paste0(varlist, collapse = "+"), 
                                         "~.it|.ms"))
        if (is.character(y)) {
                formula <- if (length(y) == 1) as.formula(paste0(y, "~.it|.ms")) 
                else as.formula(paste0(paste0(y, collapse = "+"), "~.it|.ms"))
        }
        if (is.integer(y) || is.logical(y)) {
            vars <- varlist[y]
                formula <- if (length(vars) == 1) as.formula(paste0(vars, "~.it|.ms")) 
                else as.formula(paste0(paste0(vars, collapse = "+"), "~.it|.ms"))
        }
        if (is.null(formula)) 
            formula <- as.formula(y)
    }
    
    m <- x$m
    it <- x$iteration
    mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
    sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
    
    adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
    data <- cbind(adm, rbind(mn, sm))
    colnames(data) <- c(".it", ".m", ".ms", varlist)
    .m <- NULL
    rm(.m)  ## Dummy to trick R CMD check
    
    tp <- xyplot(x = formula, data = data, groups = .m, 
                 type = type, lty = lty, col = col, layout = layout, 
                 scales = list(y = list(relation = "free"),
                               x = list(alternating = FALSE)),
                 as.table = TRUE,
                 xlab = "Iteration",
                 ylab = "",
                 strip = strip.combined,
                 par.strip.text = list(lines=0.5),
                 ...)
    tp <- update(tp, par.settings = theme)
    return(tp)
}

# 
# setMethod("plot", signature(x = "mids", y = "ANY"), function(x, y, ...) {
#     plot.mids(x, y, ...)
# })
