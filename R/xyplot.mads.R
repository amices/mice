
#
# -------------------------- xyplot.mads -------------------------------------
#

#'Scatterplot of amputed and non-amputed data against weighted sum scores
#'
#'Plotting method for amputed data using \pkg{lattice}. \code{xyplot}
#'produces scatterplots. The function plots the variables against the weighted 
#'sum scores. This is useful in analyzing the missing data mechanisms. The function
#'automatically separates the amputed and non-amputed data. 
#'
#'@param x A \code{mads} object, typically created by \code{\link{ampute}}.
#'@param yvar A string or vector of variable names that needs to be plotted. As 
#'a default, all variables will be plotted. 
#'@param which.pat A scalar or vector indicating which patterns need to be plotted. 
#'As a default, all patterns are plotted. 
#'@param standardized Logical. Whether the scatterplots need to be created
#'from standardized data or not. Default is TRUE. 
#'@param layout A vector of two values indicating how the scatterplots of one 
#'pattern should be divided over the plot. For example, \code{c(2, 3)} indicates 
#'that the scatterplots of six variables need to be placed on 3 rows and 2 columns. 
#'There are several defaults for different #variables. Note that for more than 
#'9 variables, multiple plots will be created automatically.
#'@param theme A named list containing the graphical parameters. The default
#'function \code{mice.theme} produces a short list of default colors, line
#'width, and so on. The extensive list may be obtained from
#'\code{trellis.par.get}.
#'@param outer See \code{\link[lattice:bwplot]{bwplot}}.
#'@param multiple See \code{\link[lattice:bwplot]{bwplot}}.
#'@return A list containing the scatterplots. Note that a new pattern 
#'will always be shown in a new plot. 
#'@note The \code{mads} object contains all the information you need to 
#'make any desired plots. Check \code{\link{mads-class}} or the vignette titled "Multivariate 
#'Amputation using Ampute" to understand the contents of class object \code{mads}. 
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}, \code{\link{bwplot}}, \code{\link{Lattice}} for 
#'an overview of the package, \code{\link{mads-class}}
#'@export
xyplot.mads <- function(x, yvar = NULL, which.pat = NULL,
                        standardized = TRUE, layout = NULL, theme = 
                          mice.theme(transparent = FALSE), multiple = TRUE, 
                        outer = TRUE) {
  if (!is.mads(x)) {
    stop("Object is not of class mads")
  }
  if (is.null(yvar)) {
    varlist <- colnames(x$amp)
  } else {
    varlist <- yvar
  }
  if (is.null(which.pat)) {
    pat <- nrow(x$patterns)
    which.pat <- c(1:pat)
  } else {
    pat <- length(which.pat)
  }
  if (standardized) {
    dat <- data.frame(scale(x$data))
  } else {
    dat <- x$data
  }
  data <- NULL
  for (i in 1:pat){
    can <- which(x$cand == which.pat[i])
    mis <- matrix(NA, nrow = length(can), ncol = 3)
    nc <- which(x$patterns[which.pat[i], ] == 0)
    if (length(nc) > 1){
      mis[apply(is.na(x$amp[can, nc]), 1, all), 1] <- 1
      mis[is.na(mis[, 1]), 1] <- 0
    } else if (length(nc) == 1) {
      mis[is.na(x$amp[can, nc]), 1] <- 1
      mis[is.na(mis[, 1]), 1] <- 0
    }
    mis[, 2] <- rep(which.pat[i], length(can))
    mis[, 3] <- unname(x$scores[[which.pat[i]]])
    data <- rbind(data, cbind(mis, dat[can, ]))
  }
  colnames(data) <- c(".amp", ".pat", "scores", names(x$data))
  formula = as.formula(paste("scores ~ ", 
                             paste(varlist, collapse = "+", sep = ""),
                             sep = ""))
  if (is.null(layout)) {
    if (length(varlist) > 6) {
      layout <- c(3, 3)
    } else if (length(varlist) > 4) {
      layout <- c(3, 2)
    } else if (length(varlist) > 2) {
      layout <- c(2, 2)
    } else if (length(varlist) > 1) {
      layout <- c(2, 1)
    }
  }
  p <- list()
  for (i in 1:pat) {
    p[[paste("Scatterplot Pattern", which.pat[i])]] <- 
      xyplot(x = formula, data = data[data$.pat == which.pat[i], ],
             groups = data$.amp, par.settings = theme,
             layout = layout, multiple = multiple, outer = outer, 
             ylab = "Weighted sum scores", 
             xlab = paste("Standardized values pattern", which.pat[i]))
  }
  return(p)
}
