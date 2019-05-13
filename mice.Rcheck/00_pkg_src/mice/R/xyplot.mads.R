#
# -------------------------- xyplot.mads -------------------------------------
#

#'Scatterplot of amputed and non-amputed data against weighted sum scores
#'
#'Plotting method to investigate relation between amputed data and the weighted sum 
#'scores. Based on \code{\link{lattice}}. \code{xyplot} produces scatterplots. 
#'The function plots the variables against the weighted sum scores. The function
#'automatically separates the amputed and non-amputed data to see the relation between
#'the amputation and the weighted sum scores.  
#'
#'@param x A \code{mads} object, typically created by \code{\link{ampute}}.
#'@param data A string or vector of variable names that needs to be plotted. As 
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
#'@param colors A vector of two RGB values defining the colors of the non-amputed and 
#'amputed data respectively. RGB values can be obtained with \code{\link{hcl}}.
#'@param \dots Not used, but for consistency with generic
#'@return A list containing the scatterplots. Note that a new pattern 
#'will always be shown in a new plot. 
#'@note The \code{mads} object contains all the information you need to 
#'make any desired plots. Check \code{\link{mads-class}} or the vignette \emph{Multivariate 
#'Amputation using Ampute} to understand the contents of class object \code{mads}. 
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}, \code{\link{bwplot}}, \code{\link{Lattice}} for 
#'an overview of the package, \code{\link{mads-class}}
#'@export
xyplot.mads <- function(x, data, which.pat = NULL,
                        standardized = TRUE, layout = NULL,
                        colors = mdc(1:2), ...) {
  if (!is.mads(x)) {
    stop("Object is not of class mads")
  }
  if (missing(data)) data <- NULL
  yvar <- data
  if (is.null(yvar)) {
    varlist <- colnames(x$amp)
  } else {
    varlist <- yvar
  }
  if (is.null(which.pat)) {
    pat <- nrow(x$patterns)
    which.pat <- seq_len(pat)
  } else {
    pat <- length(which.pat)
  }
  if (standardized) {
    dat <- data.frame(scale(x$data))
    xlab <- "Standardized values in pattern"
  } else {
    dat <- x$data
    xlab <- "Data values in pattern"
  }
  data <- NULL
  for (i in seq_len(pat)){
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
    mis[, 2] <- rep.int(which.pat[i], length(can))
    mis[, 3] <- unname(x$scores[[which.pat[i]]])
    data <- rbind(data, cbind(mis, dat[can, ]))
  }
  colnames(data) <- c(".amp", ".pat", "scores", names(x$data))
  data$.amp <- factor(data$.amp, levels = c(0, 1))
  formula = as.formula(paste0("scores ~ ", paste0(varlist, collapse = "+")))
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
  
  theme <- list(superpose.symbol = list(col = colors, pch = 1),
                plot.symbol = list(col = colors, pch = 1),
                strip.background = list(col = "grey95"))
  key <- list(columns = 2, points = list(col = colors, pch = 1), 
              text = list(c("Non-Amputed Data", "Amputed Data")))
  
  p <- stats::setNames(vector(mode = "list", length = pat), paste("Scatterplot Pattern", which.pat))
  for (i in seq_len(pat)) {
    p[[paste("Scatterplot Pattern", which.pat[i])]] <- 
      xyplot(x = formula, data = data[data$.pat == which.pat[i], ],
             groups = data$.amp, par.settings = theme,
             multiple = TRUE, outer = TRUE, layout = layout, key = key, 
             ylab = "Weighted sum scores", 
             xlab = paste(xlab, which.pat[i]))
  }
  return(p)
}
