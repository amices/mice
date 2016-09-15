#
# -------------------------- bwplot.mads -------------------------------------
#

#'Box-and-whisker plot of amputed and non-amputed data
#'
#'Plotting method for amputed data using \pkg{lattice}. \code{bwplot}
#'produces box-and-whisker plots. The function automatically separates the 
#'amputed and non-amputed data. 
#'
#'@param x A \code{mads} object, typically created by \code{\link{ampute}}.
#'@param yvar A string or vector of variable names that needs to be plotted. As 
#'a default, all variables will be plotted. 
#'@param which.pat A scalar or vector indicating which patterns need to be plotted. 
#'As a default, all patterns are plotted. 
#'@param standardized Logical. Whether the box-and-whisker plots need to be created
#'from standardized data or not. Default is TRUE. 
#'@param descriptives Logical. Whether the mean, variance and n of the variables
#'need to be printed. This is useful to examine the effect of the amputation. 
#'Default is TRUE. 
#'@param layout A vector of two values indicating how the boxplots of one pattern
#'should be divided over the plot. For example, \code{c(2, 3)} indicates that the 
#'boxplots of six variables need to be placed on 3 rows and 2 columns. Default
#'is 1 row and an amount of columns equal to #variables. Note that for more than 
#'6 variables, multiple plots will be created automatically.
#'@param theme A named list containing the graphical parameters. The default
#'function \code{mice.theme} produces a short list of default colors, line
#'width, and so on. The extensive list may be obtained from
#'\code{trellis.par.get}.
#'@param outer See \code{\link[lattice:bwplot]{bwplot}}.
#'@param multiple See \code{\link[lattice:bwplot]{bwplot}}.
#'@return A list containing the box-and-whisker plots. Note that a new pattern 
#'will always be shown in a new plot. 
#'@note The \code{mads} object contains all the information you need to 
#'make any desired plots. Check \code{\link{mads-class}} or the vignette titled "Multivariate 
#'Amputation using Ampute" to understand the contents of class object \code{mads}.
#'@author Rianne Schouten, 2016
#'@seealso \code{\link{ampute}}, \code{\link{bwplot}}, \code{\link{Lattice}} for 
#'an overview of the package, \code{\link{mads-class}}
#'@export
bwplot.mads <- function(x, yvar = NULL, which.pat = NULL, standardized = TRUE,
                        descriptives = TRUE, layout = NULL, theme = mice.theme(), 
                        multiple = TRUE, outer = TRUE) {
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
  formula <- as.formula(paste(paste(varlist, collapse = "+", sep = ""), 
                              "~ factor(.amp)", sep = ""))
  data <- NULL
  if (standardized) {
    dat <- data.frame(scale(x$data))
  } else {
    dat <- x$data
  }
  if (is.null(layout)) {
    if (ceiling(length(varlist) / 2) > 6) {
      layout <- c(6, 1)
    } else {
      layout <- c(length(varlist), 1)
    }
  }
  for (i in 1:pat){
    can <- which(x$cand == which.pat[i])
    mis <- matrix(NA, nrow = length(can), ncol = 2)
    nc <- which(x$patterns[which.pat[i], ] == 0)
    if (length(nc) > 1){
      mis[apply(is.na(x$amp[can, nc]), 1, all), 1] <- "Amp"
      mis[is.na(mis[, 1]), 1] <- "Non-Amp"
    } else if (length(nc) == 1) {
      mis[is.na(x$amp[can, nc]), 1] <- "Amp"
      mis[is.na(mis[, 1]), 1] <- "Non-Amp"
    }
    mis[, 2] <- rep(which.pat[i], length(can))
    data <- rbind(data, cbind(mis, dat[can, ]))
  }
  colnames(data) <- c(".amp", ".pat", varlist)
  p <- list()
  vec1 <- c()
  vec3 <- c()
  for (i in 1:length(which.pat)) {
    vec1[((i*2)-1):(i*2)] <- rep(paste(which.pat[i]), 2)
  }
  for (j in 1:length(varlist)) {
    vec3[j] <- paste("", varlist[j])
  }
  if (descriptives) {
    var <- length(varlist)
    desc <- array(NA, dim = c(2 * length(which.pat), 4, var),
                  dimnames = list(Pattern = vec1, 
                                  Descriptives = c("Amp", "Mean", "Var", "N"), 
                                  Variable = vec3))
    desc[, 1, ] <- rep(rep(c(1, 0), length(which.pat)), var)
    for (i in 1:length(which.pat)) {
      wp <- which.pat[i]
      desc[(i*2) - 1, 2, ] <- 
        round(unname(sapply(data[data$.pat == wp & data$.amp == "Amp", 
                                 varlist], mean)), 5)
      desc[(i*2), 2, ] <- 
        round(unname(sapply(data[data$.pat == wp & data$.amp == "Non-Amp", 
                                 varlist], mean)), 5)
      desc[(i*2) - 1, 3, ] <- 
        round(unname(sapply(data[data$.pat == wp & data$.amp == "Amp", 
                                 varlist], var)), 5)
      desc[(i*2), 3, ] <- 
        round(unname(sapply(data[data$.pat == wp & data$.amp == "Non-Amp", 
                                 varlist], var)), 5)
      desc[(i*2) - 1, 4, ] <- 
        unname(sapply(data[data$.pat == wp & data$.amp == "Amp", 
                           varlist], length))
      desc[(i*2), 4, ] <- 
        unname(sapply(data[data$.pat == wp & data$.amp == "Non-Amp", 
                           varlist], length))
    }
    p[["Descriptives"]] <- desc
  }
  for (i in 1:pat) {
    p[[paste("Boxplot pattern", which.pat[i])]] <- 
      bwplot(x = formula, data = data[data$.pat == which.pat[i], ],
             multiple = multiple, outer = outer, layout = layout, 
             ylab = "", par.settings = list(strip.background = list(
               col = "grey95")), 
             xlab = paste("Data distributions in pattern", which.pat[i]))
  }
  return(p)
}
