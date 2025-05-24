#' Box-and-whisker plot of amputed and non-amputed data
#'
#' Plotting method to investigate the relation between the data variables and
#' the amputed data. The function shows how the amputed values are related
#' to the variable values.
#'
#' @method bwplot mads
#' @param x A \code{mads} (\code{\link{mads}}) object, typically created by
#' \code{\link{ampute}}.
#' @param data A string or vector of variable names that needs to be plotted. As
#' a default, all variables will be plotted.
#' @param which.pat A scalar or vector indicating which patterns need to be plotted.
#' As a default, all patterns are plotted.
#' @param standardized Logical. Whether the box-and-whisker plots need to be created
#' from standardized data or not. Default is TRUE.
#' @param descriptives Logical. Whether the mean, variance and n of the variables
#' need to be printed. This is useful to examine the effect of the amputation.
#' Default is TRUE.
#' @param layout A vector of two values indicating how the boxplots of one pattern
#' should be divided over the plot. For example, \code{c(2, 3)} indicates that the
#' boxplots of six variables need to be placed on 3 rows and 2 columns. Default
#' is 1 row and an amount of columns equal to #variables. Note that for more than
#' 6 variables, multiple plots will be created automatically.
#' @param \dots Not used, but for consistency with generic
#' @return A list containing the box-and-whisker plots. Note that a new pattern
#' will always be shown in a new plot.
#' @note The \code{mads} object contains all the information you need to
#' make any desired plots. Check \code{\link{mads}} or the vignette \emph{Multivariate
#' Amputation using Ampute} to understand the contents of class object \code{mads}.
#' @author Rianne Schouten, 2016
#' @seealso \code{\link{ampute}}, \code{\link[lattice]{bwplot}}, \code{\link{mads}}
#' @export
bwplot.mads <- function(x, data, which.pat = NULL, standardized = TRUE,
                        descriptives = TRUE, layout = NULL, ...) {
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
  formula <- as.formula(paste0(paste0(varlist, collapse = "+"), "~ factor(.amp)"))
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
  for (i in seq_len(pat)) {
    can <- which(x$cand == which.pat[i])
    mis <- matrix(NA, nrow = length(can), ncol = 2)
    nc <- which(x$patterns[which.pat[i], ] == 0)
    if (length(nc) > 1) {
      mis[apply(is.na(x$amp[can, nc]), 1, all), 1] <- "Amp"
      mis[is.na(mis[, 1]), 1] <- "Non-Amp"
    } else if (length(nc) == 1) {
      mis[is.na(x$amp[can, nc]), 1] <- "Amp"
      mis[is.na(mis[, 1]), 1] <- "Non-Amp"
    }
    mis[, 2] <- rep.int(which.pat[i], length(can))
    data <- rbind(data, cbind(mis, dat[can, ]))
  }
  colnames(data) <- c(".amp", ".pat", varlist)
  p <- list()
  vec1 <- c()
  vec3 <- c()
  for (i in seq_along(which.pat)) {
    vec1[((i * 2) - 1):(i * 2)] <- rep.int(paste(which.pat[i]), 2)
  }

  vec3 <- paste("", varlist)
  var <- length(varlist)
  if (descriptives) {
    desc <- array(NA,
      dim = c(2 * length(which.pat), 4, var),
      dimnames = list(
        Pattern = vec1,
        Descriptives = c("Amp", "Mean", "Var", "N"),
        Variable = vec3
      )
    )
    desc[, 1, ] <- rep.int(rep.int(c(1, 0), length(which.pat)), var)
    for (i in seq_along(which.pat)) {
      wp <- which.pat[i]
      desc[(i * 2) - 1, 2, ] <-
        round(vapply(varlist, function(x) {
          mean(data[data$.pat == wp & data$.amp == "Amp", x])
        }, numeric(1)), 5)
      desc[(i * 2), 2, ] <-
        round(vapply(varlist, function(x) {
          mean(data[data$.pat == wp & data$.amp == "Non-Amp", x])
        }, numeric(1)), 5)
      desc[(i * 2) - 1, 3, ] <-
        round(vapply(varlist, function(x) {
          var(data[data$.pat == wp & data$.amp == "Amp", x])
        }, numeric(1)), 5)
      desc[(i * 2), 3, ] <-
        round(vapply(varlist, function(x) {
          var(data[data$.pat == wp & data$.amp == "Non-Amp", x])
        }, numeric(1)), 5)
      desc[(i * 2) - 1, 4, ] <-
        vapply(varlist, function(x) {
          length(data[data$.pat == wp & data$.amp == "Amp", x])
        }, numeric(1))
      desc[(i * 2), 4, ] <-
        vapply(varlist, function(x) {
          length(data[data$.pat == wp & data$.amp == "Non-Amp", x])
        }, numeric(1))
    }
    p[["Descriptives"]] <- desc
  }

  theme <- list(
    superpose.symbol = list(col = "black", pch = 1),
    superpose.line = list(col = "black", lwd = 1),
    box.dot = list(col = "black"),
    box.rectangle = list(col = "black"),
    box.umbrella = list(col = "black"),
    box.symbol = list(col = "black"),
    plot.symbol = list(col = "black", pch = 1),
    plot.line = list(col = "black"),
    strip.background = list(col = "grey95")
  )

  for (i in seq_len(pat)) {
    p[[paste("Boxplot pattern", which.pat[i])]] <-
      lattice::bwplot(
        x = formula, data = data[data$.pat == which.pat[i], ],
        multiple = TRUE, outer = TRUE, layout = layout,
        ylab = "", par.settings = theme,
        xlab = paste("Data distributions in pattern", which.pat[i])
      )
  }
  p
}
