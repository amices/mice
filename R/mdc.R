#' Graphical parameter for missing data plots
#'
#' \code{mdc} returns colors used to distinguish observed, missing and combined
#' data in plotting. \code{mice.theme} return a partial list of named objects
#' that can be used as a theme in \code{stripplot}, \code{bwplot},
#' \code{densityplot} and \code{xyplot}.
#'
#' This function eases consistent use of colors in plots. The default follows
#' the Abayomi convention, which uses blue for observed data, red for missing or
#' imputed data, and black for combined data.
#'
#' @aliases mdc
#' @param r A numerical or character vector. The numbers 1-6 request colors as
#' follows: 1=\code{cso}, 2=\code{csi}, 3=\code{csc}, 4=\code{clo}, 5=\code{cli}
#' and 6=\code{clc}. Alternatively, \code{r} may contain the strings
#'' \code{observed}', '\code{missing}', or '\code{both}', or abbreviations
#' thereof.
#' @param s A character vector containing the strings '\code{symbol}' or
#'' \code{line}', or abbreviations thereof.
#' @param transparent A logical indicating whether alpha-transparency is
#' allowed. The default is \code{TRUE}.
#' @param cso The symbol color for the observed data. The default is a
#' transparent blue.
#' @param csi The symbol color for the missing or imputed data. The default is a
#' transparent red.
#' @param csc The symbol color for the combined observed and imputed data. The
#' default is a grey color.
#' @param clo The line color for the observed data. The default is a slightly
#' darker transparent blue.
#' @param cli The line color for the missing or imputed data. The default is a
#' slightly darker transparent red.
#' @param clc The line color for the combined observed and imputed data. The
#' default is a grey color.
#' @return \code{mdc()} returns a vector containing color definitions. The length
#' of the output vector is calculate from the length of \code{r} and \code{s}.
#' Elements of the input vectors are repeated if needed.
#' @author Stef van Buuren, sept 2012.
#' @seealso \code{\link{hcl}}, \code{\link{rgb}},
#' \code{\link{xyplot.mids}}, \code{\link[lattice:xyplot]{xyplot}},
#' \code{\link[lattice:trellis.par.get]{trellis.par.set}}
#' @references Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
#' Visualization with R}, Springer.
#' @keywords hplot
#' @examples
#' # all six colors
#' mdc(1:6)
#'
#' # lines color for observed and missing data
#' mdc(c("obs", "mis"), "lin")
#' @export
mdc <- function(r = "observed",
                s = "symbol",
                transparent = TRUE,
                cso = grDevices::hcl(240, 100, 40, 0.7),
                csi = grDevices::hcl(0, 100, 40, 0.7),
                csc = "gray50",
                clo = grDevices::hcl(240, 100, 40, 0.8),
                cli = grDevices::hcl(0, 100, 40, 0.8),
                clc = "gray50") {
  # cso: blue symbol color for observed data
  # csi: red symbol color for imputations
  # csc: symbol color for combined data
  # clo: blue line color for observed data
  # cli: red line color for observed data
  # clc: line color for combined data

  if (missing(transparent)) {
    if (!supports.transparent()) {
      cso <- grDevices::hcl(240, 100, 40)
      csi <- grDevices::hcl(0, 100, 40)
      csc <- "black"
      clo <- grDevices::hcl(240, 100, 40)
      cli <- grDevices::hcl(0, 100, 40)
      clc <- "black"
    }
  } else if (!transparent) {
    cso <- grDevices::hcl(240, 100, 40)
    csi <- grDevices::hcl(0, 100, 40)
    csc <- "black"
    clo <- grDevices::hcl(240, 100, 40)
    cli <- grDevices::hcl(0, 100, 40)
    clc <- "black"
  }

  fallback <- grDevices::palette()[1]
  if (is.numeric(r)) {
    idx <- floor(r)
    idx[r < 1 | r > 6] <- 7
    myc <- c(cso, csi, csc, clo, cli, clc, fallback)[idx]
    return(myc)
  }
  rc <- pmatch(r, c("observed", "missing", "both"))
  sc <- pmatch(s, c("symbol", "line"))
  idx <- rc + (sc - 1) * 3
  idx[is.na(idx)] <- 7
  myc <- c(cso, csi, csc, clo, cli, clc, fallback)[idx]
  myc
}
