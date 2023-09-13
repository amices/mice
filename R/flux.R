#' Influx and outflux of multivariate missing data patterns
#'
#' Influx and outflux are statistics of the missing data pattern. These
#' statistics are useful in selecting predictors that should go into the
#' imputation model.
#'
#' Infux and outflux have been proposed by Van Buuren (2018), chapter 4.
#'
#' Influx is equal to the number of variable pairs `(Yj , Yk)` with
#' `Yj` missing and `Yk` observed, divided by the total number of
#' observed data cells. Influx depends on the proportion of missing data of the
#' variable. Influx of a completely observed variable is equal to 0, whereas for
#' completely missing variables we have influx = 1. For two variables with the
#' same proportion of missing data, the variable with higher influx is better
#' connected to the observed data, and might thus be easier to impute.
#'
#' Outflux is equal to the number of variable pairs with `Yj` observed and
#' `Yk` missing, divided by the total number of incomplete data cells.
#' Outflux is an indicator of the potential usefulness of `Yj` for imputing
#' other variables. Outflux depends on the proportion of missing data of the
#' variable. Outflux of a completely observed variable is equal to 1, whereas
#' outflux of a completely missing variable is equal to 0. For two variables
#' having the same proportion of missing data, the variable with higher outflux
#' is better connected to the missing data, and thus potentially more useful for
#' imputing other variables.
#'
#' FICO is an outbound statistic defined by the fraction of incomplete cases
#' among cases with `Yj` observed (White and Carlin, 2010).
#'
#' @aliases flux
#' @param data A data frame or a matrix containing the incomplete data.  Missing
#' values are coded as NA's.
#' @param local A vector of names of columns of `data`. The default is to
#' include all columns in the calculations.
#' @return A data frame with `ncol(data)` rows and six columns:
#' pobs = Proportion observed,
#' influx = Influx
#' outflux = Outflux
#' ainb = Average inbound statistic
#' aout = Average outbound statistic
#' fico = Fraction of incomplete cases among cases with `Yj` observed
#' @seealso [fluxplot()], [md.pattern()], [fico()]
#' @author Stef van Buuren, 2012
#' @references
#' Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/missing-data-pattern.html#sec:flux)
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#' compared with complete-case analysis for missing covariate values.
#' *Statistics in Medicine*, *29*, 2920-2931.
#' @keywords misc
#' @export
flux <- function(data, local = names(data)) {
  .avg <- function(row) sum(row, na.rm = TRUE) / (length(row) - 1)
  ## calculates influx and outflux statistics
  ## of the missing data pattern
  x <- colMeans(!is.na(data))
  pat <- md.pairs(data)
  pat$rr <- pat$rr[local, , drop = FALSE]
  pat$rm <- pat$rm[local, , drop = FALSE]
  pat$mr <- pat$mr[local, , drop = FALSE]
  pat$mm <- pat$mm[local, , drop = FALSE]
  ainb <- apply(pat$mr / (pat$mr + pat$mm), 1, .avg)
  aout <- apply(pat$rm / (pat$rm + pat$rr), 1, .avg)
  fico <- fico(data)
  outflux <- rowSums(pat$rm) / (rowSums(pat$rm + pat$mm))
  influx <- rowSums(pat$mr) / (rowSums(pat$mr + pat$rr))
  data.frame(pobs = x, influx = influx, outflux = outflux, ainb = ainb, aout = aout, fico = fico)
}


#' Fluxplot of the missing data pattern
#'
#' Influx and outflux are statistics of the missing data pattern. These
#' statistics are useful in selecting predictors that should go into the
#' imputation model.
#'
#' Infux and outflux have been proposed by Van Buuren (2012), chapter 4.
#'
#' Influx is equal to the number of variable pairs `(Yj , Yk)` with
#' `Yj` missing and `Yk` observed, divided by the total number of
#' observed data cells. Influx depends on the proportion of missing data of the
#' variable. Influx of a completely observed variable is equal to 0, whereas for
#' completely missing variables we have influx = 1. For two variables with the
#' same proportion of missing data, the variable with higher influx is better
#' connected to the observed data, and might thus be easier to impute.
#'
#' Outflux is equal to the number of variable pairs with `Yj` observed and
#' `Yk` missing, divided by the total number of incomplete data cells.
#' Outflux is an indicator of the potential usefulness of `Yj` for imputing
#' other variables. Outflux depends on the proportion of missing data of the
#' variable. Outflux of a completely observed variable is equal to 1, whereas
#' outflux of a completely missing variable is equal to 0. For two variables
#' having the same proportion of missing data, the variable with higher outflux
#' is better connected to the missing data, and thus potentially more useful for
#' imputing other variables.
#'
#' @aliases fluxplot
#' @param data A data frame or a matrix containing the incomplete data.  Missing
#' values are coded as NA's.
#' @param local A vector of names of columns of `data`. The default is to
#' include all columns in the calculations.
#' @param plot Should a graph be produced?
#' @param labels Should the points be labeled?
#' @param xlim See `par`.
#' @param ylim See `par`.
#' @param las See `par`.
#' @param xlab See `par`.
#' @param ylab See `par`.
#' @param main See `par`.
#' @param eqscplot Should a square plot be produced?
#' @param pty See `par`.
#' @param lwd See `par`. Controls axis line thickness and diagonal
#' @param \dots Further arguments passed to `plot()` or `eqscplot()`.
#' @return An invisible data frame with `ncol(data)` rows and six columns:
#' pobs = Proportion observed,
#' influx = Influx
#' outflux = Outflux
#' ainb = Average inbound statistic
#' aout = Average outbound statistic
#' fico = Fraction of incomplete cases among cases with `Yj` observed
#' @seealso [flux()], [md.pattern()], [fico()]
#' @author Stef van Buuren, 2012
#' @references
#' Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/missing-data-pattern.html#sec:flux)
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#' compared with complete-case analysis for missing covariate values.
#' *Statistics in Medicine*, *29*, 2920-2931.
#' @keywords misc
#' @export
fluxplot <- function(data, local = names(data),
                     plot = TRUE, labels = TRUE,
                     xlim = c(0, 1), ylim = c(0, 1), las = 1,
                     xlab = "Influx", ylab = "Outflux",
                     main = paste("Influx-outflux pattern for", deparse(substitute(data))),
                     eqscplot = TRUE, pty = "s",
                     lwd = 1,
                     ...) {
  f <- flux(data, local)
  if (plot) {
    if (eqscplot) {
      MASS::eqscplot(
        x = f$influx, y = f$outflux, type = "n",
        main = main,
        xlab = xlab, ylab = ylab,
        xlim = xlim, ylim = ylim,
        pty = pty, lwd = lwd, axes = FALSE, ...
      )
    } else {
      plot(
        x = f$influx, y = f$outflux, type = "n",
        main = main,
        xlab = xlab, ylab = ylab,
        xlim = xlim, ylim = ylim,
        pty = pty, lwd = lwd, axes = FALSE, ...
      )
    }
    axis(1, lwd = lwd, las = las)
    axis(2, lwd = lwd, las = las)
    abline(1, -1, lty = 2, lwd = lwd)
    if (labels) {
      text(x = f$influx, y = f$outflux, label = names(data), ...)
    } else {
      points(x = f$influx, y = f$outflux, ...)
    }
    box(lwd = lwd)
  }
  invisible(data.frame(f))
}

#' Fraction of incomplete cases among cases with observed
#'
#' FICO is an outbound statistic defined by the fraction of incomplete cases
#' among cases with `Yj` observed (White and Carlin, 2010).
#'
#' @aliases fico
#' @param data A data frame or a matrix containing the incomplete data.  Missing
#' values are coded as NA's.
#' @return A vector of length `ncol(data)` of FICO statistics.
#' @seealso [fluxplot()], [flux()], [md.pattern()]
#' @author Stef van Buuren, 2012
#' @references
#' Van Buuren, S. (2018).
#' [*Flexible Imputation of Missing Data. Second Edition.*](https://stefvanbuuren.name/fimd/missing-data-pattern.html#sec:flux)
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' White, I.R., Carlin, J.B. (2010). Bias and efficiency of multiple imputation
#' compared with complete-case analysis for missing covariate values.
#' *Statistics in Medicine*, *29*, 2920-2931.
#' @keywords misc
#' @export
fico <- function(data) {
  ic <- ici(data)
  unlist(lapply(data, FUN = function(x) sum((!is.na(x)) & ic) / sum(!is.na(x))))
}
