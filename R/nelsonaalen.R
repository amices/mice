# -----------------------------NELSONAALEN-------------------------------------

#'Cumulative hazard rate or Nelson-Aalen estimator
#'
#'Calculates the cumulative hazard rate (Nelson-Aalen estimator)
#'
#'This function is useful for imputing variables that depend on survival time.
#'White and Royston (2009) suggested using the cumulative hazard to the
#'survival time H0(T) rather than T or log(T) as a predictor in imputation
#'models.  See section 7.1 of Van Buuren (2012) for an example.
#'
#'@aliases nelsonaalen hazard
#'@param data A data frame containing the data.
#'@param timevar The name of the time variable in \code{data}.
#'@param statusvar The name of the event variable, e.g. death in \code{data}.
#'@return A vector with \code{nrow(data)} elements containing the Nelson-Aalen
#'estimates of the cumulative hazard function.
#'@author Stef van Buuren, 2012
#'@references White, I. R., Royston, P. (2009). Imputing missing covariate
#'values for the Cox model.  \emph{Statistics in Medicine}, \emph{28}(15),
#'1982-1998.
#'
#'Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/sec-toomany.html#a-further-improvement-survival-as-predictor-variable}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#'Chapman & Hall/CRC. Boca Raton, FL.
#'@keywords misc
#'@examples
#'require(MASS)
#'
#'leuk$status <- 1  ## no censoring occurs in leuk data (MASS)
#'ch <- nelsonaalen(leuk, time, status)
#'plot(x = leuk$time, y = ch, ylab='Cumulative hazard', xlab='Time')
#'
#'### See example on http://www.engineeredsoftware.com/lmar/pe_cum_hazard_function.htm
#'time <- c(43, 67, 92, 94, 149, rep(149,7))
#'status <- c(rep(1,5),rep(0,7))
#'eng <- data.frame(time, status)
#'ch <- nelsonaalen(eng, time, status)
#'plot(x = time, y = ch, ylab='Cumulative hazard', xlab='Time')
#'
#'
#'@export
nelsonaalen <- function(data, timevar, statusvar) {
    install.on.demand("survival")
    if (!is.data.frame(data)) 
        stop("Data must be a data frame")
    timevar <- as.character(substitute(timevar))
    statusvar <- as.character(substitute(statusvar))
    time <- data[, timevar]
    status <- data[, statusvar]
    
    hazard <- survival::basehaz(survival::coxph(survival::Surv(time, status) ~ 1))
    idx <- match(time, hazard[, "time"])
    return(hazard[idx, "hazard"])
}
