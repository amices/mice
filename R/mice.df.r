
# ------------------------------mice.df--------------------------------

mice.df <- function(m, lambda, dfcom, method) {
    if (is.null(dfcom)) {
        dfcom <- 999999
        warning("Large sample assumed.")
    }
    lambda[lambda < 1e-04] <- 1e-04
    dfold <- (m - 1)/lambda^2
    dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
    df <- dfold * dfobs/(dfold + dfobs)
    if (method != "smallsample") 
        df <- dfold  ## Rubin 1987, 3.1.6, Van Buuren 2012, 2.30, added 31/10/2012
    return(df)
}
