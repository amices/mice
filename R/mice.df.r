mice.df <- function(m, lambda, dfcom = 99999, method = "smallsample") {
    lambda[lambda < 1e-04] <- 1e-04
    dfold <- (m - 1) / lambda ^ 2
    if (method != "smallsample") return(dfold)
    dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    dfold * dfobs / (dfold + dfobs)
}
