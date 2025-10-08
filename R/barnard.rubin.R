barnard.rubin <- function(m, b, t, dfcom = Inf) {
  lambda <- (1 + 1 / m) * b / t
  dfold <- (m - 1) / (lambda^2)
  if (is.infinite(dfcom)) {
    return(dfold)
  }
  tmp <- (1 - lambda) * (1 + dfcom) * dfcom
  df_br <- (m - 1) * tmp / ((dfcom + 3) * (m - 1) + (lambda^2) * tmp)
  df_br
}
