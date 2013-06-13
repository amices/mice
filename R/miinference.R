miinference<-function (est, std.err, confidence = 0.95) 
{
  qstar <- est[[1]]
  for (i in 2:length(est)) {
    qstar <- cbind(qstar, est[[i]])
  }
  qbar <- apply(qstar, 1, mean)
  u <- std.err[[1]]
  for (i in 2:length(std.err)) {
    u <- cbind(u, std.err[[i]])
  }
  names(u) <- names(qstar)
  u <- u^2
  ubar <- apply(u, 1, mean)
  bm <- apply(qstar, 1, var)
  m <- dim(qstar)[2]
  tm <- ubar + ((1 + (1/m)) * bm)
  rem <- (1 + (1/m)) * bm/ubar
  nu <- (m - 1) * (1 + (1/rem))^2
  alpha <- 1 - (1 - confidence)/2
  low <- qbar - qt(alpha, nu) * sqrt(tm)
  up <- qbar + qt(alpha, nu) * sqrt(tm)
  pval <- 2 * (1 - pt(abs(qbar/sqrt(tm)), nu))
  fminf <- (rem + 2/(nu + 3))/(rem + 1)
  result <- list(est = qbar, std.err = sqrt(tm),t.value = qbar/sqrt(tm) ,df = nu, p.value = pval, 
                 lower = low, upper = up, r = rem, fminf = fminf)
  result
}