initialize.chain <- function(varnames, maxit, m) {
  chain <- array(NA, dim = c(length(varnames), maxit, m))
  dimnames(chain) <- list(
    varnames,
    seq_len(maxit),
    paste("Chain", seq_len(m))
  )
  chain
}
