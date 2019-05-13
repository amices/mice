initialize.chain <- function(blocks, maxit, m) {
  vars <- unique(unlist(blocks))
  chain <- array(NA, dim = c(length(vars), maxit, m))
  dimnames(chain) <- list(vars, 
                          seq_len(maxit), 
                          paste("Chain", seq_len(m)))
  chain
}