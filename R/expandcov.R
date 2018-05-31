
# ---------------------------expandvcov--------------------------------

expandvcov <- function(q, u) {
    err <- is.na(q)
    return(u)
    ## if (all(!err)) return(u) k <- length(q) v <- names(q) z <- u for (i in 1:ncol(z)){ if (err[i]) { rbind(z[,],NA,z[,]) j
    ## <- j + 1 up <- } j <- j + 1 z[i,] <- u[j,] z[,i] <- u[,j] }
    
    ## z <- matrix(NA, ncol=k, nrow=k, dimnames = list(v,v)) idx <- (is.na()) j <- 0 for (i in 1:k){ if (err[i]) next j <- j
    ## + 1 z[i,] <- u[j,] z[,i] <- u[,j] } return(z)
}
