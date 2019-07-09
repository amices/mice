# ------------------------------MD.PATTERN-------------------------------

#'Missing data pattern
#'
#'Display missing-data patterns.
#'
#'This function is useful for investigating any structure of missing
#'observations in the data. In specific case, the missing data pattern could be
#'(nearly) monotone. Monotonicity can be used to simplify the imputation model.
#'See Schafer (1997) for details. Also, the missing pattern could suggest which
#'variables could potentially be useful for imputation of missing entries.
#'
#'@param x A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as NA's.
#'@param plot Should the missing data pattern be made into a plot. Default is 
#'`plot = TRUE`.
#'@param rotate.names Whether the variable names in the plot should be placed 
#'horizontally or vertically. Default is `rotate.names = FALSE`.
#'@return A matrix with \code{ncol(x)+1} columns, in which each row corresponds
#'to a missing data pattern (1=observed, 0=missing).  Rows and columns are
#'sorted in increasing amounts of missing information. The last column and row
#'contain row and column counts, respectively.
#'@author Gerko Vink, 2018, based on an earlier version of the same function by
#'Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@references Schafer, J.L. (1997), Analysis of multivariate incomplete data.
#'London: Chapman&Hall.
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'@keywords univar
#'@examples
#'
#'
#'md.pattern(nhanes)
#'#     age hyp bmi chl
#'#  13   1   1   1   1  0
#'#   1   1   1   0   1  1
#'#   3   1   1   1   0  1
#'#   1   1   0   0   1  2
#'#   7   1   0   0   0  3
#'#   0   8   9  10 27
#'
#'
#'@export
md.pattern <- function(x, plot = TRUE, rotate.names = FALSE){
  if (!(is.matrix(x) || is.data.frame(x))) 
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2) 
    stop("Data should have at least two columns")
  R <- is.na(x)
  nmis <- colSums(R)
  R <- matrix(R[, order(nmis)], dim(x)) #sort columnwise
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse=''))
  sortR <- matrix(R[order(pat), ], dim(x)) #sort rowwise
  if (nrow(x) == 1){
    mpat <- is.na(x)
  } else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  #update row and column margins
  if (all(!is.na(x))){
    cat(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==") 
    cat("  No need for mice. This data set is completely observed.\n")
    cat(" \\  \\|/  /\n  `-----'\n\n")
    mpat <- t(as.matrix(mpat, byrow = TRUE))
    rownames(mpat) <- table(pat)
  } else {
    if(is.null(dim(mpat))){
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))
  if (plot){ #add plot
    plot.new()
    if (is.null(dim(sortR[!duplicated(sortR), ]))){
      R <- t(as.matrix(r[1:nrow(r)-1, 1:ncol(r)-1]))
    } else {
      if(is.null(dim(R))){
        R <- t(as.matrix(R))
      }
      R <- r[1:nrow(r)-1, 1:ncol(r)-1]
    }
    op <- par(mar = rep(0, 4))
    on.exit(par(op))
    if (rotate.names) {
      adj = c(0, 0.5)
      srt = 90
      length_of_longest_colname = max(nchar(colnames(r)))/2.6
      plot.window(xlim = c(-1, ncol(R) + 1), 
                  ylim = c(-1, nrow(R) + length_of_longest_colname), 
                  asp = 1)
    } else {
      adj = c(0.5, 0)
      srt = 0
      plot.window(xlim = c(-1, ncol(R) + 1), 
                  ylim = c(-1, nrow(R) + 1), 
                  asp = 1)
    }
    M <- cbind(c(row(R)), c(col(R))) - 1
    shade <- ifelse(R[nrow(R):1, ], mdc(1), mdc(2))
    rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
    for(i in 1:ncol(R)) {
      text(i - .5, nrow(R) + .3, colnames(r)[i], adj = adj, srt = srt)
      text(i - .5, -.3, nmis[order(nmis)][i])
    }
    for(i in 1:nrow(R)){
      text(ncol(R) + .3, i - .5, r[(nrow(r)-1):1, ncol(r)][i], adj = 0)
      text(-.3, i - .5, rownames(r)[(nrow(r)-1):1][i], adj = 1)
    }
    text(ncol(R) + .3,  -.3, r[nrow(r), ncol(r)])
    return(r)
  } else {
    return(r)
  }
}
