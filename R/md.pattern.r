# ------------------------------MD.PATTERN-------------------------------

#'Missing data pattern
#'
#'Display missing-data patterns.
#'
#'This function is useful for investigating any structure of missing
#'observation in the data. In specific case, the missing data pattern could be
#'(nearly) monotone. Monotonicity can be used to simplify the imputation model.
#'See Schafer (1997) for details. Also, the missing pattern could suggest which
#'variables could potentially be useful for imputation of missing entries.
#'
#'@param x A data frame or a matrix containing the incomplete data.  Missing
#'values are coded as NA's.
#'@return A matrix with \code{ncol(x)+1} columns, in which each row corresponds
#'to a missing data pattern (1=observed, 0=missing).  Rows and columns are
#'sorted in increasing amounts of missing information. The last column and row
#'contain row and column counts, respectively.
#'@author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#'@references Schafer, J.L. (1997), Analysis of multivariate incomplete data.
#'London: Chapman&Hall.
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
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
md.pattern <- function(x) {
    # md.pattern
    # 
    # computes the missing data pattern in the data
    # x can be a vector, matrix or data frame
    # NA's indicate missing data
    # based on Schafer's prelim.norm function
    # SvB, 13-7-99
    # SvB, 32 columns bug removed - 8mar2012
    #
    if (!(is.matrix(x) | is.data.frame(x))) 
        stop("Data should be a matrix or dataframe")
    if (ncol(x) < 2) 
        stop("Data should have at least two columns")
    # if(is.data.frame(x)) x <- data.frame.to.matrix(x)
    if (is.data.frame(x)) 
        x <- data.matrix(x)  # SvB use standard R function > V2.5
    n <- nrow(x)
    p <- ncol(x)
    mode(x) <- "single"  # find missingness patterns
    r <- 1 * is.na(x)
    nmis <- as.integer(apply(r, 2, sum))
    names(nmis) <- dimnames(x)[[2]]  # index the missing data patterns
    mdp <- (r %*% (2^((1:ncol(x)) - 1))) + 1  # do row sort  SvB 8mar2012
    ro <- order(mdp)
    x <- matrix(x[ro, ], n, p)  ##pm 04/02
    mdp <- mdp[ro]
    r <- matrix(r[ro, ], n, p)  ##pm 04/02
    ro <- order(ro)  # compress missing data patterns
    mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
    mdp <- unique(mdp)
    npatt <- length(mdpst)  # create r-matrix for display purposes
    r <- 1 - r
    r <- matrix(r[mdpst, ], npatt, p)
    if (npatt == 1) 
        tmp <- format(n)
    if (npatt > 1) 
        tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
    dimnames(r) <- list(tmp, dimnames(x)[[2]])
    storage.mode(r) <- "integer"  # center and scale the columns of x
    #
    if (npatt > 1) 
        nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
    if (npatt == 1) 
        nmdp <- n  #
    # sort the rows and columns according to the marginals
    co <- order(nmis)
    ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))
    r <- rbind(r[ro2, co], nmis[co])
    r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
    r
}
