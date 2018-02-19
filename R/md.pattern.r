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
#'@param plot Should the missing data pattern be made into a plot. Default is 
#'`plot = TRUE`.
#'@param convert.char Should character variables be converted to factors 
#'beforehand? This may have a great impact on the number of missing data patterns. 
#'Default is `convert.char = FALSE`.
#'@param use.empty Should completely unobserved columns count towards the missing 
#'data patterns? Default is `use.empty = TRUE`.
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
md.pattern <- function(x, plot = TRUE, convert.char = FALSE, use.empty = TRUE) {
    # md.pattern
    # 
    # computes the missing data pattern in the data
    # x can be a vector, matrix or data frame
    # NA's indicate missing data
    # based on Schafer's prelim.norm function
    # SvB, 13-7-99
    # SvB, 32 columns bug removed - 8mar2012
    #
    if (!(is.matrix(x) || is.data.frame(x))) 
        stop("Data should be a matrix or dataframe")
    if (ncol(x) < 2) 
        stop("Data should have at least two columns")
    # if(is.data.frame(x)) x <- data.frame.to.matrix(x)
    if (!use.empty){
      if (any(sapply(x, is.character)) & !convert.char){
        x <- x[, !apply(is.na(x), 2, sum) %in% nrow(x)]
        warning('Completely unobserved columns are set to be ignored. Some of the ignored columns are of type `character`.')
      } else {
        x <- x[, !apply(is.na(x), 2, sum) %in% nrow(x)]
        warning('Completely unobserved columns are set to be ignored.')
      }
    }
    if(nrow(x) %in% apply(is.na(x), 2, sum) & use.empty){
      if(!convert.char){
        if (any(sapply(x[, apply(is.na(x), 2, sum) %in% nrow(x)], is.logical))){
          warning('Some columns are completely unobserved and some of those columns are of type `character`. Setting `use.empty = FALSE` and `convert.char = TRUE` may make the missing data pattern easier to interpret.')
        } else {
          warning('Some columns are completely unobserved and are of type `character`. Setting `convert.char = TRUE` may make the missing data pattern easier to interpret.')
        }
      } else {
        warning('Some columns are completely unobserved. Setting `use.empty = FALSE` may make the missing data pattern easier to interpret.')
      }
    }
    if (is.data.frame(x)){
      if (convert.char){
        if(!all(sapply(x, is.numeric))){
          x[!sapply(x, is.numeric)] <- lapply(x[!sapply(x, is.numeric)], factor)
          warning('Columns of class `character` transformed into `factor`')
        }
        x <- data.matrix(x)
      } else {
        x <- suppressWarnings(data.matrix(x))
      }
    }
    n <- nrow(x)
    p <- ncol(x)
    mode(x) <- "single"  # find missingness patterns
    r <- 1 * is.na(x)
    nmis <- as.integer(apply(r, 2, sum))
    names(nmis) <- dimnames(x)[[2]]  # index the missing data patterns
    mdp <- (r %*% (2^((seq_len(ncol(x))) - 1))) + 1  # do row sort  SvB 8mar2012
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
    if (plot){ #add plot
      plot.new()
      R <- r[1:nrow(r)-1, 1:ncol(r)-1]
      par(mar=rep(0, 4))
      plot.window(xlim=c(-1, ncol(R) + 1), ylim=c(-1, nrow(R) + 1), asp=1)
      o <- cbind(c(row(R)), c(col(R))) - 1
      shade <- ifelse(R[nrow(R):1, ], mdc(1), mdc(2))
      rect(o[, 2], o[, 1], o[, 2] + 1, o[, 1] + 1, col=shade)
      for(i in 1:ncol(R)){
        text(i - .5, nrow(R) + .3, colnames(r)[i])
        text(i - .5, -.3, nmis[co][i])
      }
      for(i in 1:nrow(R)){
        text(ncol(R) + .3, i - .5, r[(nrow(r)-1):1, ncol(r)][i])
        text(-.3, i - .5, rownames(r)[(nrow(r)-1):1][i])
      }
      text(ncol(R) + .3,  -.3, r[nrow(r), ncol(r)])
      return(r)
    } else {
      return(r)
    }
}
