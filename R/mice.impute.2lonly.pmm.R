### contributed by Alexander Robitzsch (robitzsch@ipn.uni-kiel.de)

#' Imputation at level 2 by predictive mean matching
#'
#' Imputes univariate missing data at level 2 using predictive mean matching.
#' Variables are level 1 are aggregated at level 2. The group identifier at
#' level 2 must be indicated by \code{type = -2} in the \code{predictorMatrix}.
#'
#' @aliases 2lonly.pmm
#' @inheritParams mice.impute.pmm
#' @param type Group identifier must be specified by '-2'. Predictors must be
#' specified by '1'.
#' @param ... Other named arguments.
#' @return A vector of length \code{nmis} with imputations.
#' @author Alexander Robitzsch (IPN - Leibniz Institute for Science and 
#' Mathematics Education, Kiel, Germany), \email{robitzsch@@ipn.uni-kiel.de},
#' plus some tweaks by Stef van Buuren
#' @seealso \code{\link{mice.impute.pmm}},
#' \code{\link{mice.impute.2lonly.norm}}, \code{\link{mice.impute.2l.pan}},
#' \code{\link{mice.impute.2lonly.mean}}
#' @details
#' This function allows in combination with \code{\link{mice.impute.2l.pan}}
#' switching regression imputation between level 1 and level 2 as described in
#' Yucel (2008) or Gelman and Hill (2007, p. 541).
#' 
#' The function checks for partial missing level-2 data. Level-2 data 
#' are assumed to be constant within the same cluster. If one or more
#' entries are missing, then the procedure aborts with an error 
#' message that identifies the cluster with incomplete level-2 data.
#' In such cases, one may first fill in the cluster mean (or mode) by 
#' the \code{2lonly.mean} method to remove inconsistencies.
#' @references Gelman, A. and Hill, J. (2007). \emph{Data analysis using
#' regression and multilevel/hierarchical models}. Cambridge, Cambridge
#' University Press.
#'
#' Yucel, RM (2008). Multiple imputation inference for multivariate multilevel
#' continuous data with ignorable non-response.  \emph{Philosophical
#' Transactions of the Royal Society A}, \bold{366}, 2389-2404.
#' 
#' Van Buuren, S. (2018). 
#' \href{https://stefvanbuuren.name/fimd/sec-level2pred.html}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' 
#' @note The extension to categorical variables transform 
#' a dependent factor variable by means of the \code{as.integer()}
#' function. This may make sense for categories that are 
#' approximately ordered, but less so for pure nominal measures.
#' 
#' For a more general approach, see 
#' \code{miceadds::mice.impute.2lonly.function()}.
#' @family univariate-2lonly
#' @examples
#'
#'##################################################
#'# simulate some data
#'# x,y ... level 1 variables
#'# v,w ... level 2 variables
#'
#'G <- 250            # number of groups
#'n <- 20             # number of persons
#'beta <- .3          # regression coefficient
#'rho <- .30          # residual intraclass correlation
#'rho.miss <- .10     # correlation with missing response
#'missrate <- .50     # missing proportion
#'y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho )) 
#'w <- rep( round( rnorm(G ) , 2 ) , each=n )
#'v <- rep( round( runif( G , 0 , 3 ) ) , each=n )
#'x <-  rnorm( G*n ) 
#'y <- y1 + beta  * x + .2 * w + .1 * v
#'dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y , "w" = w , "v" = v )
#'dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , "y" ] <- NA
#'dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "w" ] <- NA
#'dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "v" ] <- NA
#'
#'#....
#'# empty mice imputation
#'imp0 <- mice( as.matrix(dfr)  , maxit=0 )
#'predM <- imp0$predictorMatrix
#'impM <- imp0$method
#'
#'#...
#'# multilevel imputation
#'predM1 <- predM
#'predM1[c("w","y","v"),"group"] <- -2
#'predM1["y","x"] <- 1        # fixed x effects imputation
#'impM1 <- impM
#'impM1[c("y","w","v")] <- c("2l.pan" , "2lonly.norm" , "2lonly.pmm" )
#'
#'# turn v into a categorical variable
#'dfr$v <- as.factor(dfr$v)
#'levels(dfr$v) <- LETTERS[1:4]
#'
#'# y ... imputation using pan
#'# w ... imputation at level 2 using norm
#'# v ... imputation at level 2 using pmm
#'
#' # skip imputation on solaris
#' is.solaris <- function() grepl('SunOS',Sys.info()['sysname'])
#' if (!is.solaris()) {
#' imp <- mice(dfr, m = 1, predictorMatrix = predM1 , 
#'            method = impM1, maxit = 1, paniter = 500)
#' }
#'@export
mice.impute.2lonly.pmm <- function (y, ry, x, type, wy = NULL, ...) {
  .imputation.level2(y = y, ry = ry, x = x, type = type, wy = wy, 
                     method = "pmm", ... )
}

#******************************************
# imputation function at level 2
# can be done with norm and pmm
.imputation.level2 <- function(y, ry, x, type, wy, method, ... ){
  if (sum(type == -2L) != 1L) stop( "No class variable")
  if (is.null(wy)) wy <- !ry
  
  # handle categorical data
  if (is.factor(y)) y <- as.integer(y)
  
  # extract cluster index
  clusterx <- x[, type == -2L]
  # clusters with one or more missing y's
  cm <- unique(clusterx[!ry])
  # clusters with one or more observed y's
  co <- unique(clusterx[ry])
  # cluster where all y's are observed
  cobs <- setdiff(co, cm)
  # clusters where some y's are missing
  csom <- intersect(co, cm)
  
  if (length(csom) > 0L) 
    stop(paste0("Method 2lonly.", method, " found the following clusters with partially missing\n", 
    "  level-2 data: ", paste(csom, collapse = ", "), "\n",
    "  Method 2lonly.mean can fix such inconsistencies."))
  
  # calculate aggregated values
  x <- cbind(1, as.matrix(x[, type %in% c(1L, 2L)]))
  a2 <- rowsum(cbind(x, y), clusterx, na.rm = TRUE)
  a2 <- a2 / rowsum(1 * cbind(!is.na(x), ry), clusterx)
  
  clusterx0 <- as.numeric(paste0(rownames(a2)))
  a1 <- cbind(clusterx0, a2)
  
  ry2 <- a1[, 1L] %in% cobs
  wy2 <- !(a1[, 1L] %in% unique(clusterx[!wy]))
  y2 <- a1[, ncol(a1)]
  x2 <- as.matrix(a1[, -c(1L:2L, ncol(a1))])
  
  # norm imputation at level 2
  if (method == "norm")
    ximp2 <- mice.impute.norm(y = y2, ry = ry2, x = x2, 
                              wy = wy2, ...)
  
  # pmm imputation at level 2
  if (method == "pmm")
    ximp2 <- mice.impute.pmm(y = y2, ry = ry2, x = x2, 
                             wy = wy2, ...) 
  
  # expland to full matrix
  cly2 <- a1[wy2, 1L]
  i1 <- match(clusterx, cly2)
  ximp <- (ximp2[i1])[wy]
  ximp
}
