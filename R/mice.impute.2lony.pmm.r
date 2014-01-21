### contributed by Alexander Robitzsch (a.robitzsch@bifie.at)

#'Imputation at level 2 by predictive mean matching
#'
#'Imputes univariate missing data at level 2 using predictive mean matching.
#'Variables are level 1 are aggregated at level 2. The group identifier at
#'level 2 must be indicated by \code{type=-2} in the \code{predictorMatrix}.
#'
#'This function allows in combination with \code{\link{mice.impute.2l.pan}}
#'switching regression imputation between level 1 and level 2 as described in
#'Yucel (2008) or Gelman and Hill (2007, p. 541).
#'
#'@aliases mice.impute.2lonly.pmm 2lonly.pmm
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates. Only numeric
#'variables are permitted for usage of this function.
#'@param type Group identifier must be specified by '-2'. Predictors must be
#'specified by '1'.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Alexander Robitzsch (Federal Institute for Education Research,
#'Innovation, and Development of the Austrian School System, Salzburg,
#'Austria), \email{a.robitzsch@@bifie.at}
#'@seealso \code{\link{mice.impute.pmm}},
#'\code{\link{mice.impute.2lonly.norm}}, \code{\link{mice.impute.2l.pan}}
#'@references Gelman, A. and Hill, J. (2007). \emph{Data analysis using
#'regression and multilevel/hierarchical models}. Cambridge, Cambridge
#'University Press.
#'
#'Yucel, RM (2008). Multiple imputation inference for multivariate multilevel
#'continuous data with ignorable non-response.  \emph{Philosophical
#'Transactions of the Royal Society A}, \bold{366}, 2389-2404.
#'@examples
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
#'# y ... imputation using pan
#'# w ... imputation at level 2 using norm
#'# v ... imputation at level 2 using pmm
#'
#'imp1 <- mice( as.matrix( dfr ) , m = 1 , predictorMatrix = predM1 , 
#'            imputationMethod = impM1 , maxit=1 , paniter=500)
#'    
#'@export
mice.impute.2lonly.pmm <- function (y, ry, x, type , ...){
    imp <- .imputation.level2( y = y , ry = ry , x = x , type = type , 
                               imputationMethod = "pmm" , ... )
}

#******************************************
# imputation function at level 2
# can be done with norm and pmm
.imputation.level2 <- function( y , ry , x , type , imputationMethod , ... ){
    if ( sum(type==-2 ) != 1 ){
        stop( "No class variable")
    }
    # extract cluster index
    clusterx <- x[,type == -2 ]
    x <- cbind(1, as.matrix(x[,type %in% c(1,2)]))      # calculate aggregated values
    # change ARb 2013-02-12
    # a1 <- aggregate( (cbind(x,y)) , list( clusterx ) , mean , na.rm=F)
    a2 <- rowsum( cbind(x,y) , clusterx , na.rm=FALSE)
    a2 <- a2 / rowsum( 1+0*y , clusterx , na.rm=FALSE )[,1] 
    a1 <- cbind( clusterx  , a2 )
    #*****	
    N1 <- ncol(a1)
    cly2 <- unique( clusterx[  ry ] )  # clusters without missings on y
    ry2 <- a1[,1] %in% cly2  
    x1 <- as.matrix(a1[, -c(1,N1)])
    # norm imputation at level 2
    if ( imputationMethod == "norm" ){ 
        ximp2 <- mice.impute.norm( y= as.matrix(a1[,N1]), ry=ry2, x = x1[,-1] , ...) 
    }
    # pmm imputation at level 2
    if ( imputationMethod == "pmm" ){ 
        ximp2 <- mice.impute.pmm( y= as.matrix(a1[,N1]), ry=ry2, x = x1[,-1] , ...) 
    }
    # data postprocessing
    cly2 <- a1[ ! ry2 , 1] 
    i1 <- match( clusterx, cly2 )
    ximp <- ( ximp2[i1] )[ ! ry ]
    return(ximp)	
}
