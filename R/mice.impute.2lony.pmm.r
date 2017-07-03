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
#'@inheritParams mice.impute.pmm
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
#'@note The extension to categorical variables transform 
#'a dependent factor variable by means of the \code{as.integer()}
#'function. This may make sense for categories that are 
#'approximately ordered, and less so for pure nominal measures.
#'@family univariate \code{2lonly} functions
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
#'# turn v into a categorical variable
#'dfr$v <- as.factor(dfr$v)
#'levels(dfr$v) <- LETTERS[1:4]
#'
#'# y ... imputation using pan
#'# w ... imputation at level 2 using norm
#'# v ... imputation at level 2 using pmm
#'
#'imp <- mice(dfr, m = 1, predictorMatrix = predM1 , 
#'            imputationMethod = impM1, maxit = 1, paniter = 500)
#'    
#'@export
mice.impute.2lonly.pmm <- function (y, ry, x, type, wy = NULL, ...){
  imp <- .imputation.level2( y = y , ry = ry , x = x , type = type, wy = wy, 
                             imputationMethod = "pmm" , ... )				   
}

#******************************************
# imputation function at level 2
# can be done with norm and pmm
.imputation.level2 <- function( y , ry , x , type, wy, imputationMethod , ... ){
  if ( sum(type==-2 ) != 1 ){
    stop( "No class variable")
  }

  if (is.null(wy)) wy <- !ry

  # handle categorical data
  ynum <- y
  if (is.factor(y)) ynum <- as.integer(y)
  
  # extract cluster index
  clusterx <- x[, type == -2]
  x <- cbind(1, as.matrix(x[,type %in% c(1,2)]))      # calculate aggregated values	
  # change ARb 2013-02-12
  # a1 <- aggregate( (cbind(x,y)) , list( clusterx ) , mean , na.rm=F)
  a2 <- rowsum( cbind(x, ynum) , clusterx , na.rm=FALSE)
  #~~~~~
  # change ARb 2014-02-18
  clusterx0 <- as.numeric(paste0(rownames(a2)))
  a2 <- a2 / rowsum( 1+0*ynum , clusterx , na.rm=FALSE )[,1] 
  a1 <- cbind( clusterx0  , a2 )
  #~~~~~
  #*****	
  N1 <- ncol(a1)
  cly2 <- unique( clusterx[ry ] )  # clusters without missings on y
  ry2 <- a1[, 1] %in% cly2
  wly2 <- unique( clusterx[wy ] )  # clusters without missings on y
  wy2 <- a1[, 1] %in% wly2
  x1 <- as.matrix(a1[, -c(1, N1)])
  
  # norm imputation at level 2
  if ( imputationMethod == "norm" ) { 
    ximp2 <- mice.impute.norm( y= as.matrix(a1[,N1]), ry=ry2, x = x1[,-1] , 
                               wy = wy2, ...)
    
    # data postprocessing
    cly2 <- a1[ ! ry2 , 1] 
    i1 <- match( clusterx, cly2 )
    ximp <- ( ximp2[i1] )[ wy ]
    return(ximp)
  }
  
  # pmm imputation at level 2
  if ( imputationMethod == "pmm" ){ 
    ximp2 <- mice.impute.pmm(y = a1[, N1], ry = ry2, x = x1[, -1], 
                             wy = wy2, ...) 
    
    # aggregate y to second level group
    splity <- split(y, f = clusterx)
    y2 <- unlist(lapply(splity, function(x) return(x[1])))
    
    # find donor values: use only the indices of second-levels donors
    yimp2 <- y2[names(ximp2)]
    
    # expland to full matrix
    cly2 <- a1[ !ry2 , 1] 
    i1 <- match( clusterx, cly2 )
    yimp <- (yimp2[ i1 ])[ wy ]
    
    return(yimp)
  }
}
