### contributed by Alexander Robitzsch (robitzsch@ipn.uni-kiel.de)

#-------------------MICE.IMPUTE.2L.PAN----------------------------
# Usage is an extension of 2l.norm
# -2 ... group identifier
#  1 ... fixed effects
#  2 ... fixed and random effects
#  3 ... introduce aggregated effects (i.e. group means)
#  4 ... fixed, random and aggregated effects


#'Imputation by a two-level normal model using \code{pan}
#'
#'Imputes univariate missing data using a two-level normal model with
#'homogeneous within group variances. Aggregated group effects (i.e. group
#'means) can be automatically created and included as predictors in the
#'two-level regression (see argument \code{type}). This function needs the
#'\code{pan} package.
#'
#'Implements the Gibbs sampler for the linear two-level model with homogeneous
#'within group variances which is a special case of a multivariate linear mixed
#'effects model (Schafer & Yucel, 2002).  For a two-level imputation with
#'heterogeneous within-group variances see \code{\link{mice.impute.2l.norm}}. %
#'The random intercept is automatically added in %
#'\code{mice.impute.2l.norm()}.
#'
#'@aliases mice.impute.2l.pan 2l.pan
#'@name mice.impute.2l.pan
#'@param y Incomplete data vector of length \code{n}
#'@param ry Vector of missing data pattern (\code{FALSE}=missing,
#'\code{TRUE}=observed)
#'@param x Matrix (\code{n} x \code{p}) of complete covariates.
#'@param type Vector of length \code{ncol(x)} identifying random and class
#'variables.  Random effects are identified by a '2'. The group variable (only
#'one is allowed) is coded as '-2'. Random effects also include the fixed
#'effect. If for a covariates X1 group means shall be calculated and included
#'as further fixed effects choose '3'. In addition to the effects in '3',
#'specification '4' also includes random effects of X1.
#'@param intercept Logical determining whether the intercept is automatically
#'added.
#'@param paniter Number of iterations in \code{pan}. Default is 500.
#'@param groupcenter.slope If \code{TRUE}, in case of group means (\code{type}
#'is '3' or'4') group mean centering for these predictors are conducted before
#'doing imputations. Default is \code{FALSE}.
#'@param ... Other named arguments.
#'@return A vector of length \code{nmis} with imputations.
#'@author Alexander Robitzsch (IPN - Leibniz Institute for Science and 
#'Mathematics Education, Kiel, Germany), \email{robitzsch@@ipn.uni-kiel.de}.
#'@note This function does not implement the \code{where} functionality. It 
#'always produces \code{nmis} imputation, irrespective of the \code{where} 
#'argument of the \code{mice} function.
#'@family univariate \code{2l} functions
#'@references
#'
#'Schafer J L, Yucel RM (2002). Computational strategies for multivariate
#'linear mixed-effects models with missing values.  \emph{Journal of
#'Computational and Graphical Statistics}. \bold{11}, 437-457.
#'
#'Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'@examples
#'
#'###################################
#'# simulate some data
#'# two-level regression model with fixed slope
#'
#'# number of groups
#'G <- 250
#'# number of persons
#'n <- 20
#'# regression parameter
#'beta <- .3
#'# intraclass correlation
#'rho <- .30
#'# correlation with missing response
#'rho.miss <- .10
#'# missing proportion
#'missrate <- .50
#'y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho )) 
#'x <-  rnorm( G*n )
#'y <- y1 + beta  * x
#'dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y )
#'dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , "y" ] <- NA
#'
#'#.....
#'# empty imputation in mice
#'imp0 <- mice( as.matrix(dfr)  , maxit=0 )
#'predM <- imp0$predictorMatrix
#'impM <- imp0$method
#'
#'#...
#'# specify predictor matrix and imputationMethod
#'predM1 <- predM
#'predM1["y","group"] <- -2
#'predM1["y","x"] <- 1        # fixed x effects imputation
#'impM1 <- impM
#'impM1["y"] <- "2l.pan"
#'
#'# multilevel imputation
#'imp1 <- mice( as.matrix( dfr ) , m = 1 , predictorMatrix = predM1 , 
#'            imputationMethod = impM1 , maxit=1 )
#'# multilevel analysis
#'library(lme4)
#'mod <- lmer( y ~ ( 1 + x | group) + x , data = complete(imp1) )
#'summary(mod)
#'
#'############################################
#'# Examples of predictorMatrix specification
#'
#'# random x effects
#'# predM1["y","x"] <- 2
#'
#'# fixed x effects and group mean of x
#'# predM1["y","x"] <- 3        
#'
#'# random x effects and group mean of x
#'# predM1["y","x"] <- 4        
#'
#'@export
mice.impute.2l.pan <- function(y, ry, x, type, intercept=TRUE, paniter = 500 , 
                               groupcenter.slope = FALSE , ...){
    if (!requireNamespace("pan", quietly = TRUE))
        stop("Package 'pan' needed fo this function 
             to work. Please install it.", 
             call. = FALSE)
    ## append intercept
    if (intercept) {
        x <- cbind(1, as.matrix(x))
        type <- c(2, type)
    }
        
    # add groupmeans in the regression model
    if (any(type %in% c(3,4))) { 
        x0 <- as.matrix(cbind( x[ , type == -2  ]  , x[ , type %in% c(3,4) ]  ))
        colnames(x0) <- c( colnames(x)[ type==-2] , colnames(x)[ type %in% c(3,4) ] )
        type0 <- c( -2 , rep.int(1 , ncol(x0)-1) )
        x0.aggr <- as.matrix( .mice.impute.2l.groupmean(y = y , ry=ry , x = x0 , 
                                                        type = type0  , grmeanwarning=FALSE , ...)   )
        colnames(x0.aggr) <- paste0( "M." , colnames(x0)[-1])
        # groupcentering
        if ( groupcenter.slope ){ 
            x0.aggr1 <- as.matrix(x0.aggr)
            colnames(x0.aggr1) <- colnames(x0)[-1]
            x0cent <- x0[,-1] - x0.aggr1
            x[ , colnames(x0cent) ] <- x0cent
        }
        # combine covariate matrix
        x <- cbind( x , x0.aggr )
        # add type
        type1 <- c( type , rep.int(1 , ncol(x0.aggr) ) )
        names(type1) <- c( names(type) , colnames(x0.aggr) )   
        type1[ type1 == 3 ] <- 1
        type1[ type1 == 4 ] <- 2
        type <- type1
    }	
    #############################
    # pan imputation
    # define cluster
    group <- x[ , type == -2 ]
    subj <- match( group , unique(group) )
    # is group resorting necessary? (need this for pan)
    sortgroups <- any( diff(subj) < 0 )
    if ( sortgroups ){ 
        dfr <- data.frame( "group" = group , "ry" = ry , 
                           "index" = seq(1,length(ry)) )
        dfr <- dfr[ order(dfr$group) , ]
        group <- group[ dfr$index ]
        y <- y[ dfr$index ]
        x <- x[ dfr$index , ]
        ry <- ry[ dfr$index ]
        subj <- subj[ dfr$index ]
        #			stop( "Sort group identifiers in increasing order!\n")
    }
    y1 <- matrix( as.numeric(y) , ncol=1 )
    y1[ ! ry , 1 ] <- NA
    # specify predictors
    pred <-  x[ , type != -2 , drop = FALSE] ## fixed SvB 1feb2013
    # columns fixed effects
    xcol <- seq( 1 , ncol(pred) )
    type1 <- type[ type != -2 ]
    zcol <- which(type1 == 2 )
    # noninformative priors
    prior <- list( a=ncol(y1),   Binv= diag( rep(1,ncol(y1) ) ) , 
                   c= ncol(y1) * length(zcol) ,  Dinv= diag( rep(1 ,ncol(y1)*length(zcol) ) )     )
    
    if (length(subj) != nrow(y1)) stop("No class variable")     ## fixed SvB 27apr2013

    # pan imputation
    ii <- 0
    while (ii == 0){ 
        s1 <- round(runif(1, 1,10^7))
        imput <-  pan::pan(y1,subj,pred,xcol,zcol,prior,seed= s1 ,iter= paniter )
        res <- imput$y
        ii <- 1 - any( is.na( res ) )	
        # check for invalid imputations: pan occasionally produces NaNs
    }
    if ( sortgroups ){ 
        dfr <- cbind( res , dfr )
        dfr <- dfr[ order(dfr$index ) , ]
        res <- dfr[ ! dfr$ry , "res" ]
    } else { 	res <- res[ ! ry ] }
    flush.console()
    return(res)
}


###########################################################################################
# compute cluster groupmean
.mice.impute.2l.groupmean <- function (y, ry, x, type , grmeanwarning=TRUE, ...){  
    if ( ( ncol(x) > 2 ) & grmeanwarning )   warning("\nMore than one variable is requested to be aggregated.\n")    
    # calculate aggregated values
    a1 <- aggregate( x[, type %in% c(1,2) ] , list( x[,type == -2] ) , mean , na.rm=TRUE)
    i1 <- match( x[,type == -2] , a1[,1] )
    ximp <- as.matrix(a1[i1,-1])
    colnames(ximp) <- paste( names(type)[ type %in% c(1,2) ] , names(type)[ type == -2 ] , sep="." )
    return(ximp)
}
###########################################################################################
