### contributed by Alexander Robitzsch (a.robitzsch@bifie.at)

#-------------------MICE.IMPUTE.2L.PAN----------------------------
# Usage is an extension of 2l.norm
# -2 ... group identifier
#  1 ... fixed effects
#  2 ... fixed and random effects
#  3 ... introduce aggregated effects (i.e. group means)
#  4 ... fixed, random and aggregated effects
mice.impute.2l.pan <- function(y, ry, x, type, intercept=TRUE, paniter = 500 , 
				groupcenter.slope = FALSE , ...){
	  require(pan)
	  ## append intercept
	  if (intercept) {
		x <- cbind(1, as.matrix(x))
		type <- c(2, type)
			}
	# add groupmeans in the regression model
    if ( sum( type %in% c(3,4)) > 0 ){ 
            x0 <- as.matrix(cbind( x[ , type == -2  ]  , x[ , type %in% c(3,4) ]  ))
            colnames(x0) <- c( colnames(x)[ type==-2] , colnames(x)[ type %in% c(3,4) ] )
            type0 <- c( -2 , rep(1 , ncol(x0)-1) )
            x0.aggr <- as.matrix( .mice.impute.2l.groupmean(y = y , ry=ry , x = x0 , 
								type = type0  , grmeanwarning=FALSE , ...)   )
            colnames(x0.aggr) <- paste( "M." , colnames(x0)[-1] , sep="")
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
            type1 <- c( type , rep(1 , ncol(x0.aggr) ) )
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
	# pan imputation
	ii <- 0
	while (ii == 0){ 
		s1 <- round(runif(1, 1,10^7))
		imput <-  pan(y1,subj,pred,xcol,zcol,prior,seed= s1 ,iter= paniter )
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

mice.impute.2L.pan <- mice.impute.2l.pan   # for compatibility


###########################################################################################
# compute cluster groupmean
.mice.impute.2l.groupmean <- function (y, ry, x, type , grmeanwarning=TRUE, ...){  
    if ( ( ncol(x) > 2 ) & grmeanwarning )   warning("\nMore than one variable is requested to be aggregated.\n")    
    # calculate aggregated values
    a1 <- aggregate( x[, type %in% c(1,2) ] , list( x[,type == -2] ) , mean , na.rm=T)
    i1 <- match( x[,type == -2] , a1[,1] )
    ximp <- as.matrix(a1[i1,-1])
	colnames(ximp) <- paste( names(type)[ type %in% c(1,2) ] , names(type)[ type == -2 ] , sep="." )
    return(ximp)
}
###########################################################################################

#******************************************
# imputation function at level 2
# can be done with norm and pmm
.imputation.level2 <- function( y , ry , x , type , imputationMethod , ... ){
	if ( sum(type==-2 ) != 1 ){
	stop( "define for 2lonly imputation one and only grouping variable with type=-2")
			}
   # extract cluster index
   clusterx <- x[,type == -2 ]
   x <- cbind(1, as.matrix(x[,type %in% c(1,2)]))      # calculate aggregated values
   a1 <- aggregate( (cbind(x,y)) , list( clusterx ) , mean , na.rm=F)
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
#******************************************
		
###################################################################################
# norm imputation at level 2
mice.impute.2lonly.norm <- function (y, ry, x, type , ...){
	imp <- .imputation.level2( y = y , ry = ry , x = x , type = type , 
				imputationMethod = "norm" , ... )
	return(imp)
	    }
###########################################################################################

###################################################################################
# norm imputation at level 2
mice.impute.2lonly.pmm <- function (y, ry, x, type , ...){
	imp <- .imputation.level2( y = y , ry = ry , x = x , type = type , 
				imputationMethod = "pmm" , ... )
	    }
###########################################################################################


