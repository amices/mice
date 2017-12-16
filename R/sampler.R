sampler <- function(data, m, where, imp, setup, fromto, printFlag, ...)
  # The sampler controls the actual Gibbs sampling iteration scheme.
  # This function is called by mice and mice.mids
  # Authors: S van Buuren, K Groothuis-Oudshoorn
{
  blocks <- setup$blocks
  method <- setup$method
  visitSequence <- setup$visitSequence
  predictorMatrix <- setup$predictorMatrix
  formula <- setup$formula
  post <- setup$post
  
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)
  
  # set up array for convergence checking
  chainMean <- chainVar <- initialize.chain(blocks, maxit, m)
  
  ## THE MAIN LOOP: GIBBS SAMPLER ##
  if (maxit < 1) iteration <- 0
  if (maxit >= 1) {
    if (printFlag)
      cat("\n iter imp variable")
    for (k in from:to) {
      # begin k loop : main iteration loop
      iteration <- k
      for (i in seq_len(m)) {
        # begin i loop: repeated imputation loop
        if (printFlag)
          cat("\n ", iteration, " ", i)
        
        # prepare the i'th imputation
        # do not overwrite any observed data
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            y <- data[, j]
            ry <- r[, j]
            wy <- where[, j]
            data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
          }
        }
        
        # impute block-by-block - type
        for (h in visitSequence) {
          
          type <- predictorMatrix[h, ]
          predictors <- names(type)[type != 0]
          ff <- extend.formula(formula = ~ 0, predictors = predictors, ...)
          
          theMethod <- method[h]
          empt <- theMethod == ""
          univ <- !empt && !is.passive(theMethod) && 
            !handles.format(paste0("mice.impute.", theMethod))
          mult <- !empt && !is.passive(theMethod) && 
            handles.format(paste0("mice.impute.", theMethod))
          pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 1
          
          b <- blocks[[h]]
          bname <- names(blocks)[h]
          
          ## store current state
          oldstate <- get("state", pos = parent.frame())
          newstate <- list(it = k, im = i, 
                           dep = bname, 
                           meth = theMethod, 
                           log = oldstate$log)
          assign("state", newstate, pos = parent.frame(), inherits = TRUE)
          
          if (printFlag) cat(" ", b)
          
          # (repeated) univariate imputation - type method
          if (univ) {
            for (j in b) {
              imp[[j]][, i] <- 
                sampler.univ(data = data, r = r, where = where, 
                             type = type, formula = ff, 
                             method = theMethod, 
                             yname = j, 
                             k = k, ...)
              
              data[(!r[, j]) & where[, j], j] <- 
                imp[[j]][(!r[, j])[where[, j]], i]
              
              # optional post-processing
              cmd <- post[bname]
              if (cmd != "") {
                eval(parse(text = cmd))
                data[where[, j], j] <- imp[[j]][, i]
              }
            }
          }
          
          # multivariate imputation - type method
          if (mult) {
            mis <- !r
            mis[, setdiff(b, colnames(data))] <- FALSE
            data[mis] <- NA
            
            fm <- paste("mice.impute", theMethod, sep = ".")
            imputes <- do.call(fm, args = list(data = data, type = type, ...))
            if (is.null(imputes)) stop("No imputations from ", theMethod)
            for (j in names(imputes)) {
              imp[[j]][, i] <- imputes[[j]]
              data[!r[, j], j] <- imp[[j]][, i]
            }
          }
          
          # # multivariate imputation - formula method
          # if (mult.formula) {
          #   mis <- !r
          #   mis[, setdiff(b, colnames(data))] <- FALSE
          #   data[mis] <- NA
          #   
          #   fm <- paste("mice.impute", theMethod, sep = ".")
          #   imputes <- do.call(fm, args = list(data = data, type = type, ...))
          #   if (is.null(imputes)) stop("No imputations from ", theMethod)
          #   for (j in names(imputes)) {
          #     imp[[j]][, i] <- imputes[[j]]
          #     data[!r[, j], j] <- imp[[j]][, i]
          #   }
          # }
          
          # passive imputation
          if (pass) {
            for (j in b) {
              wy <- where[, j]
              imp[[j]][, i] <- model.frame(as.formula(theMethod), data[wy, ], 
                                           na.action = na.pass)
              data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
            }
          }
        } # end h loop (blocks)
      }  # end i loop (imputation number)
      
      # store means and sd of m imputes
      k2 <- k - from + 1
      if (length(visitSequence) > 0) {
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            if (!is.factor(data[, j])) {
              chainVar[j, k2, ] <- apply(imp[[j]], 2, var, na.rm = TRUE)
              chainMean[j, k2, ] <- colMeans(as.matrix(imp[[j]]), na.rm = TRUE)
            }
            if (is.factor(data[, j])) {
              for (mm in seq_len(m)) {
                nc <- as.integer(factor(imp[[j]][, mm], levels = levels(data[, j])))
                chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
                chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
              }
            }
          }
        }
      }
    }  # end main iteration
    
    if (printFlag)
      cat("\n")
  }
  return(list(iteration = maxit, imp = imp, chainMean = chainMean, chainVar = chainVar))
}


sampler.univ <- function(data, r, where, type, formula, method, yname, k, ...) {
  j <- yname[1]
  x <- obtain.design(data, formula)
  y <- data[, j]
  ry <- complete.cases(x, y) & r[, j]
  wy <- complete.cases(x) & where[, j]
  cc <- wy[where[, j]]
  if (k == 1) check.df(x, y, ry)
  
  keep <- remove.lindep(x, y, ry, ...)
  x <- x[, keep, drop = FALSE]
  type <- type[keep]
  
  # here we go
  f <- paste("mice.impute", method, sep = ".")
  imputes <- data[wy, j]
  imputes[!cc] <- NA
  imputes[cc] <- do.call(f, args = list(y, ry, x, 
                                        wy = wy, type = type, ...))
  imputes
}
