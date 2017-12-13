sampler <- function(data, m, where, imp, setup, fromto, printFlag, ...)
  # The sampler controls the actual Gibbs sampling iteration scheme.
  # This function is called by mice and mice.mids
  # Authors: S van Buuren, K Groothuis-Oudshoorn
{
  blocks <- setup$blocks
  method <- setup$method
  visitSequence <- setup$visitSequence
  predictorMatrix <- setup$predictorMatrix
  form <- setup$form
  post <- setup$post
  
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)
  
  # set up array for convergence checking
  chainVar <- chainMean <- NULL
  if (maxit > 0) {
    chainMean <- array(0, dim = c(length(visitSequence), maxit, m))
    dimnames(chainMean) <- list(dimnames(data)[[2]][visitSequence], 
                                seq_len(maxit), paste("Chain", seq_len(m)))
    chainVar <- chainMean
  }
  
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
          ff <- create.formula(form = ~ 0, predictors = predictors, ...)
          
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
            x <- obtain.design(data, ff)
            y <- data[, j]
            ry <- complete.cases(x, y) & r[, j]
            wy <- complete.cases(x) & where[, j]
            cc <- vector("list", length(j))
            names(cc) <- j
            for (jj in j) cc[[jj]] <- wy[where[, jj], jj]
            if (k == 1) check.df(x, y, ry)
            
            #keep <- remove.lindep(x, y, ry, ...)
            #x <- x[, keep, drop = FALSE]
            #type <- type[keep]
            
            # here we go
            fm <- paste("mice.impute", theMethod, sep = ".")
            z <- do.call(fm, args = list(y, ry, x, wy = wy, 
                                         type = type, format = "list", ...))
            if (!identical(names(z), j)) stop("Mismatch ", names(z), " and ", j)
            
            imputes <- data[wy, j]
            imputes[!cc] <- NA
            imputes[cc] <- z
            for (k in j) {
              imp[[k]][, i] <- imputes
              data[(!r[, k]) & where[, k], j] <- imp[[k]][(!r[, k])[where[, k]], i]
            }
          }
          
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
        for (j in seq_along(visitSequence)) {
          jj <- visitSequence[j]
          if (!is.factor(data[, jj])) {
            chainVar[j, k2, ] <- apply(imp[[jj]], 2, var, na.rm = TRUE)
            chainMean[j, k2, ] <- colMeans(as.matrix(imp[[jj]]), na.rm = TRUE)
          }
          if (is.factor(data[, jj])) {
            for (mm in seq_len(m)) {
              nc <- as.integer(factor(imp[[jj]][, mm], levels = levels(data[, jj])))
              chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
              chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
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
