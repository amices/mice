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
  
  ## THE ITERATION MAIN LOOP: GIBBS SAMPLER
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
        design <- initialize.design(data)
        
        # impute block-by-block
        for (h in visitSequence) {
          theMethod <- method[h]
          # impute variable-by-variable
          for (j in blocks[[h]]) {
            
            ## store current state
            oldstate <- get("state", pos = parent.frame())
            newstate <- list(it = k, im = i, dep = j, meth = theMethod, 
                             log = oldstate$log)
            assign("state", newstate, pos = parent.frame(), inherits = TRUE)
            
            if (printFlag && theMethod != "dummy")
              cat(" ", j)
            
            # switching logic: flat, mult, pass, dumm
            empt <- theMethod == ""
            elem <- !empt && !is.passive(theMethod) && theMethod != "dummy"
            flat <- elem && substring(theMethod, 1, 2) != "2l"
            pass <- !empt && is.passive(theMethod)
            dumm <- theMethod == "dummy"
            
            # standard imputation
            if (elem) {
              if (flat) {
                predictors <- predictorMatrix[j, ] == 1
              } else {
                predictors <- predictorMatrix[j, ] != 0
              }
              if (!is.null(form) && nchar(form[j]) > 0) {
                # form: reconstruct entire model.matrix
                myform <- paste(form[j], "0", sep = "+")
                x <- model.matrix(formula(myform), data)
                type <- NULL
              } else {
                # predictormatrix: select columns from entire design
                idx <- attr(design, "assign") %in% which(predictors)
                x <- design[, idx, drop = FALSE]
                type <- predictorMatrix[j, attr(design, "assign")[idx]]
                names(type) <- names(x)
              }
              y <- data[, j]
              ry <- complete.cases(x, y) & r[, j]
              wy <- complete.cases(x) & where[, j]
              cc <- wy[where[, j]]
              if (k == 1) check.df(x, y, ry)
              
              keep <- remove.lindep(x, y, ry, ...)
              x <- x[, keep, drop = FALSE]
              type <- type[keep]
              
              f <- paste("mice.impute", theMethod, sep = ".")
              imputes <- data[wy, j]
              imputes[!cc] <- NA
              imputes[cc] <- do.call(f, args = list(y, ry, x, wy = wy, type = type, ...))
              imp[[j]][, i] <- imputes
              data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, j])[where[, j]], i]
              design <- update.design(design, data, varname = j)
            }
            
            # passive imputation
            if (pass) {
              wy <- where[, j]
              imp[[j]][, i] <- model.frame(as.formula(theMethod), data[wy, ], 
                                           na.action = na.pass)
              data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
            }
            
            # optional post-processing
            cmd <- post[j]
            if (cmd != "") {
              eval(parse(text = cmd))
              data[where[, j], j] <- imp[[j]][, i]
            }
          } # end j loop (variables)
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
