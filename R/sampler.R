sampler <- function(p, data, where, m, imp, r, visitSequence, fromto, printFlag, ...)
  # The sampler controls the actual Gibbs sampling iteration scheme.
  # This function is called by mice and mice.mids
  # Authors: S van Buuren, K Groothuis-Oudshoorn
{
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  
  # set up array for convergence checking
  chainVar <- chainMean <- NULL
  if (maxit > 0)
    chainVar <- chainMean <- array(0, dim = c(length(visitSequence), maxit, m), 
                                   dimnames = list(dimnames(data)[[2]][visitSequence], 
                                                   seq_len(maxit), paste("Chain", seq_len(m))))
  
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
        
        # complete data, but do not overwrite any observed data
        for (j in visitSequence) {
          wy <- where[, j]
          ry <- r[, j]
          p$data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
        }
        
        # refresh dummy variables
        for (j in setdiff(p$visitSequence, visitSequence)) {
          cat.columns <- p$data[, p$categories[j, 4]]
          mm <- model.matrix(~ cat.columns - 1, model.frame(~ cat.columns, na.action = na.pass))[, -1]
          p$data[, (j:(j + p$categories[p$categories[j, 4], 2] - 1))] <- mm
        }
        
        # one iteration over augmented model
        for (j in p$visitSequence) {
          theMethod <- p$method[j]
          vname <- dimnames(p$data)[[2]][j]
          
          ## store current state
          oldstate <- get("state", pos = parent.frame())
          newstate <- list(it = k, im = i, co = j, dep = vname, meth = theMethod, log = oldstate$log)
          assign("state", newstate, pos = parent.frame(), inherits = TRUE)
          
          if (printFlag && theMethod != "dummy")
            cat(" ", vname)
          
          # switching logic: flat, mult, pass, dumm
          empt <- theMethod == ""
          elem <- !empt && !is.passive(theMethod) && theMethod != "dummy"
          flat <- elem && substring(theMethod, 1, 2) != "2l"
          pass <- !empt && is.passive(theMethod)
          dumm <- theMethod == "dummy"
          
          # standard imputation
          if (elem) {
            if (flat) {
              predictors <- p$predictorMatrix[j, ] == 1
            } else {
              predictors <- p$predictorMatrix[j, ] != 0
            }
            if (!is.null(p$form) && nchar(p$form[j]) > 0) {
              myform <- paste(p$form[j], "0", sep = "+")
              x <- model.matrix(formula(myform), p$data)
            } else {
              x <- p$data[, predictors, drop = FALSE]
            }
            y <- p$data[, j]
            ry <- complete.cases(x, y) & r[, j]
            wy <- complete.cases(x) & where[, j]
            cc <- wy[where[, j]]
            type <- p$predictorMatrix[j, predictors]
            if (k == 1)
              check.df(x, y, ry)
            keep <- remove.lindep(x, y, ry, ...)
            x <- x[, keep, drop = FALSE]
            type <- type[keep]
            f <- paste("mice.impute", theMethod, sep = ".")
            imputes <- p$data[wy, j]
            imputes[!cc] <- NA
            imputes[cc] <- do.call(f, args = list(y, ry, x, wy = wy, type = type, ...))
            imp[[j]][, i] <- imputes
            p$data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, j])[where[, j]], i]
          }
          
          # passive imputation
          if (pass) {
            wy <- where[, j]
            imp[[j]][, i] <- model.frame(as.formula(theMethod), p$data[wy, ], 
                                         na.action = na.pass)
            p$data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
          }
          
          # dummy imputation
          if (dumm) {
            cat.columns <- p$data[, p$categories[j, 4]]
            mm <- model.matrix(~ cat.columns - 1, 
                               model.frame(~ cat.columns, na.action = na.pass))[, -1]
            p$data[, (j:(j + p$categories[p$categories[j, 4], 2] - 1))] <- mm
            remove("cat.columns")
          }
          
          # optional post-processing
          cmd <- p$post[j]
          if (cmd != "") {
            eval(parse(text = cmd))
            p$data[where[, j], j] <- imp[[j]][, i]
          }
        }  # end j loop (variables)
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
