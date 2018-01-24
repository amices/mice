sampler <- function(data, m, where, imp, blocks, method, visitSequence, 
                    predictorMatrix, formulas, blots, post, 
                    fromto, printFlag, ...)
  # The sampler controls the actual Gibbs sampling iteration scheme.
  # This function is called by mice and mice.mids
{
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
        
        # impute block-by-block
        for (h in visitSequence) {
          ct <- attr(blocks, "calltype")
          calltype <- ifelse(length(ct) == 1, ct[1], ct[h])
          
          b <- blocks[[h]]
          bname <- names(blocks)[h]
          if (calltype == "formula") ff <- formulas[[h]] else ff <- NULL
          if (calltype == "type") type <- predictorMatrix[h, ] else type <- NULL
          user <- blots[[h]]
          
          # univariate/multivariate logic
          theMethod <- method[h]
          empt <- theMethod == ""
          univ <- !empt && !is.passive(theMethod) && 
            !handles.format(paste0("mice.impute.", theMethod))
          mult <- !empt && !is.passive(theMethod) && 
            handles.format(paste0("mice.impute.", theMethod))
          pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 1
          
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
                             yname = j, k = k, 
                             calltype = calltype, 
                             user = user, ...)
              
              data[(!r[, j]) & where[, j], j] <- 
                imp[[j]][(!r[, j])[where[, j]], i]
              
              # optional post-processing
              cmd <- post[j]
              if (cmd != "") {
                eval(parse(text = cmd))
                data[where[, j], j] <- imp[[j]][, i]
              }
            }
          }
          
          # multivariate imputation - type and formula
          if (mult) {
            mis <- !r
            mis[, setdiff(b, colnames(data))] <- FALSE
            data[mis] <- NA
            
            fm <- paste("mice.impute", theMethod, sep = ".")
            if (calltype == "formula")
              imputes <- do.call(fm, args = list(data = data, 
                                                 formula = ff, ...))
            else if (calltype == "type")
              imputes <- do.call(fm, args = list(data = data, 
                                                 type = type, ...))
            else stop("Cannot call function of type ", calltype, 
                      call. = FALSE)
            if (is.null(imputes)) stop("No imputations from ", theMethod, 
                                       bname, call. = FALSE)
            for (j in names(imputes)) {
              imp[[j]][, i] <- imputes[[j]]
              data[!r[, j], j] <- imp[[j]][, i]
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


sampler.univ <- function(data, r, where, type, formula, method, yname, k, 
                         calltype = "type", user, ...) {
  j <- yname[1]
  if (calltype == "type") {
    vars <- colnames(data)[type != 0]
    formula <- reformulate(setdiff(vars, j), response = j)
    formula <- update(formula, ". ~ . -1")
  }
  if (calltype == "formula") {
    # move terms other than j from lhs to rhs, remove intercept
    ymove <- setdiff(lhs(formula), j)
    formula <- update(formula, paste(j, " ~ . -1"))
    if (length(ymove) > 0L) 
      formula <- update(formula, paste("~ . + ", paste(ymove, collapse = "+")))
  }
  x <- obtain.design(data, formula)
  y <- data[, j]
  ry <- complete.cases(x, y) & r[, j]
  wy <- complete.cases(x) & where[, j]
  
  # nothing to impute
  if (all(!wy)) return(numeric(0))
  
  cc <- wy[where[, j]]
  if (k == 1) check.df(x, y, ry)
  
  keep <- remove.lindep(x, y, ry, ...)
  x <- x[, keep, drop = FALSE]
  type <- type[keep]
  
  # here we go
  f <- paste("mice.impute", method, sep = ".")
  imputes <- data[wy, j]
  imputes[!cc] <- NA
  ## ready
  args <- c(list(y, ry, x, wy = wy, type = type), user, list(...))
  imputes[cc] <- do.call(f, args = args)
  imputes
}
