padModel <- function(data, method, predictorMatrix, visitSequence, 
                     form, post, nvar) {
  # Called by mice().  Augments the imputation model by including 
  # dummy variables. Adapts data, predictorMatrix, method and
  # visitSequence.  Returns a list whose components make up the padded model.
  
  categories <- data.frame(
    is.factor = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), 
    n.dummy   = rep(0, nvar), 
    is.dummy  = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), 
    father    = rep(0, nvar))
  
  # make explicit local copy
  data <- data
  
  for (j in 1:nvar) {
    if (is.factor(data[, j]) && any(predictorMatrix[, j] != 0)) {
      
      categories[j, 1] <- TRUE
      data[, j] <- C(data[, j], contr.treatment)
      
      n.dummy <- length(levels(data[, j])) - 1
      categories[j, 2] <- n.dummy
      
      # add n.dummy rows and columns to curreny predictorMatrix
      predictorMatrix <- rbind(predictorMatrix,
                               matrix(0, 
                                      ncol = ncol(predictorMatrix), 
                                      nrow = n.dummy))
      predictorMatrix <- cbind(predictorMatrix, 
                               matrix(rep(predictorMatrix[, j], times = n.dummy), 
                                      ncol = n.dummy))
      
      # remove j as predictor
      predictorMatrix[1:nvar, j] <- rep(0, times = nvar)
      form <- c(form, rep("", n.dummy))
      
      if (any(visitSequence == j)) {
        # make column j a predictor for its dummies
        idx <- (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)
        predictorMatrix[idx, j] <- rep(1, times = n.dummy)
        
        # extend visitSequence
        newcol <- ncol(predictorMatrix) - n.dummy + 1
        nloops <- sum(visitSequence == j)
        for (ii in 1:nloops) {
          idx2 <- (1:length(visitSequence))[visitSequence == j][ii]
          visitSequence <- append(visitSequence, newcol, idx2)
        }
      }
      
      # add n.dummy columns to data
      data <- cbind(data, matrix(0, ncol = n.dummy, nrow = nrow(data)))
      idx <- (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)

      # set missing entries in new columns
      data[is.na(data[, j]), idx] <- NA
      
      # and merge model expansion by model.matrix() for observed values
      cat.column <- data[!is.na(data[, j]), j]
      data[!is.na(data[, j]), idx] <- model.matrix(~ cat.column - 1)[, -1]
      
      # name new columns
      names(data)[idx] <- paste(attr(data, "names")[j], (1:n.dummy), sep = ".")
      
      # extend method and post arrays
      method <- c(method, rep("dummy", n.dummy))
      post <- c(post, rep("", n.dummy))
      
      # append administrative info
      categories <- rbind(
        categories, 
        data.frame(is.factor = rep(FALSE, n.dummy), 
                   n.dummy   = rep(0, n.dummy), 
                   is.dummy  = rep(TRUE, n.dummy), 
                   father    = rep(j, n.dummy)))
    }
  }
  
  varnames <- dimnames(data)[[2]]
  dimnames(predictorMatrix) <- list(varnames, varnames)
  names(method) <- varnames
  names(form) <- varnames
  names(post) <- varnames
  names(visitSequence) <- varnames[visitSequence]
  dimnames(categories)[[1]] <- dimnames(data)[[2]]
  
  if (any(duplicated(names(data))))
    stop("Column names of padded data not unique")
  
  return(list(data = as.data.frame(data), 
              predictorMatrix = predictorMatrix, 
              method = method, 
              visitSequence = visitSequence,
              form = form, post = post, categories = categories))
}
