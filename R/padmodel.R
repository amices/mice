padModel <- function(data, method, predictorMatrix, visitSequence, 
                     form, post, nvar) {
  # Called by mice().  Augments the imputation model by including 
  # dummy variables. Adapts data, predictorMatrix, method and
  # visitSequence.  Returns a list whose components make up the padded model.
  
  categories <- data.frame(
    yes.no.categorical = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), 
    number.of.dummies = rep(0, nvar), 
    yes.no.dummy = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), 
    corresponding.column.dummy = rep(0, nvar))
  
  # make explicit local copy
  data <- data
  
  for (j in 1:nvar) {
    if (is.factor(data[, j]) && any(predictorMatrix[, j] != 0)) {
      categories[j, 1] <- TRUE
      data[, j] <- C(data[, j], contr.treatment)
      n.dummy <- length(levels(data[, j])) - 1
      
      # add NA as extra level
      # data[, j] <- addNA(data[, j])
      
      categories[j, 2] <- n.dummy
      predictorMatrix <- rbind(predictorMatrix,
                               matrix(0, 
                                      ncol = ncol(predictorMatrix), 
                                      nrow = n.dummy))
      predictorMatrix <- cbind(predictorMatrix, 
                               matrix(rep(predictorMatrix[, j], times = n.dummy), 
                                      ncol = n.dummy))
      predictorMatrix[1:nvar, j] <- rep(0, times = nvar)
      form <- c(form, rep("", n.dummy))
      
      if (any(visitSequence == j)) {
        idx <- (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)
        predictorMatrix[idx, j] <- rep(1, times = n.dummy)
        newcol <- ncol(predictorMatrix) - n.dummy + 1
        nloops <- sum(visitSequence == j)
        for (ii in 1:nloops) {
          idx2 <- (1:length(visitSequence))[visitSequence == j][ii]
          visitSequence <- append(visitSequence, newcol, idx2)
        }
      }
      data <- cbind(data, matrix(0, ncol = n.dummy, nrow = nrow(data)))
      idx <- (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)
      data[is.na(data[, j]), idx] <- NA
      cat.column <- data[!is.na(data[, j]), j]
      
      # model expansion by model.matrix()
      data[!is.na(data[, j]), idx] <- model.matrix(~cat.column - 1)[, -1]
      names(data)[idx] <- paste(attr(data, "names")[j], (1:n.dummy), sep = ".")
      method <- c(method, rep("dummy", n.dummy))
      post <- c(post, rep("", n.dummy))
      categories <- rbind(
        categories, 
        data.frame(yes.no.categorical = rep(FALSE, n.dummy), 
                   number.of.dummies = rep(0, n.dummy), 
                   yes.no.dummy = rep(TRUE, n.dummy), 
                   corresponding.column.dummy = rep(j, n.dummy)))
    }
  }
  
  varnames <- dimnames(data)[[2]]
  dimnames(predictorMatrix) <- list(varnames, varnames)
  names(method) <- varnames
  names(form) <- varnames
  names(post) <- varnames
  names(visitSequence) <- varnames[visitSequence]
  dimnames(categories)[[1]] <- dimnames(data)[[2]]
  return(list(data = as.data.frame(data), 
              predictorMatrix = predictorMatrix, 
              method = method, 
              visitSequence = visitSequence,
              form = form, post = post, categories = categories))
}
