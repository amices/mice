check.data <- function(data) {
  data <- check.dataform(data)
  
  # predictorMatrix <- pred
  # visitSequence <- vis
  # post <- post
  # meth <- meth
  
  # # stop if the class variable is a factor
  # isclassvar <- apply(pred == -2, 2, any)
  # for (j in seq_len(nvar)) {
  #   if (isclassvar[j] && is.factor(data[,j])) 
  #     stop("Class variable (column ", j,
  #          ") cannot be factor. Convert to numeric by as.integer()")        
  # }
  # 
  # # remove constant variables but leave passive variables untouched
  # for (j in seq_len(nvar)) {
  #   if (!is.passive(meth[j])) {
  #     d.j <- data[, j]
  #     v <- if (is.character(d.j)) NA else var(as.numeric(d.j), na.rm = TRUE)
  #     constant <- if (allow.na) {
  #       if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
  #     } else {
  #       is.na(v) || v < 1000 * .Machine$double.eps
  #     }
  #     didlog <- FALSE
  #     if (constant && any(pred[, j] != 0)) {
  #       out <- varnames[j]
  #       pred[, j] <- 0
  #       updateLog(out = out, meth = "constant")
  #       didlog <- TRUE
  #     }
  #     if (constant && meth[j] != "") {
  #       out <- varnames[j]
  #       pred[j, ] <- 0
  #       if (!didlog)
  #         updateLog(out = out, meth = "constant")
  #       meth[j] <- ""
  #       vis <- vis[vis != j]
  #       post[j] <- ""
  #     }
  #   }
  # }
  # 
  # ## remove collinear variables
  # ispredictor <- apply(pred != 0, 2, any)
  # if (any(ispredictor)) {
  #   droplist <- find.collinear(data[, ispredictor, drop = FALSE], ...)
  # } else {
  #   droplist <- NULL
  # }
  # if (length(droplist) > 0 && remove_collinear) {
  #   for (k in seq_along(droplist)) {
  #     j <- which(varnames %in% droplist[k])
  #     didlog <- FALSE
  #     if (any(pred[, j] != 0)) {
  #       # remove as predictor
  #       out <- varnames[j]
  #       pred[, j] <- 0
  #       updateLog(out = out, meth = "collinear")
  #       didlog <- TRUE
  #     }
  #     if (meth[j] != "") {
  #       out <- varnames[j]
  #       pred[j, ] <- 0
  #       if (!didlog)
  #         updateLog(out = out, meth = "collinear")
  #       meth[j] <- ""
  #       vis <- vis[vis != j]
  #       post[j] <- ""
  #     }
  #   }
  # }
  
}

check.dataform <- function(data) {
  if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame", call. = FALSE)
  if (ncol(data) < 2)
    stop("Data should contain at least two columns", call. = FALSE)
  data <- as.data.frame(data)
  mat <- sapply(data, is.matrix)
  if (any(mat)) stop("Cannot handle columns with class matrix: ", 
                     colnames(data)[mat])
  data
}

check.cluster <- function(data, predictorMatrix) {
  # stop if the cluster variable is a factor
  isclassvar <- apply(predictorMatrix == -2, 2, any)
  for (j in colnames(predictorMatrix)) {
    if (isclassvar[j] && lapply(data, is.factor)[[j]]) 
      stop("Convert cluster variable ", j, " to integer by as.integer()")
  }
  TRUE
}
