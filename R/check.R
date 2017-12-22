# internal function for checking input to main mice() function


check.data <- function(setup, data, allow.na = FALSE, 
                       remove_collinear = TRUE, ...) {
  blocks <- setup$blocks
  nimp <- setup$nimp
  pred <- setup$predictorMatrix
  nvar <- setup$nvar
  varnames <- setup$varnames
  meth <- setup$method
  vis <- setup$visitSequence
  post <- setup$post
  nblo <- length(blocks)
  
  # stop if the class variable is a factor
  if (!is.null(pred)) {
    isclassvar <- apply(pred == -2, 2, any)
    for (j in seq_len(nvar)) {
      if (isclassvar[j] && is.factor(data[, j])) 
        stop("Class variable (column ", j,
             ") cannot be factor. Convert to numeric by as.integer()")
    }
  }
  
  # # remove constant variables but leave passive variables untouched
  # for (j in seq_len(nvar)) {
  #   d.j <- data[, j]
  #   v <- ifelse(is.character(d.j), NA, var(as.numeric(d.j), na.rm = TRUE))
  #   constant <- if (allow.na) {
  #     if (is.na(v)) FALSE else v < 1000 * .Machine$double.eps
  #   } else {
  #     is.na(v) || v < 1000 * .Machine$double.eps
  #   }
  #   if (constant) {
  #     pred[, j] <- 0
  #     i <- grep(varnames[j], rownames(pred))[1]
  #     pred[i, ] <- 0
  #     meth[i] <- ""
  #     post[i] <- ""
  #     vis <- vis[vis != i]
  #     updateLog(out = varnames[j], meth = "constant")
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
  #     j <- grep(droplist[k], varnames)[1]
  #     pred[, j] <- 0
  #     i <- grep(varnames[j], rownames(pred))[1]
  #     pred[i, ] <- 0
  #     meth[i] <- ""
  #     vis <- vis[vis != i]
  #     post[i] <- ""
  #     updateLog(out = varnames[j], meth = "collinear")
  #   }
  # }
  
  setup$predictorMatrix <- pred
  setup$visitSequence <- vis
  setup$post <- post
  setup$meth <- meth
  setup
}

check.post <- function(setup) {
  blocks <- setup$blocks
  post <- setup$post
  
  # check
  #if (length(post) != length(blocks))
  #  stop("`length(post)` does not match `length(blocks)`.")
  
  # change
  #if (is.null(names(post))) names(post) <- names(blocks)
  
  setup$post <- post
  setup
}

