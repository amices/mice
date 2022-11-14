parse.ums <- function(x, ums = NULL, umx = NULL, ...) {
  if (is.null(ums)) stop("Unidentifiable model specification (ums) not found.")
  if (!is.null(umx)) x <- base::cbind(x, umx)

  ## Unidentifiable part
  # e.g. specified in blots as list(X = list(ums = "-3+2*bmi"))
  mnar0 <- gsub("-", "+-", ums)
  mnar0 <- unlist(strsplit(mnar0, "+", fixed = TRUE))
  if (mnar0[1L] == "") mnar0 <- mnar0[-1L]
  if (sum(!grepl("*", mnar0, fixed = TRUE)) == 0L) {
    stop("An intercept (constant) term must be included in the expression")
  } else if (sum(!grepl("*", mnar0, fixed = TRUE)) == 1L) {
    mnar0[!grepl("*", mnar0, fixed = TRUE)] <- paste(
      mnar0[!grepl("*", mnar0, fixed = TRUE)], "*intercept",
      sep = ""
    )
  } else if (sum(!grepl("*", mnar0, fixed = TRUE)) > 1L) {
    stop("Only one intercept term allowed")
  }
  mnar <- strsplit(mnar0, "*", fixed = TRUE)
  # e.g. c("-3","2")
  mnar.parm <- as.numeric(unlist(lapply(mnar, function(x) x[1L])))
  # e.g. c("intercept","bmi")
  mnar.vars <- unlist(lapply(mnar, function(x) x[2L]))
  mnar.parm <- mnar.parm[c(which(mnar.vars == "intercept"), which(mnar.vars != "intercept"))]
  mnar.vars <- mnar.vars[c(which(mnar.vars == "intercept"), which(mnar.vars != "intercept"))]
  xmnar <- as.matrix(cbind(1, as.matrix(x[, mnar.vars[!mnar.vars == "intercept"]])))

  list(delta = mnar.parm, x = xmnar)
}
