#' Jamshidian and Jalal's Non-Parametric MCAR Test
#'
#' Test whether missingness is contingent upon the observed variables,
#' according to the methodology developed by Jamshidian and Jalal (2010) (see
#' Details).
#' @param x An object for which a method exists; usually a `data.frame`.
#' @param imputed Either an object of class `mids`, as returned by
#' [mice::mice()], or a list of `data.frame`s.
#' @param min_n Atomic numeric, must be greater than 1. When there are missing
#' data patterns with fewer than `min_n` cases, all cases with that pattern will
#' be removed from `x` and `imputed`.
#' @param method Atomic character. If it is known (or assumed) that data are
#' either multivariate normally distributed or not, then use either
#' `method = "hawkins"` or `method = "nonparametric"`, respectively.
#' The default argument `method = "auto"` follows the procedure outlined in the
#' Details section, and in Figure 7 of Jamshidian and Jalal (2010).
#' @param replications Number of replications used to simulate the Neyman
#' distribution when performing Hawkins' test. As this method is based on random
#' sampling, use a high number of `replications` (and optionally,
#' [set.seed()]) to minimize Monte Carlo error and ensure reproducibility.
#' @param use_chisq Atomic integer, indicating the minimum number of cases
#' within a group *k* that triggers the use of asymptotic Chi-square
#' distribution instead of the emprical distribution in the Neyman uniformity
#' test, which is performed as part of Hawkins' test.
#' @param alpha Atomic numeric, indicating the significance level of tests.
#' @details Three types of missingness have been distinguished in the literature
#' (Rubin, 1976):
#' Missing completely at random (MCAR), which means that missingness is random;
#' missing at random (MAR), which means that missingness is contingent on the
#' *observed*;
#' and missing not at random (MNAR), which means that missingness is related to
#' unobserved data.
#'
#' Jamshidian and Jalal's non-parametric MCAR test assumes that the missing data
#' are either MCAR or MAR, and tests whether the missingness is independent of
#' the observed values. If so, the covariance matrices of the imputed data will
#' be equal accross groups with different patterns of missingness. This test
#' consists of the following procedure:
#' \enumerate{
#'   \item Data are imputed.
#'   \item The imputed data are split into *k* groups according to the
#'   *k* missing data patterns in the original data (see
#'   [mice::md.pattern()]).
#'   \item Perform Hawkins' test for equality of covariances across the *k*
#'   groups.
#'   \item If the test is *not significant*, conclude that there is no evidence
#'   against multivariate normality of the data, nor against MCAR.
#'   \item If the test *is significant*, and multivariate normality of the data
#'   can be assumed, then it can be concluded that missingness is MAR.
#'   \item If multivariate normality cannot be assumed, then perform the
#'   Anderson-Darling non-parametric test for equality of covariances across the
#'   *k* groups.
#'   \item If the Anderson-Darling test is *not significant*, this is evidence
#'   against multivariate normality - but no evidence against MCAR.
#'   \item If the Anderson-Darling test *is significant*, this is evidence
#'   it can be concluded that missingness is MAR.
#' }
#'
#' Note that, despite its name in common parlance, an MCAR test can only
#' indicate whether missingness is MCAR or MAR. The procedure cannot distinguish
#' MCAR from MNAR, so a non-significant result does not rule out MNAR.
#'
#' This is a re-implementation of the function `TestMCARNormality`, which was
#' originally published in the R-packgage `MissMech`, which has been removed
#' from CRAN. This new implementation is faster, as its backend is written in
#' C++. It also enhances the functionality of the original:
#' \itemize{
#'   \item Multiply imputed data can now be used; the median p-value and test
#'   statistic across replications is then reported, as suggested by
#'   Eekhout, Wiel, and Heymans (2017).
#'   \item The printing method for an `mcar_object` gives a warning when at
#'   least one p-value of either test was significant. In this case, it is
#'   recommended to inspect the range of p-values, and consider potential
#'   violations of MCAR.
#'   \item A plotting method for an `mcar_object` is provided.
#'   \item A plotting method for the `$md.pattern` element of an `mcar_object`
#'   is provided.
#' }
#'
#' @return An object of class `mcar_object`.
#' @author Caspar J. Van Lissa
#' @references
#' Rubin, D. B. (1976). Inference and Missing Data. Biometrika, Vol. 63, No. 3,
#' pp. 581-592. <doi:10.2307/2335739>
#'
#' Eekhout, I., M. A. Wiel, & M. W. Heymans (2017). Methods for Significance
#' Testing of Categorical Covariates in Logistic Regression Models After
#' Multiple Imputation: Power and Applicability Analysis. BMC Medical Research
#' Methodology 17 (1): 129.
#'
#' Jamshidian, M., & Jalal, S. (2010). Tests of homoscedasticity, normality, and
#' missing completely at random for incomplete multivariate data. Psychometrika,
#' 75(4), 649–674. <doi:10.1007/s11336-010-9175-3>
#' @keywords internal
#' @examples
#' res <- mcar(nhanes)
#' # Examine test results
#' res
#' # Plot p-values across imputed data sets
#' plot(res)
#' # Plot md patterns used for the test
#' plot(res, type = "md.pattern")
#' # Note difference with the raw md.patterns:
#' md.pattern(nhanes)
#' @export
#' @importFrom stats cov pchisq spline
#' @md
mcar <- function(x,
                 imputed = mice(x, method = "norm"),
                 min_n = 6,
                 method = "auto",
                 replications = 10000,
                 use_chisq = 30,
                 alpha = 0.05) {
  UseMethod("mcar", x)
}

#' @method mcar data.frame
#' @export
mcar.data.frame <- function(x,
                            imputed = mice(x, method = "norm"),
                            min_n = 6,
                            method = "auto",
                            replications = 10000,
                            use_chisq = 30,
                            alpha = 0.05) {
  anyfact <- sapply(x, inherits, what = "factor")
  if(any(anyfact)){
    stop("Be advised that this MCAR test has not been validated for categorical variables.")
  }
  if(min_n < 1){
    stop("Argument 'min_n' must be greater than 1.")
  }
  out <-
    list(
      hawk_chisq = NULL,
      hawk_df = NULL,
      hawk_p = NULL,
      ad_value = NULL,
      ad_p = NULL,
      alpha = alpha,
      method = method,
      md.pattern = NULL,
      removed_rows = NULL,
      removed_patterns = NULL
    )
  if(inherits(imputed, "mids")){
    imputed <- mice::complete(imputed, action = "all")
  } else if(inherits(imputed, "list")){
    if(!inherits(imputed[[1]], "data.frame")){
      imputed <- tryCatch(lapply(imputed, data.frame), error = function(e){
        stop("Argument 'imputed' must be a list of data.frames or an object of class 'mids'. Could not coerce argument 'imputed' to class 'data.frame'.")
      })
      message("Argument 'imputed' must be an object of class 'mids', or a list of 'data.frame's. Coerced argument 'imputed' to data.frame.")
    }
  }
  if(!all(dim(x) == dim(imputed[[1]]))){
    stop("Imputed data must have the same dimensions as the incomplete data.")
  }
  if(any(is.na(imputed[[1]]))){
    stop("Imputed data contain missing values.")
  }
  missings <- is.na(x)
  rowmis <- rowSums(missings)
  colmis <- colSums(missings)
  if(any(rowmis == ncol(x))) {
    warning("Note that there were some rows with all missing data. Tests may be invalid for these rows. Consider removing them.")
    #x <- x[!rowmis == ncol(x), , drop = FALSE]
  }
  if(any(colmis == nrow(x))) {
    stop("Some columns contain all missing data. This will result in invalid results.")
  }
  univals <- sapply(x, function(i){length(unique(i))})
  if(any(univals < 2)){
    stop("Some columns are constant, not variable. This will result in invalid results.")
  }
  newdata <- x
  missings <- is.na(x)
  pats <- mice::md.pattern(x, plot = FALSE)
  remove_pats <- as.numeric(rownames(pats))[-nrow(pats)] <= min_n
  if (any(remove_pats)) {
    out$removed_patterns <- pats[remove_pats, ]
    pats <- pats[-nrow(pats), colnames(missings)]
    idmiss <- do.call(paste, as.data.frame(missings))
    idpats <- do.call(paste, as.data.frame(pats == 0))
    remove_these <- idmiss %in% idpats[remove_pats]
    out$removed_rows <- remove_these
    newdata <- x[!remove_these, , drop = FALSE]
    imputed <- lapply(imputed, `[`, i = !remove_these, j = colnames(newdata), drop = FALSE)
    missings <- is.na(newdata)
    pats <- mice::md.pattern(newdata, plot = FALSE)
  }
  class(pats) <- c("md.pattern", class(pats))
  out$md.pattern <- pats
  pat_n <- as.numeric(rownames(pats))[-nrow(pats)]
  pats <- pats[-nrow(pats), colnames(missings)]
  idpats <- idpats[!remove_pats]
  idmiss <- do.call(paste, as.data.frame(missings))
  which_pat <- apply(sapply(idpats, `==`, idmiss), 1, which)
  numpat <- length(pat_n)

  if (nrow(newdata) == 0)
    stop("No valid rows of data left.")
  if (numpat == 1)
    stop("Only one missing data pattern.")
  if (any(pat_n < 2))
    stop("At least 2 cases needed in each missing data pattern.")

# Perform Hawkins test ----------------------------------------------------

  hawklist <- lapply(imputed, function(thisimp){
    hawkins(thisimp, which_pat)
  })

  if(method %in% c("auto", "hawkins")) {
    pvalsn <- sapply(hawklist, function(thisimp){
      sapply(thisimp[["a"]], function(thistail){
        ni = length(thistail)
        pn <- p_neyman(thistail, replications, use_chisq)
        pn + (pn == 0)/replications
        })
    })
    out$hawk_chisq <- -2 * colSums(log(pvalsn))
    out$hawk_df <- 2 * numpat
    out$hawk_p <- pchisq(out$hawk_chisq, out$hawk_df, lower.tail = FALSE)
  }

# Perform Anderson-Darling test -------------------------------------------

  if((method == "auto" & any(out$hawk_p < alpha)) | method == "nonparametric"){
    adout <- sapply(hawklist, function(thisimp){
      anderson_darling(thisimp[["fij"]]) # First row is p
    })
    out$ad_value  <- colSums(adout[-1, , drop = FALSE])
    out$ad_p <- adout[1, ]
  }
  class(out) <- c("mcar_object", class(out))
  out
}



#' @method print mcar_object
#' @export
print.mcar_object <- function(x, ...) {
  ni <- x$pat_n
  out <- "\nInterpretation of results:\n"
  cat("\nMissing data patterns:", (nrow(x$md.pattern)-1))
  if(!is.null(x$removed_patterns)) cat(" used,", nrow(x$removed_patterns), "removed.")
  cat("\nCases used:", sum(!x$removed_rows), "\n\n")
  if (!is.null(x$hawk_p)){
    if(length(x$hawk_p) > 1){
      hawkp <- median(x$hawk_p)
      cat("Hawkins' test: median chi^2 (", x$hawk_df, ") = ", median(x$hawk_chisq), ", median p = ", hawkp, sep = "")
      if(any(x$hawk_p < x$alpha) & !hawkp < x$alpha) cat(". Some p-values for Hawkins' test were significant; please inspect their values, e.g., using `plot(", deparse(substitute(x)), ")`", sep = "")
    } else {
      hawkp <- x$hawk_p
      cat("Hawkins' test: chi^2 (", x$hawk_df, ") = ", x$hawk_chisq, ", p = ", x$hawk_p, sep = "")
    }
    cat("\n\n")
    if(x$method == "auto"){
      out <- c(out,
               c("Hawkins' test is not significant; there is no evidence to reject the assumptions of multivariate normality and MCAR.\n",
                 "Hawkins' test is significant; if multivariate normality can be assumed, then reject the assumption that missingness is MCAR.\n")[(hawkp < x$alpha)+1])
    }
  }
  if (!is.null(x$ad_p)){
    if(length(x$ad_p) > 1){
      adp <- median(x$ad_p)
      cat("Anderson-Darling rank test: median T = ", median(x$ad_value), ", median p = ", median(x$ad_p), sep = "")
      if(any(x$ad_p < x$alpha) & !median(x$ad_p) < x$alpha) cat(". Some p-values for the Anderson-Darling test were significant; please inspect their values, e.g., using `plot(", deparse(substitute(x)), ")`", sep = "")
    } else {
      adp <- x$ad_p
      cat("Anderson-Darling rank test: T = ", x$ad_value, ", p = ", x$ad_p, sep = "")
    }
    cat("\n")
    if(x$method == "auto"){
      out <- c(out,
               c("Anderson-Darling test is not significant. There is thus evidence against multivariate normality, but not against MCAR.\n",
                 "Anderson-Darling test is significant. Reject the assumption that missingness is MCAR.")[(adp < x$alpha)+1])
    }
  }
  if(x$method == "auto"){
    cat(out)
  }
}

#' @importFrom graphics hist
#' @importFrom grDevices dev.off
#' @method plot mcar_object
#' @export
plot.mcar_object <- function(x, y, type = NULL, ...){
  if(isTRUE(type == "md.pattern")){
    plot(x[["md.pattern"]])
  } else {
    op <- par(mar = rep(0, 4))
    on.exit(par(op))
    dev.off()
    if(!is.null(x$hawk_p) & !is.null(x$ad_p)) par(mfrow=c(2,1))
    if(!is.null(x$hawk_p)){
      pct <- sum(x$hawk_p < x$alpha)/length(x$hawk_p)
      hist(x$hawk_p, main = NULL, xlab = paste0("Hawkins p-values, ", round(pct*100), "% significant"), ylab = NULL)
      abline(v = x$alpha, col = "red")
    }
    if(!is.null(x$ad_p)){
      pct <- sum(x$ad_p < x$alpha)/length(x$ad_p)
      hist(x$ad_p, main = NULL, xlab = paste0("Anderson-Darling p-values, ", round(pct*100), "% significant"), ylab = NULL)
      abline(v = x$alpha, col = "red")
    }
  }
}

hawkins <- function(x, grouping){
  p <- ncol(x)
  n <- nrow(x)
  x <- split(x, factor(grouping))
  g <- length(x)
  S_pooled <- lapply(x, function(i){
    (nrow(i) - 1) * cov(i, use = "complete.obs")
  })
  S_pooled <- Reduce("+", S_pooled)
  S_pooled <- S_pooled/(n - g)
  S_pooled <- solve(S_pooled)
  f <- lapply(x, function(i){
    i_centered <- scale(i, center = TRUE, scale = FALSE)
    i_centered <- apply(i_centered %*% S_pooled * i_centered, 1, sum)
    i_centered <- i_centered * nrow(i)
    ((n - g - p) * i_centered)/(p * ((nrow(i) - 1) * (n - g) - i_centered))
  })
  a <- lapply(f, function(thisf){ 1 - pf(thisf, p, (n-g - p)) })
  list(fij = f, a = a, ni = matrix(sapply(x, nrow), ncol = 1))
}

p_neyman <- function(x, replications = 10000, use_chisq = 30)
{
  n <- length(x)
  n4 <- sum(colSums(legendre(x, 4))^2)/n
  if (n < use_chisq) {
    sum(sim_neyman(n, replications) > n4)/replications
  } else {
    pchisq(n4, 4, lower.tail = FALSE)
  }
}

sim_neyman <- function (n, replications)
{
  x <- matrix(runif(replications * n), ncol = replications)
  pi <- apply(x, 2, function(i){
    sum(colSums(legendre(i, 4))^2)/n
  })
  sort(pi)
}

anderson_darling <- function (fij){
  x <- unlist(fij)
  ni <- sapply(fij, length)
  if (length(ni) < 2) {
    stop("At least 2 groups required for Anderson-Darling test.")
  }
  k <- length(ni)
  n <- length(x)
  x.sort <- sort(x)[-n]
  hj <- rle(x.sort)$lengths
  hn <- cumsum(hj)
  zj <- x.sort[which(!duplicated(x.sort))]
  adk.all <- sapply(fij, function(fi){
    ni <- length(fi)
    combs <- expand.grid(zj, fi)
    b <- combs[, 1] == combs[, 2]
    thisfij <- rowSums(matrix(b, length(zj)))
    mij <- cumsum(thisfij)
    num <- (n * mij - ni * hn)^2
    den <- hn * (n - hn)
    (1/ni * sum(hj * (num/den)))
  })
  adk <- sum(adk.all)/n
  adk.all <- adk.all/n
  j <- sum(1/ni)
  h <- sum(1/seq(1:(n - 1)))
  g <- sum(sapply(1:(n - 2), function(i){
    (1/(n - i)) * sum(1/seq((i + 1), (n - 1)))
  }))
  a <- (4 * g - 6) * (k - 1) + (10 - 6 * g) * j
  b <- (2 * g - 4) * k^2 + 8 * h * k + (2 * g - 14 * h - 4) *
    j - 8 * h + 4 * g - 6
  c <- (6 * h + 2 * g - 2) * k^2 + (4 * h - 4 * g + 6) * k +
    (2 * h - 6) * j + 4 * h
  d <- (2 * h + 6) * k^2 - 4 * h * k
  var.adk <- max(((a * n^3) + (b * n^2) + (c * n) + d)/((n - 1) *
                                                          (n - 2) * (n - 3)), 0)
  adk.s <- (adk - (k - 1))/sqrt(var.adk)
  b0 <- c(0.675, 1.281, 1.645, 1.96, 2.326)
  b1 <- c(-0.245, 0.25, 0.678, 1.149, 1.822)
  b2 <- c(-0.105, -0.305, -0.362, -0.391, -0.396)
  c0 <- c(1.09861228866811, 2.19722457733622, 2.94443897916644, 3.66356164612965,
          4.59511985013459)
  qnt <- b0 + b1/sqrt(k - 1) + b2/(k - 1)
  ind <- seq(1:4) + (adk.s <= qnt[3])
  yy <- spline(qnt[ind], c0[ind], xout = adk.s)$y
  p <- 1/(1 + exp(yy))
  c(p, adk.all)
}

ad <- function (fij){
  x <- unlist(fij)
  ni <- sapply(fij, length)
  if (length(ni) < 2) {
    stop("At least 2 groups required for Anderson-Darling test.")
  }
  k <- length(ni)
  n <- length(x)
  x.sort <- sort(x)[-n]
  hj <- rle(x.sort)$lengths
  hn <- cumsum(hj)
  zj <- x.sort[which(!duplicated(x.sort))]
  adk.all <- sapply(fij, function(fi){
    ni <- length(fi)
    combs <- expand.grid(zj, fi)
    b <- combs[, 1] == combs[, 2]
    thisfij <- rowSums(matrix(b, length(zj)))
    mij <- cumsum(thisfij)
    num <- (n * mij - ni * hn)^2
    den <- hn * (n - hn)
    (1/ni * sum(hj * (num/den)))
  })
  adk <- sum(adk.all)/n
  adk.all <- adk.all/n
  j <- sum(1/ni)
  h <- sum(1/seq(1:(n - 1)))
  g <- sum(sapply(1:(n - 2), function(i){
    (1/(n - i)) * sum(1/seq((i + 1), (n - 1)))
  }))
  a <- (4 * g - 6) * (k - 1) + (10 - 6 * g) * j
  b <- (2 * g - 4) * k^2 + 8 * h * k + (2 * g - 14 * h - 4) *
    j - 8 * h + 4 * g - 6
  c <- (6 * h + 2 * g - 2) * k^2 + (4 * h - 4 * g + 6) * k +
    (2 * h - 6) * j + 4 * h
  d <- (2 * h + 6) * k^2 - 4 * h * k
  var.adk <- max(((a * n^3) + (b * n^2) + (c * n) + d)/((n - 1) *
                                                      (n - 2) * (n - 3)), 0)
  adk.s <- (adk - (k - 1))/sqrt(var.adk)
  b0 <- c(0.675, 1.281, 1.645, 1.96, 2.326)
  b1 <- c(-0.245, 0.25, 0.678, 1.149, 1.822)
  b2 <- c(-0.105, -0.305, -0.362, -0.391, -0.396)
  c0 <- c(1.09861228866811, 2.19722457733622, 2.94443897916644, 3.66356164612965,
          4.59511985013459)
  qnt <- b0 + b1/sqrt(k - 1) + b2/(k - 1)
  ind <- seq(1:4) + (adk.s <= qnt[3])
  yy <- spline(qnt[ind], c0[ind], xout = adk.s)$y
  p <- 1/(1 + exp(yy))
  list(pn = p, adk.all = adk.all, adk = adk, var.sdk = var.adk)
}

#' @method plot md.pattern
#' @export
plot.md.pattern <- function(x, y, rotate.names = FALSE, ...) {
  op <- par(mar = rep(0, 4))
  on.exit(par(op))
  plot.new()
  R <- x[1:nrow(x) - 1, 1:ncol(x) - 1]
  nmis <- x[nrow(x), 1:ncol(x) - 1]
  if (rotate.names) {
    adj <- c(0, 0.5)
    srt <- 90
    length_of_longest_colname <- max(nchar(colnames(x))) / 2.6
    plot.window(
      xlim = c(-1, ncol(R) + 1),
      ylim = c(-1, nrow(R) + length_of_longest_colname),
      asp = 1
    )
  } else {
    adj <- c(0.5, 0)
    srt <- 0
    plot.window(
      xlim = c(-1, ncol(R) + 1),
      ylim = c(-1, nrow(R) + 1),
      asp = 1
    )
  }
  M <- cbind(c(row(R)), c(col(R))) - 1
  shade <- ifelse(R[nrow(R):1,], mdc(1), mdc(2))
  rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
  for (i in 1:ncol(R)) {
    text(i - .5,
         nrow(R) + .3,
         colnames(x)[i],
         adj = adj,
         srt = srt)
    text(i - .5,-.3, nmis[order(nmis)][i])
  }
  for (i in 1:nrow(R)) {
    text(ncol(R) + .3, i - .5, x[(nrow(x) - 1):1, ncol(x)][i], adj = 0)
    text(-.3, i - .5, rownames(x)[(nrow(x) - 1):1][i], adj = 1)
  }
  text(ncol(R) + .3,-.3, x[nrow(x), ncol(x)])
}
