suppressWarnings(RNGversion("3.5.0"))

# ---- set-up ---------------------------------------------------------------

# load data
dat <- mice::nhanes

# add indicator training and test
# first 20 for training, last 5 for testing
dat$set <- c(rep("train", 20), rep("test", 5))
# the outcome is unknown for the test cases
dat$age[dat$set == "test"] <- NA

# Make prediction matrix and ensure that set is not used as a predictor
predmat <- mice::make.predictorMatrix(dat)
predmat[, "set"] <- 0

# train imputation model
m <- 30
imp <- mice(
  dat,
  m = m,
  maxit = 5,
  seed = 1,
  print = FALSE,
  predictorMatrix = predmat,
  ignore = ifelse(dat$set == "test", TRUE, FALSE)
)

# extract the training and test data sets
impdats <- mice::complete(imp, "all")
traindats <- lapply(impdats, function(dat) {
  subset(dat, set == "train", select = -set)
})
testdats <- lapply(impdats, function(dat) {
  subset(dat, set == "test", select = -c(set, age))
})
ntest <- nrow(testdats[[1]])

# predict age with other variables with training datasets
fits <- lapply(traindats, function(dat) lm(age ~ bmi + hyp + chl, data = dat))

# reference quantities, computed without predict_mi()
ref <- Map(function(f, nd) predict(f, newdata = nd, se.fit = TRUE), fits, testdats)
ref_fit <- sapply(ref, function(x) x$fit)                           # ntest x m
ref_se <- sapply(ref, function(x) x$se.fit)                         # ntest x m
ref_scale <- sapply(ref, function(x) sqrt(x$se.fit^2 + x$residual.scale^2))
ref_df <- ref[[1]]$df
# cdf of the mixture of t-distributions, evaluated at q (one value per test case)
mixture_cdf <- function(q) {
  unname(rowMeans(pt((q - ref_fit) / ref_scale, ref_df)))
}

# pool the predictions with function
# (m = 30 is just below what the diagnostic asks for here, hence suppressWarnings)
pool_preds <- suppressWarnings(predict_mi(
  object = fits,
  newdata = testdats,
  pool = TRUE,
  interval = "prediction",
  level = 0.95
))

# ---- structure ------------------------------------------------------------

test_that("PREDICT_MI-001: Output class is correct", {
  expect_type(pool_preds, "double") # checks storage type
  expect_true(is.matrix(pool_preds)) # checks it's a matrix
  expect_equal(dim(pool_preds), c(ntest, 3L))
  expect_equal(colnames(pool_preds), c("fit", "lwr", "upr"))
  expect_false(anyNA(pool_preds))
})

test_that("PREDICT_MI-002: Without interval a vector of pooled predictions is returned", {
  p <- predict_mi(fits, testdats)
  expect_true(is.numeric(p) && !is.matrix(p))
  expect_length(p, ntest)
  expect_equal(unname(p), unname(rowMeans(ref_fit)))
})

# ---- prediction interval --------------------------------------------------

test_that("PREDICT_MI-003: Pooled prediction is the mean over imputations", {
  expect_equal(unname(pool_preds[, "fit"]), unname(rowMeans(ref_fit)))
})

test_that("PREDICT_MI-004: Prediction bounds are quantiles of the mixture of t-distributions", {
  expect_equal(mixture_cdf(pool_preds[, "lwr"]), rep(0.025, ntest), tolerance = 1e-5)
  expect_equal(mixture_cdf(pool_preds[, "upr"]), rep(0.975, ntest), tolerance = 1e-5)
  expect_true(all(pool_preds[, "lwr"] < pool_preds[, "fit"]))
  expect_true(all(pool_preds[, "fit"] < pool_preds[, "upr"]))
})

test_that("PREDICT_MI-005: Prediction bounds lie between the smallest and largest component quantiles", {
  lo <- ref_fit + ref_scale * qt(0.025, ref_df)
  hi <- ref_fit + ref_scale * qt(0.975, ref_df)
  expect_true(all(pool_preds[, "lwr"] >= apply(lo, 1, min)))
  expect_true(all(pool_preds[, "lwr"] <= apply(lo, 1, max)))
  expect_true(all(pool_preds[, "upr"] >= apply(hi, 1, min)))
  expect_true(all(pool_preds[, "upr"] <= apply(hi, 1, max)))
})

test_that("PREDICT_MI-006: `level` is respected", {
  p80 <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", level = 0.8))
  expect_equal(mixture_cdf(p80[, "lwr"]), rep(0.1, ntest), tolerance = 1e-5)
  expect_equal(mixture_cdf(p80[, "upr"]), rep(0.9, ntest), tolerance = 1e-5)
  expect_true(all(p80[, "upr"] - p80[, "lwr"] < pool_preds[, "upr"] - pool_preds[, "lwr"]))
})

test_that("PREDICT_MI-007: `iterations` controls the bisection", {
  p1 <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", iterations = 1))
  p40 <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", iterations = 40))
  expect_false(isTRUE(all.equal(p1, pool_preds)))
  expect_equal(p40, pool_preds, tolerance = 1e-5)
  expect_equal(mixture_cdf(p40[, "lwr"]), rep(0.025, ntest), tolerance = 1e-8)
  # non-integer values are truncated
  p20 <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", iterations = 20.7))
  expect_equal(p20, pool_preds)
  # values below 1 would skip the bisection and are rejected
  expect_error(predict_mi(fits, testdats, interval = "prediction", iterations = 0), "at least 1")
  expect_error(predict_mi(fits, testdats, interval = "prediction", iterations = 0.5), "at least 1")
  expect_error(predict_mi(fits, testdats, interval = "prediction", iterations = "a"), "at least 1")
  expect_error(predict_mi(fits, testdats, interval = "prediction", iterations = c(5, 20)), "at least 1")
})

test_that("PREDICT_MI-008: A single new case gives the same result as that row of a larger set", {
  one <- lapply(testdats, function(x) x[2, , drop = FALSE])
  p <- suppressWarnings(predict_mi(fits, one, interval = "prediction"))
  expect_true(is.matrix(p))
  expect_equal(dim(p), c(1L, 3L))
  expect_equal(unname(p[1, ]), unname(pool_preds[2, ]))
  ci <- predict_mi(fits, one, interval = "confidence")
  expect_equal(dim(ci), c(1L, 3L))
})

# ---- probs ----------------------------------------------------------------

test_that("PREDICT_MI-009: `probs` returns named mixture quantiles and overrules `level`", {
  probs <- c(0.1, 0.5, 0.9)
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", probs = probs, level = 0.5))
  expect_equal(dim(p), c(ntest, 4L))
  expect_equal(colnames(p)[-1], as.character(probs))
  for (pr in probs) {
    expect_equal(mixture_cdf(p[, as.character(pr)]), rep(pr, ntest), tolerance = 1e-5)
  }
  expect_true(all(p[, "0.1"] < p[, "0.5"] & p[, "0.5"] < p[, "0.9"]))
  expect_equal(unname(p[, 1]), unname(rowMeans(ref_fit)))
})

test_that("PREDICT_MI-010: `probs` equal to the default tails reproduces the default interval", {
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", probs = c(0.025, 0.975)))
  expect_equal(unname(p[, 2:3]), unname(pool_preds[, c("lwr", "upr")]))
})

test_that("PREDICT_MI-011: A single probability works without a warning from `probs`", {
  # complete, identical new data: no between-imputation variance, so no MC warning either
  nd <- testdats[[1]]
  expect_warning(p <- predict_mi(fits, nd, interval = "confidence", probs = 0.5), NA)
  expect_equal(dim(p), c(ntest, 2L))
  expect_equal(unname(p[, 2]), unname(p[, 1]))
})

test_that("PREDICT_MI-012: Invalid `probs` are rejected", {
  msg <- "between 0 and 1"
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = c(-0.1, 0.5)), msg)
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = 1.5), msg)
  # the bounds themselves would give infinite quantiles
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = c(0, 1)), msg)
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = NA), msg)
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = c(0.1, NA)), msg)
  expect_error(predict_mi(fits, testdats, interval = "prediction", probs = "a"), msg)
})

test_that("PREDICT_MI-013: `probs` without an interval warns and returns the pooled predictions", {
  expect_warning(p <- predict_mi(fits, testdats, probs = c(0.1, 0.9)), "probs")
  expect_equal(p, predict_mi(fits, testdats))
})

# ---- confidence interval, se.fit, df --------------------------------------

rr <- lapply(seq_len(ntest), function(i) {
  mice::pool.scalar(ref_fit[i, ], ref_se[i, ]^2, n = ref_df + 1, k = 1)
})
rr_t <- sapply(rr, function(x) x$t)
rr_df <- sapply(rr, function(x) x$df)

test_that("PREDICT_MI-014: Confidence interval follows Rubin's rules", {
  ci <- predict_mi(fits, testdats, interval = "confidence")
  expect_equal(colnames(ci), c("fit", "lwr", "upr"))
  expect_equal(unname(ci[, "fit"]), unname(rowMeans(ref_fit)))
  expect_equal(unname(ci[, "lwr"]), unname(rowMeans(ref_fit)) + qt(0.025, rr_df) * sqrt(rr_t))
  expect_equal(unname(ci[, "upr"]), unname(rowMeans(ref_fit)) + qt(0.975, rr_df) * sqrt(rr_t))
})

test_that("PREDICT_MI-015: `se.fit = TRUE` returns pooled standard errors and Barnard-Rubin df", {
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", se.fit = TRUE))
  expect_type(p, "list")
  expect_true(all(c("fit", "se.fit", "df", "residual.scale") %in% names(p)))
  expect_equal(unname(p$se.fit), sqrt(rr_t))
  expect_equal(unname(p$df), rr_df)
  expect_equal(p$residual.scale, sqrt(mean(sapply(ref, function(x) x$residual.scale^2))))
  expect_equal(unname(p$fit), unname(pool_preds))
  # the between-imputation variance is included
  expect_true(all(p$se.fit >= sqrt(rowMeans(ref_se^2))))
})

# ---- methods --------------------------------------------------------------

test_that("PREDICT_MI-016: list and mira objects give identical results", {
  mira <- mice::as.mira(fits)
  expect_equal(suppressWarnings(predict_mi(mira, testdats, interval = "prediction")), pool_preds)
  expect_equal(
    suppressWarnings(predict_mi(mira, testdats, interval = "prediction", probs = c(0.2, 0.8), iterations = 10)),
    suppressWarnings(predict_mi(fits, testdats, interval = "prediction", probs = c(0.2, 0.8), iterations = 10))
  )
})

test_that("PREDICT_MI-017: A single lm with multiply imputed new data is pooled as a mixture", {
  fit1 <- fits[[1]]
  r1 <- lapply(testdats, function(nd) predict(fit1, newdata = nd, se.fit = TRUE))
  f1 <- sapply(r1, function(x) x$fit)
  s1 <- sapply(r1, function(x) sqrt(x$se.fit^2 + x$residual.scale^2))
  cdf1 <- function(q) unname(rowMeans(pt((q - f1) / s1, fit1$df.residual)))

  p <- suppressWarnings(predict_mi(fit1, testdats, interval = "prediction"))
  expect_equal(colnames(p), c("fit", "lwr", "upr"))
  expect_equal(unname(p[, "fit"]), unname(rowMeans(f1)))
  expect_equal(cdf1(p[, "lwr"]), rep(0.025, ntest), tolerance = 1e-5)
  expect_equal(cdf1(p[, "upr"]), rep(0.975, ntest), tolerance = 1e-5)

  # arguments are passed on by the lm method
  q <- suppressWarnings(predict_mi(fit1, testdats, interval = "prediction", probs = c(0.1, 0.9)))
  expect_equal(colnames(q)[-1], c("0.1", "0.9"))
  expect_equal(cdf1(q[, "0.1"]), rep(0.1, ntest), tolerance = 1e-5)
  p1 <- suppressWarnings(predict_mi(fit1, testdats, interval = "prediction", iterations = 1))
  expect_false(isTRUE(all.equal(p1, p)))
})

test_that("PREDICT_MI-018: Without between-imputation variance the mixture equals predict.lm", {
  # one model, the same complete new data in every 'imputation'
  nd <- testdats[[1]]
  same <- rep(list(nd), 5)
  expect_warning(p <- predict_mi(fits[[1]], same, interval = "prediction"), NA)
  expect_equal(unname(p), unname(predict(fits[[1]], newdata = nd, interval = "prediction")), tolerance = 1e-6)
})

test_that("PREDICT_MI-019: A single lm with ordinary new data warns and returns predict.lm output", {
  nd <- testdats[[1]]
  expect_warning(p <- predict_mi(fits[[1]], nd, interval = "prediction"), "multiple")
  expect_equal(p, predict(fits[[1]], newdata = nd, interval = "prediction"))
  # `probs` and `nsamples` do not apply here, which is signalled
  w <- character()
  withCallingHandlers(
    predict_mi(fits[[1]], nd, probs = 0.5, nsamples = 10),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("probs", w)))
})

test_that("PREDICT_MI-020: `pool = FALSE` returns one prediction per imputation", {
  p <- predict_mi(fits, testdats, pool = FALSE, interval = "prediction")
  expect_type(p, "list")
  expect_length(p, m)
  expect_equal(unname(p[[3]]), unname(predict(fits[[3]], newdata = testdats[[3]], interval = "prediction")))
  expect_warning(predict_mi(fits, testdats, pool = FALSE, se.fit = TRUE), "se.fit")
  # no warning without `probs` and `nsamples`
  expect_warning(predict_mi(fits, testdats, pool = FALSE), NA)
  expect_warning(predict_mi(fits[[1]], testdats, pool = FALSE), NA)
})

test_that("PREDICT_MI-021: `probs` and `nsamples` warn with `pool = FALSE` and leave the output unchanged", {
  p <- predict_mi(fits, testdats, pool = FALSE)
  expect_warning(q <- predict_mi(fits, testdats, pool = FALSE, probs = 0.5), "pool = FALSE")
  expect_equal(q, p)
  expect_warning(q <- predict_mi(fits, testdats, pool = FALSE, nsamples = 10), "pool = FALSE")
  expect_equal(q, p)
  # also for a single lm with multiply imputed new data
  expect_warning(predict_mi(fits[[1]], testdats, pool = FALSE, probs = 0.5), "pool = FALSE")
  expect_warning(predict_mi(fits[[1]], testdats, pool = FALSE, nsamples = 10), "pool = FALSE")
})

test_that("PREDICT_MI-022: Invalid input is rejected", {
  expect_error(predict_mi(fits, testdats[1:5], interval = "prediction"), "different lengths")
  expect_error(predict_mi(list(1, 2), testdats[1:2]))
  expect_error(predict_mi(fits, testdats, interval = "foo"))
})

# ---- warning on the number of imputations ----------------------------------

test_that("PREDICT_MI-023: Too few imputations give a warning that names a larger m", {
  few <- 1:5
  expect_warning(
    predict_mi(fits[few], testdats[few], interval = "prediction"),
    "number of imputations"
  )
  w <- tryCatch(
    predict_mi(fits[few], testdats[few], interval = "prediction"),
    warning = function(w) conditionMessage(w)
  )
  suggested <- as.numeric(sub(".*m = ([0-9]+).*", "\\1", w))
  expect_true(suggested > length(few))
})

test_that("PREDICT_MI-024: No such warning for confidence intervals or without interval", {
  few <- 1:5
  expect_warning(predict_mi(fits[few], testdats[few], interval = "confidence"), NA)
  expect_warning(predict_mi(fits[few], testdats[few]), NA)
})

# ---- type = "terms" ---------------------------------------------------------

test_that("PREDICT_MI-025: type = 'terms' returns pooled terms with bounds", {
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", type = "terms"))
  expect_type(p, "list")
  expect_equal(names(p), c("fit", "lwr", "upr"))
  expect_equal(dim(p$fit), c(ntest, 3L))
  expect_equal(colnames(p$fit), c("bmi", "hyp", "chl"))
  expect_equal(dim(p$lwr), dim(p$fit))
  expect_equal(dim(p$upr), dim(p$fit))
  expect_true(all(p$lwr < p$fit & p$fit < p$upr))
  # terms plus the constant add up to the pooled prediction
  expect_equal(unname(rowSums(p$fit) + attr(p$fit, "constant")), unname(rowMeans(ref_fit)))
})

test_that("PREDICT_MI-026: type = 'terms' works with `probs` and with confidence intervals", {
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", type = "terms", probs = c(0.1, 0.9)))
  expect_equal(names(p), c("fit", "0.1", "0.9"))
  expect_true(all(p[["0.1"]] < p[["0.9"]]))
  ci <- predict_mi(fits, testdats, interval = "confidence", type = "terms")
  expect_equal(names(ci), c("fit", "lwr", "upr"))
  expect_equal(dim(ci$lwr), dim(ci$fit))
  expect_true(all(ci$lwr < ci$fit & ci$fit < ci$upr))
})

# ---- samples ----------------------------------------------------------------

test_that("PREDICT_MI-027: `nsamples` returns draws from the posterior predictive distribution", {
  set.seed(123)
  ns <- 400
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", nsamples = ns))
  expect_type(p, "list")
  expect_true(all(c("fit", "samples") %in% names(p)))
  s <- p$samples[[1]]
  expect_equal(dim(s), c(ntest, m, ns))
  # draws for each case are centred on the pooled prediction ...
  draws <- matrix(s, nrow = ntest)
  expect_equal(rowMeans(draws), unname(rowMeans(ref_fit)), tolerance = 0.05)
  # ... and their quantiles agree with the mixture quantiles
  expect_equal(mixture_cdf(apply(draws, 1, quantile, 0.025)), rep(0.025, ntest), tolerance = 0.2)
  expect_equal(mixture_cdf(apply(draws, 1, quantile, 0.975)), rep(0.975, ntest), tolerance = 0.01)
})

test_that("PREDICT_MI-028: `nsamples` works per term, for the mean, and rejects values below 1", {
  set.seed(123)
  # one array per term
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", type = "terms", nsamples = 3))
  expect_equal(names(p$samples), c("bmi", "hyp", "chl"))
  expect_equal(dim(p$samples[["bmi"]]), c(ntest, m, 3L))
  # draws for the mean vary less than draws for new observations
  ns <- 400
  s_new <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", nsamples = ns))$samples[[1]]
  s_mean <- predict_mi(fits, testdats, interval = "confidence", nsamples = ns)$samples[[1]]
  expect_equal(dim(s_mean), c(ntest, m, ns))
  expect_true(all(apply(matrix(s_mean, ntest), 1, sd) < apply(matrix(s_new, ntest), 1, sd)))
  # non-integer values are truncated
  p <- suppressWarnings(predict_mi(fits, testdats, interval = "prediction", nsamples = 2.5))
  expect_equal(dim(p$samples[[1]]), c(ntest, m, 2L))
  expect_error(predict_mi(fits, testdats, interval = "prediction", nsamples = -1), "at least 1")
  expect_error(predict_mi(fits, testdats, interval = "prediction", nsamples = 0.5), "at least 1")
  expect_error(predict_mi(fits, testdats, interval = "prediction", nsamples = "a"), "at least 1")
})