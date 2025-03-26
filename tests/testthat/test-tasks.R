context("tasks")

# We have to test the following cases:

# - Does train-run setup with a factor variable produce imputations? YES
# - Does train-run setup with a factor variable produce imputations when the factor has fewer categories during running than training? YES
# - Does train-run setup with a factor variable produce imputations when the factor has more categories during running than training?

test_that("m filling recycles training models", {
  expect_silent(imp1 <- mice(nhanes2, m = 2, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$bmi[[1]]$lookup))
  expect_silent(imp2 <- mice(nhanes2, m = 4, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  expect_silent(imp2 <- mice(nhanes2[1, ], m = 2, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
})

test_that("fully synthetic datasets can be created from completely observed variables", {
  dataset <- complete(mice(nhanes2, m = 1, maxit = 1, method = "pmm", print = FALSE))
  expect_silent(imp1 <- mice(dataset, m = 2, maxit = 1, task = "train", method = "pmm", print = FALSE))
  expect_false(is.null(imp1$models$age[[1]]$lookup))
  expect_silent(imp2 <- mice(dataset, where = make.where(dataset, "all"), m = 2, maxit = 3, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  synt1 <- complete(imp2, 1)
  synt2 <- complete(imp2, 2)
})

test_that("the procedure informs the user about a mismatch between model and data", {
  expect_silent(imp1 <- mice(nhanes2, m = 3, maxit = 1, task = "train", method = "pmm", print = FALSE))
  newdata <- nhanes2
  newdata$age <- factor(newdata$age, levels = c(levels(newdata$age), "not_a_level"))
  newdata$age[1] <- "not_a_level"
  newdata$age[2] <- NA
  expect_silent(imp2 <- mice(newdata, m = 1, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE))
  newdata <- nhanes2
  levels(newdata$age)[levels(newdata$age) == "60-99"] <- "60+"
  expect_error(imp2 <- mice(newdata, m = 1, maxit = 1, task = "fill", method = "pmm", models = imp1$models, print = FALSE), "Model-Data mismatch")
})

## Check how categorical method handle fills for single row newdata

set.seed(123)
df <- data.frame(
  factor2 = factor(sample(c("Yes", "No"), 20, replace = TRUE)),
  factor3 = factor(sample(c("Low", "Medium", "High"), 20, replace = TRUE)),
  factor4 = factor(sample(c("A", "B", "C", "D"), 20, replace = TRUE), ordered = TRUE),
  logical1 = sample(c(TRUE, FALSE), 20, replace = TRUE),
  logical2 = sample(c(TRUE, FALSE), 20, replace = TRUE),
  numeric1 = rnorm(20)
)
n <- prod(dim(df))
na_count <- round(0.3 * n)
missing_idx <- arrayInd(sample(n, na_count), .dim = dim(df))
for (i in seq_len(nrow(missing_idx))) {
  df[missing_idx[i, 1], missing_idx[i, 2]] <- NA
}

expect_warning(trained <<- mice(df, m = 2, maxit = 2, seed = 1, tasks = "train", print = FALSE))


# single-row new data, wrong types
newdata_wrong <- data.frame(
  factor2 = NA,
  factor3 = NA,
  factor4 = NA,
  logical1 = NA,
  logical2 = NA,
  numeric1 = NA)

# single-row new data, correct types
newdata_correct <- data.frame(
  factor2 = factor(NA, levels = levels(df$factor2)),
  factor3 = factor(NA, levels = levels(df$factor3)),
  factor4 = factor(NA, levels = levels(df$factor4), ordered = TRUE),
  logical1 = as.logical(NA),
  logical2 = as.logical(NA),
  numeric1 = as.numeric(NA))

test_that("df and newdata have same types before fill", {
  expect_false(identical(sapply(df, class), sapply(newdata_wrong, class)))
  expect_true(identical(sapply(df, class), sapply(newdata_correct, class)))
})

# fill0 <- mice(newdata_correct, tasks = "fill", models = trained$models, m = 20, maxit = 0, print = FALSE, seed = 2)
# fill0$imp
#
# fill1 <- mice(newdata_correct, tasks = "fill", models = trained$models, m = 20, maxit = 1, print = FALSE, seed = 2)
# fill1$imp

test_that("Filling logicals work without converting to factors", {
  expect_silent(filled <- mice(newdata_correct, tasks = "fill", models = trained$models, print = FALSE))
  expect_identical(sapply(newdata_correct, class), sapply(complete(filled), class))
})

