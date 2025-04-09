context("scan.newdata, make.newdata, coerce.newdata")

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

# make single-row new data with correct type
newdata <- make.newdata(models = trained$models, vars = names(df))

# run scan on newdata
result1 <- scan.newdata(data = newdata, models = trained$models)

test_that("scan.newdata() sets can_fill to Y for correctly typed newdata", {
  expect_true(length(result1$can_fill) > 0)
  expect_true(all(result1$can_fill))
})

# coerce newdata (not needed here)
coerced1 <- coerce.newdata(data = newdata, models = trained$models)

test_that("coerce.newdata() does not alter when it has correct type", {
  expect_true(identical(coerced1, newdata))
})


# single-row new data, wrong types
newdata <- data.frame(
  factor2 = NA,
  factor3 = NA,
  factor4 = NA,
  logical1 = NA,
  logical2 = NA,
  numeric1 = NA)

result2 <- scan.newdata(data = newdata, models = trained$models)

test_that("scan.newdata() reports that it cannot fill all variables", {
  expect_false(all(result2$can_fill))
})

coerced2 <- coerce.newdata(data = newdata, models = trained$models)

test_that("coerce.newdata() can coerces wrong to correct types", {
  expect_false(identical(attr(result2, "data"), newdata))
  expect_true(identical(coerced1, coerced2))
})

