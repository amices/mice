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

# single-row new data, correct typ
newdata <- make.data(models = trained$models, vars = names(df))
result1 <- scan.types(data = newdata, models = trained$models, coerce = TRUE)

test_that("scan.types() does not alter new data that has correct type", {
  expect_true(identical(attr(result1, "data"), newdata))
})

test_that("scan.types() sets can_fill to Y for correctly typed newdata", {
  expect_true(length(result1$can_fill) > 0)
  expect_true(all(result1$can_fill == "Y"))
})

# single-row new data, wrong types
newdata <- data.frame(
  factor2 = NA,
  factor3 = NA,
  factor4 = NA,
  logical1 = NA,
  logical2 = NA,
  numeric1 = NA)

result2 <- scan.types(data = newdata, models = trained$models, coerce = TRUE)

test_that("scan.types) coerces new data of wrong types to correct types", {
  expect_false(identical(attr(result2, "data"), newdata))
  expect_true(identical(attr(result1, "data"), attr(result2, "data")))
})

test_that("scan.types() sets can_fill to Y for coerced data", {
  expect_true(all(result2$can_fill == "Y"))
})
