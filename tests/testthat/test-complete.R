context("complete")

imp <- mice(nhanes, maxit = 1, m = 2, seed = 123, print = FALSE)

lng <- subset(complete(imp, "long"), .imp == 1, select = c(age, bmi, hyp, chl))
all <- complete(imp, "all")[[1]]

test_that("long and all produce same data", {
  expect_equal(lng, all)
})

# preserve rownames type
nhanes3 <- mice::nhanes
row.names(nhanes3) <- seq_len(nrow(nhanes3))
imp <- mice(nhanes, maxit = 1, m = 2, seed = 123, print = FALSE)
imp3 <- mice(nhanes3, maxit = 1, m = 2, seed = 123, print = FALSE)
test_that("mice() preserves the rownames type attribute", {
  expect_type(attr(imp$data, "row.names"), "character")
  expect_type(attr(imp3$data, "row.names"), "integer")
})
cmp <- complete(imp, "long")
cmp3 <- complete(imp3, "long")
test_that("complete() preserves the rownames type attribute", {
  expect_type(attr(cmp, "row.names"), "character")
  expect_type(attr(cmp3, "row.names"), "integer")
})

# mids workflow using saved objects
imp <- mice(nhanes, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
est <- pool(fit)
est.mice <- est

# mild workflow using saved objects and base::lapply
idl <- complete(imp, "all")
fit <- lapply(idl, lm, formula = chl ~ age + bmi + hyp)
est <- pool(fit)
est.mild <- est

# long workflow using base::by
cmp <- complete(imp, "long")
fit <- by(cmp, as.factor(cmp$.imp), lm, formula = chl ~ age + bmi + hyp)
est <- pool(fit)
est.long <- est

test_that("workflow mids, mild and long produce same estimates", {
  expect_identical(getqbar(est.mice), getqbar(est.mild))
  expect_identical(getqbar(est.mice), getqbar(est.long))
})
