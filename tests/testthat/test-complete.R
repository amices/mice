test_that("COMPLETE-001: long and all produce same data", {
  imp <- mice(nhanes, maxit = 1, m = 2, seed = 123, print = FALSE)
  lng <- subset(complete(imp, "long"), .imp == 1, select = c(age, bmi, hyp, chl))
  all <- complete(imp, "all")[[1]]
  expect_equal(lng, all)
})

# preserve rownames type
nhanes3 <- mice::nhanes
row.names(nhanes3) <- seq_len(nrow(nhanes3))
imp <- mice(nhanes, maxit = 1, m = 2, seed = 123, print = FALSE)
imp3 <- mice(nhanes3, maxit = 1, m = 2, seed = 123, print = FALSE)
test_that("COMPLETE-002: mice() preserves the rownames type attribute", {
  expect_type(attr(imp$data, "row.names"), "character")
  expect_type(attr(imp3$data, "row.names"), "integer")
})
cmp <- complete(imp, "long")
cmp3 <- complete(imp3, "long")
test_that("COMPLETE-003: complete() preserves the rownames type attribute", {
  expect_type(attr(cmp, "row.names"), "character")
  expect_type(attr(cmp3, "row.names"), "integer")
})

test_that("COMPLETE-004: workflow mids, mild and long produce same estimates", {
  imp <- mice(nhanes, seed = 123, print = FALSE)

  # mids workflow using saved objects
  fit <- with(imp, lm(chl ~ age + bmi + hyp))
  est.mice <- pool(fit)

  # mild workflow using saved objects and base::lapply
  idl <- complete(imp, "all")
  fit <- lapply(idl, lm, formula = chl ~ age + bmi + hyp)
  est.mild <- pool(fit)

  # long workflow using base::by
  cmp <- complete(imp, "long")
  fit <- by(cmp, as.factor(cmp$.imp), lm, formula = chl ~ age + bmi + hyp)
  est.long <- pool(fit)

  expect_identical(getqbar(est.mice), getqbar(est.mild))
  expect_identical(getqbar(est.mice), getqbar(est.long))
})
