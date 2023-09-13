context("mice.impute.panImpute")

data <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
type <- c(2, 0, 0, 0, -2, 0, 1, 1, 0)
names(type) <- names(data)

z1 <- mice.impute.panImpute(data = data, type = type, format = "native")

test_that("panImpute returns native class", {
  expect_is(z1, "mitml")
})

blocks <- make.blocks(list(c("bmi", "chl", "hyp"), "age"))
method <- c("panImpute", "pmm")
pred <- make.predictorMatrix(nhanes, blocks)
pred[c("bmi", "chl", "hyp"), "hyp"] <- -2
imp1 <- mice(nhanes,
             blocks = blocks, method = method, pred = pred,
             maxit = 1, seed = 1, print = FALSE)
z <- complete(imp1)

test_that("mice can call panImpute with type argument", {
  expect_equal(sum(is.na(z$bmi)), 0)
  expect_equal(sum(is.na(z$chl)), 0)
})

method <- c("panImpute", "pmm")
formulas <- list(bmi + chl + hyp ~ 1 | age,
                 age ~ bmi + chl + hyp)
formulas <- name.formulas(formulas)
imp2 <- mice(nhanes, formulas = formulas, method = method, maxit = 1, seed = 1, print = FALSE)
z <- complete(imp2)

test_that("mice can call panImpute with formula argument", {
  expect_equal(sum(is.na(z$bmi)), 0)
  expect_equal(sum(is.na(z$chl)), 0)
})
