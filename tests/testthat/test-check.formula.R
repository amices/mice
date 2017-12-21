context("check.formulas")

data <- nhanes
where <- is.na(data)

# blocks <- name.blocks(list("bmi", "age", "chl"))
# ini <- mice(data, blocks = blocks, maxit = 0)
# 
# # classic type specification
# setup <- list(blocks = blocks, 
#               predictorMatrix = ini$predictorMatrix,
#               formulas = NULL)
# # v1 <- mice:::check.formulas(setup, data)
# 
# # using a formula
# #formulas <- v1$formulas
# setup <- list(blocks = blocks, 
#               predictorMatrix = ini$predictorMatrix,
#               formulas = formulas)
# #v2 <- mice:::check.formulas(setup, data)
# #v2$formulas
# 
# test_that("updates `mode.formula` attribute", {
#   # expect_false(identical(v2$formulas, v2$formulas.arg))
# #  expect_identical(v2$formulas[[1]], v2$formulas.arg[[1]])
# })
# 
# # try dot in formula
# formulas <- list(bmi ~ ., age ~ ., chl ~ .)
# formulas <- name.formulas(formulas)
# setup <- list(blocks = blocks, 
#               predictorMatrix = ini$predictorMatrix,
#               formulas = formulas)
# #v3 <- mice:::check.formulas(setup, data)
# #v3$formulas
# 
# # classic specification using predictorMatrix
# imp1 <- mice(nhanes, seed = 51212, print = FALSE, m = 1)
# cmp1 <- complete(imp1)
# 
# # formula specification
# form <- list(age ~ ., bmi ~ ., hyp ~., chl ~ .)
# imp2 <- mice(nhanes, formulas = form, seed = 51212, print = FALSE, m = 1)
# cmp2 <- complete(imp2)
# 
# test_that("predictorMatrix and formula yield same imputations", {
#   expect_identical(cmp1, cmp2)
#   expect_identical(imp1$imp, imp2$imp)
# })

# formula specification
form <- name.blocks(list(bmi ~ ., hyp ~., chl ~ .))
imp3 <- mice(nhanes, formulas = form, seed = 51212, print = FALSE, m = 1)
cmp3 <- complete(imp3)

# old.form <- c("", "bmi ~ chl + hyp", "hyp ~ bmi + chl", "chl ~ bmi + hyp")
# imp <- mice(nhanes, formula = old.form, m = 1, maxit = 2, print = FALSE)
# 
# form1 <- list(bmi = ~ 1, chl = ~ 1, hyp = ~ 1)
# # impute given predictors
# imp1 <- mice(nhanes, formula = form1, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, seed = 1)
# # impute the mean
# imp2 <- mice(nhanes, formula = form1, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, seed = 1)
# 
# form2 <- list(bmi = "hyp ~ 1", chl = "hyp ~ 1", hyp = "hyp ~ 1")
# imp3 <- mice(nhanes, formula = form2, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, seed = 1)
# 

