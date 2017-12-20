context("mice.impute.panImpute")

data <- boys[c(1:10, 101:110, 501:510, 601:620, 701:710), ]
type <- c(2, 0, 0, 0, -2, 0, 1, 1, 0)
names(type) <- names(data)
  
z1 <- mice.impute.panImpute(data = data, type = type, format = "native")

test_that("panImpute returns native class", {
  expect_is(z1, "mitml")
})

blocks <-  make.blocks(list(c("bmi", "chl", "hyp"), "age"))
method <- c("panImpute", "pmm")
pred <- make.predictorMatrix(nhanes, blocks)
pred["B1", "hyp"] <- -2
imp <- mice(nhanes, blocks = blocks, method = method, pred = pred, 
            maxit = 1, seed = 1, print = FALSE)
z <- complete(imp)

test_that("mice can call panImpute", {
  expect_equal(sum(is.na(z$bmi)), 0)
  expect_equal(sum(is.na(z$chl)), 0)
})
