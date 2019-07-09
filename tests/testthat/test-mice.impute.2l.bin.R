context("mice.impute.2l.bin")

# suppressPackageStartupMessages(library(miceadds, warn.conflicts = FALSE, quietly = TRUE))
# packageVersion(c("miceadds"))
# suppressPackageStartupMessages(library(micemd, warn.conflicts = FALSE, quietly = TRUE))
# packageVersion(c("micemd"))

# toenail: outcome is factor
data("toenail", package = "HSAUR3")
data <- tidyr::complete(toenail, patientID, visit) %>% 
  tidyr::fill(treatment) %>% 
  dplyr::select(-time) %>%
  dplyr::mutate(patientID = as.integer(patientID))
summary(data)
# fit1 <- glm(outcome ~ treatment * month, data = toenail, family = binomial)
# fit2 <- glm(outcome ~ treatment * visit, data = toenail, family = binomial)
# fit3 <- lme4::glmer(outcome ~ treatment * visit + (1 | ID), data = data, family = binomial)

pred <- make.predictorMatrix(data)
pred["outcome", "patientID"] <- -2

test_that("mice::mice.impute.2l.bin() accepts factor outcome", {
  expect_silent(imp <- mice(data, method = "2l.bin", print = FALSE, pred = pred, m = 1, maxit = 1))
  expect_false(anyNA(complete(imp)))
})

# test_that("miceadds::mice.impute.2l.binary() accepts factor outcome", {
#   expect_silent(imp <- mice(data, method = "2l.binary", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
# })

# test_that("micemd::mice.impute.2l.2stage.bin() accepts factor outcome", {
#   expect_silent(imp <- mice(data, method = "2l.2stage.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
#  })

# test_that("micemd::mice.impute.2l.glm.bin() accepts factor outcome", {
#   expect_silent(imp <- mice(data, method = "2l.glm.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
# })


# toenail: outcome is 0/1
data("toenail")
data <- tidyr::complete(toenail, ID, visit) %>% 
  tidyr::fill(treatment) %>% 
  dplyr::select(-month)
summary(data)
pred <- make.predictorMatrix(data)
pred["outcome", "ID"] <- -2

test_that("mice::mice.impute.2l.bin() accepts 0/1 outcome", {
  expect_silent(imp <- mice(data, method = "2l.bin", print = FALSE, pred = pred, m = 1, maxit = 1))
  expect_false(anyNA(complete(imp)))
})

# test_that("miceadds::mice.impute.2l.binary() accepts 0/1 outcome", {
#   expect_silent(imp <- mice(data, method = "2l.binary", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
# })

# test_that("micemd::mice.impute.2l.2stage.bin() accepts 0/1 outcome", {
#   expect_silent(imp <- mice(data, method = "2l.2stage.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
# })

# test_that("micemd::mice.impute.2l.glm.bin() accepts 0/1 outcome", {
#   expect_silent(imp <- mice(data, method = "2l.glm.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
#   expect_false(anyNA(complete(imp)))
# })
