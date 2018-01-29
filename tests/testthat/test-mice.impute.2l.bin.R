context("mice.impute.2l.bin")
# library(mice) 
# library(dplyr)
# library(tidyr, warn.conflicts = FALSE)
data(toenail, package = "DPpackage")
data <- tidyr::complete(toenail, ID, visit) %>% 
  tidyr::fill(treatment) %>% 
  dplyr::select(-month)

# fit1 <- glm(outcome ~ treatment * month, data = toenail, family = binomial)
# fit2 <- glm(outcome ~ treatment * visit, data = toenail, family = binomial)
# fit3 <- lme4::glmer(outcome ~ treatment * visit + (1 | ID), data = data, family = binomial)

pred <- mice(data, print = FALSE, maxit = 0, seed = 1)$pred
pred["outcome", "ID"] <- -2
imp <- mice(data, method = "2l.bin", pred = pred, m = 1, maxit = 1, print = FALSE)

test_that("mice.impute.2l.bin() is silent", {
  expect_silent(mice(data, method = "2l.bin", print = FALSE, pred = pred, m = 1, maxit = 1))
})

data("toenail", package = "DPpackage")
data <- tidyr::complete(toenail, ID, visit) %>% 
  tidyr::fill(treatment) %>% 
  dplyr::select(-month)
summary(data)

suppressPackageStartupMessages(library(miceadds, warn.conflicts = FALSE, quietly = TRUE))
pred <- make.predictorMatrix(data)
pred["outcome", "ID"] <- -2
test_that("miceadds::mice.impute.2l.binary() is silent", {
  expect_silent(mice(data, method = "2l.binary", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
})
test_that("miceadds::mice.impute.2l.glm.bin() is silent", {
  expect_silent(mice(data, method = "2l.glm.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
})

suppressPackageStartupMessages(library(micemd, warn.conflicts = FALSE, quietly = TRUE))
test_that("micemd::mice.impute.2l.2stage.bin() is silent", {
  expect_silent(mice(data, method = "2l.2stage.bin", pred = pred, seed = 1, maxit = 1, m = 1, print = FALSE))
})

