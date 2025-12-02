context("mice.impute.2l.bin")

# toenail: outcome is factor
data("toenail2")
data <- tidyr::complete(toenail2, patientID, visit) %>%
  tidyr::fill(treatment) %>%
  dplyr::select(-time) %>%
  dplyr::mutate(patientID = as.integer(patientID))
summary(data)
# fit1 <- glm(outcome ~ treatment * month, data = toenail2, family = binomial)
# fit2 <- glm(outcome ~ treatment * visit, data = toenail2, family = binomial)
# fit3 <- lme4::glmer(outcome ~ treatment * visit + (1 | ID), data = data, family = binomial)

pred <- make.predictorMatrix(data)
pred["outcome", "patientID"] <- -2

# taken out of the test:
## ── Failure ('test-mice.impute.2l.bin.R:18:3'): mice::mice.impute.2l.bin()
## accepts factor outcome ──
## `... <- NULL` produced warnings.
## expect_silent()

imp <- mice(data, method = "2l.bin", print = FALSE, pred = pred, m = 1, maxit = 1)
test_that("mice::mice.impute.2l.bin() accepts factor outcome", {
  expect_false(anyNA(complete(imp)))
})

# toenail: outcome is 0/1
data("toenail", package = "mice")
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
