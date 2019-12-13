context("mice: loggedEvents")

imp1 <- mice(nhanes, m = 2, print = FALSE)

# copy of data, different names
data2 <- cbind(nhanes, nhanes)
colnames(data2)[5:8] <- c("age2", "bmi2", "hyp2", "chl2")
imp2 <- suppressWarnings(mice(data2, m = 2, print = FALSE))

test_that("loggedEvents is NULL", {
  expect_null(imp1$loggedEvents)
  expect_type(imp2$loggedEvents, "list")
})


