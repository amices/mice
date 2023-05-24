# Outcommented to reduce dependencies under the _R_CHECK_DEPENDS_ONLY=true flag

# context("mice.impute.rf")
#
# #####################################
# # TEST 1: runs with single miss val #
# #####################################
#
# data <- matrix(
#   c(
#     1.0, 10.5, 1.5, 13.2, 1.8, 8.0, 1.7, 15.0, 23.0, 40.0,
#     2.0, 21.0, 3.3, 38.0, 4.5, -2.3, NA, -2.4
#   ),
#   nrow = 9, ncol = 2, byrow = TRUE
# )
# df <- data.frame(data)
#
# par <- list(
#   y = df$X1,
#   ry = !is.na(df$X1),
#   x = df[, "X2", drop = FALSE]
# )
#
# test_that(
#   "Runs with a single missing value",
#   {
#     expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "ranger"))))
#     expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "randomForest"))))
#   }
# )
