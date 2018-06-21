#Same seed - multiple cores - 
#Result: Imputations not equal between mice and parlmice
test_that("Warning and Imputations between mice and parlmice are unequal", {
  expect_warning(A <- parlmice(nhanes, m = 2, seed = 123))
  B <- mice(nhanes, m = 2, print = FALSE, seed = 123)
  expect_false(all(complete(A, "long") == complete(B, "long")))
})

#Same seed - single core - 
#Result: Imputations equal between mice and parlmice
test_that("Imputations are equal between mice and parlmice", {
  C <- parlmice(nhanes, n.core = 1, n.imp.core = 5, seed = 123)
  D <- mice(nhanes, m = 5, print = FALSE, seed = 123)
  expect_identical(complete(C, "long"), complete(D, "long"))
})

#Should return m = 8
I <- parlmice(nhanes, n.core = 2, n.imp.core = 4)
test_that("Cores and n.imp.core specified. Override m", {
  expect_identical(I$m, 2*4)
})

#Should return m = 3x5=15
test_that("n.imp.core not specified", {
  expect_warning(J <- parlmice(nhanes, n.core = 2))
  expect_identical(J$m, 2 * 5)
})

#Should return m = 2x7=42
test_that("n.imp.core not specified", {
  expect_warning(K <- parlmice(nhanes, n.core = 2, m = 7))
  expect_identical(K$m, 2 * 7)
})

#Should return error
test_that("n.core larger than logical CPU cores", {
  expect_error(parlmice(nhanes, n.core = parallel::detectCores() + 1))
})

# # NOT RUN ON R CMD CHECK AND CRAN CHECK - TOO MANY PARALLEL PROCESSES SPAWNED
# # Should return m = n.imp.core * parallel::detectCores() - 1
# test_that("Warning because n.core not specified", {
#   expect_warning(H <- parlmice(nhanes, n.imp.core = 3))
#   expect_identical(H$m, 3 * (parallel::detectCores() - 1))
# })

# #Same cluster.seed - multiple cores
# #Result: Imputations equal between parlmice instances
# test_that("cluster.seed", {
# imp1 <- parlmice(nhanes, m=4, cluster.seed = 123)
# imp2 <- parlmice(nhanes, m=4, cluster.seed = 123)
# expect_equal(imp1, imp2)
# })

# #Should run without failure
# test_that("Runs when overriding defaults", {
#   set.seed(123)
#   df <- boys
#   meth <- make.method(df)
#   pred <- make.predictorMatrix(df)
#   visit <- sample(1:ncol(df))
#   imp <- parlmice(df, method = meth, 
#                   predictorMatrix = pred, 
#                   visitSequence = visit, 
#                   m = 2,
#                   maxit = 3, 
#                   cluster.seed = 123,
#                   seed = 234)
#   expect_identical(pred, imp$pred)
#   expect_identical(imp$iteration, 3)
#   expect_identical(imp$method, meth)
#   expect_identical(imp$visitSequence, names(df)[visit])
#   expect_identical(imp$m, 2*4)
# })
