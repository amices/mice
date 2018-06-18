#Same seed - multiple cores - 
#Result: Imputations not equal between mice and parlmice
test_that("Warning and Imputations between mice and parlmice are unequal", {
  expect_warning(A <- parlmice(nhanes, m = 2, seed = 123))
  B <- mice(nhanes, m = 2, print = FALSE, seed = 123)
  expect_false(all(complete(A, "long") == complete(B, "long")))
})

#Same seed - single core - 
#Result: Imputations equal between mice and parlmice
C <- parlmice(nhanes, n.core = 1, n.imp.core = 5, seed = 123)
D <- mice(nhanes, m = 5, print = FALSE, seed = 123)
test_that("Imputations are equal between mice and parlmice", {
  expect_identical(complete(C, "long"), complete(D, "long"))
})

#Same cluster.seed - multiple cores
#Result: Imputations equal between parlmice instances
E <- parlmice(nhanes, n.core = 2, n.imp.core = 2, cluster.seed = 123)
G <- parlmice(nhanes, n.core = 2, n.imp.core = 2, cluster.seed = 123)
test_that("Imputations are equal between mice and parlmice", {
  expect_equal(E, G)
})

#NOT RUN ON R CMD CHECK AND CRAN CHECK - TOO MANY PARALLEL PROCESSES SPAWNED
#Should return m = n.imp.core * parallel::detectCores() - 1
# test_that("Warning because n.core not specified", {
#   expect_warning(H <- parlmice(nhanes, n.imp.core = 3))
# })
# test_that("Cores not specified", {
#   expect_identical(H$m, 3 * (parallel::detectCores() - 1))
# })

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

