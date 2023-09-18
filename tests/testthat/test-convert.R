context("p2f")

# p2f is not required to do this

# method <- c("panImpute", "pmm")
# formulas <- list(bmi + chl + hyp ~ 1 | age,
#                  age ~ bmi + chl + hyp)
# formulas <- name.formulas(formulas)
# predictorMatrix <-
#   structure(c(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0),
#             dim = c(4L, 4L),
#             dimnames = list(c("bmi", "chl", "hyp", "age"),
#                             c("bmi", "chl", "hyp", "age")))
# form2 <- p2f(predictorMatrix,
#              blocks = construct.blocks(formulas, predictorMatrix))
# form2
# test_that("p2f() preserves random intercept '1 | age' in formula", {
#   expect_identical(
#     attr(terms(formulas[["F1"]]), "term.labels"),
#     attr(terms(form2[["F1"]]), "term.labels")
#   )
# })
