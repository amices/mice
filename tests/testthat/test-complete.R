context("complete")

imp <- mice(nhanes, maxit = 1, m = 2, seed = 123, print = FALSE)

lng <- subset(complete(imp, "long"), .imp == 1, select = c(age, bmi, hyp, chl))
all <- complete(imp, "all")[[1]]

test_that("long and all produce same data", {
  expect_equal(lng, all)
})


# mids workflow using saved objects
imp <- mice(nhanes, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
est <- pool(fit)
est.mice <- est

# mild workflow using saved objects and base::lapply
idl <- complete(imp, "all")
fit <- lapply(idl, lm, formula = chl ~ age + bmi + hyp)
est <- pool(fit)
est.mild <- est

# long workflow using base::by
cmp <- complete(imp, "long")
fit <- by(cmp, as.factor(cmp$.imp), lm, formula = chl ~ age + bmi + hyp)
est <- pool(fit)
est.long <- est

test_that("workflow mids, mild and long produce same estimates", {
  expect_identical(getqbar(est.mice), getqbar(est.mild))
  expect_identical(getqbar(est.mice), getqbar(est.long))
})



#library(miceadds)
#data("data.allison.gssexp")
# library(mice)
# set.seed(62626)
# data <- nhanes2
# where <-  matrix(sample(c(FALSE, TRUE), size = nrow(data) * ncol(data),
#                         prob = c(0.5, 0.5), replace = TRUE),
#                         nrow = nrow(data), ncol = ncol(data))
# imp <- mice(data, maxit = 1, where = where, seed = 17731)
# c1 <- complete(imp)
# 
# # library(profvis)
# # profvis(complete(imp, "long"))
# 
# library(microbenchmark)
# mild <- FALSE
# z1 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, mild = mild),
#                      mice:::complete.mids(imp, include = FALSE, mild = mild),
#                      mice:::complete.mids(imp, "all", mild = mild),
#                      mice:::complete.mids(imp, "long", mild = mild),
#                      mice:::complete.mids(imp, "broad", mild = mild),
#                      mice:::complete.mids(imp, "stacked", mild = mild),
#                      mice:::complete.mids(imp, "rep", mild = mild),
#                     times = 100)
# mild <- TRUE
# z2 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, mild = mild),
#                      mice:::complete.mids(imp, include = FALSE, mild = mild),
#                      mice:::complete.mids(imp, "all", mild = mild),
#                      mice:::complete.mids(imp, "long", mild = mild),
#                      mice:::complete.mids(imp, "broad", mild = mild),
#                      mice:::complete.mids(imp, "stacked", mild = mild),
#                      mice:::complete.mids(imp, "rep", mild = mild),
#                      times = 100)

