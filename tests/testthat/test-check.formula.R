context("check.formula")

data <- nhanes
where <- is.na(data)

blocks <- name.blocks(list("bmi", "age", "chl"))
setup <- list(blocks = blocks, 
              formula = NULL, 
              varnames = names(data))
v1 <- mice:::check.formula(setup, data)

# old.form <- c("", "bmi ~ chl + hyp", "hyp ~ bmi + chl", "chl ~ bmi + hyp")
# imp <- mice(nhanes, formula = old.form, m = 1, maxit = 2, print = FALSE)
# 
# form1 <- list(bmi = ~ 1, chl = ~ 1, hyp = ~ 1)
# # impute given predictors
# imp1 <- mice(nhanes, formula = form1, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, seed = 1)
# # impute the mean
# imp2 <- mice(nhanes, formula = form1, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, include.auxiliary = FALSE, seed = 1)
# 
# form2 <- list(bmi = "hyp ~ 1", chl = "hyp ~ 1", hyp = "hyp ~ 1")
# imp3 <- mice(nhanes, formula = form2, m = 1, maxit = 2, method = "norm.predict",
#              print = FALSE, include.auxiliary = FALSE, seed = 1)
# 

