context("check.formula")

data <- nhanes
where <- is.na(data)

blocks <- name.blocks(list("bmi", "age", "chl"))
setup <- list(blocks = blocks, 
              formula = NULL, 
              varnames = names(data))
v1 <- mice:::check.formula(setup, data)

