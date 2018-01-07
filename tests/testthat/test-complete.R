# 
# #library(miceadds)
# #data("data.allison.gssexp")
# library(mice)
# set.seed(62626)
# data <- nhanes2
# where <-  matrix(sample(c(FALSE, TRUE), size = nrow(data) * ncol(data), 
#                         prob = c(0.5, 0.5), replace = TRUE),
#                         nrow = nrow(data), ncol = ncol(data))
# imp <- mice(data, maxit = 1, where = where, seed = 17731)
# c1 <- complete(imp)
# 
# library(profvis)
# profvis(complete(imp, "long"))
#         
# library(microbenchmark)
# z1 <- microbenchmark(mice:::complete.mids(imp, include = TRUE),
#                      mice:::complete.mids(imp, include = FALSE),
#                      mice:::complete.mids(imp, "long"), 
#                      mice:::complete.mids(imp, "broad"),
#                      mice:::complete.mids(imp, "rep"),
#                     times = 100)
# z2 <- microbenchmark(mice:::complete.mids(imp, baseR = FALSE, include = TRUE),
#                      mice:::complete.mids(imp, baseR = FALSE), 
#                      mice:::complete.mids(imp, "long", baseR = FALSE), 
#                      mice:::complete.mids(imp, "broad", baseR = FALSE),
#                      mice:::complete.mids(imp, "rep", baseR = FALSE),
#                     times = 100)
# 
# baseR <- TRUE
# z3 <- microbenchmark(complete2(imp, baseR = baseR, include = TRUE),
#                      complete2(imp, baseR = baseR), 
#                      complete2(imp, "long", baseR = baseR), 
#                      complete2(imp, "broad", baseR = baseR),
#                      complete2(imp, "rep", baseR = baseR),
#                      times = 100)
