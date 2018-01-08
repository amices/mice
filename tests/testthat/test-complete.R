
#library(miceadds)
#data("data.allison.gssexp")
library(mice)
set.seed(62626)
data <- nhanes2
where <-  matrix(sample(c(FALSE, TRUE), size = nrow(data) * ncol(data),
                        prob = c(0.5, 0.5), replace = TRUE),
                        nrow = nrow(data), ncol = ncol(data))
imp <- mice(data, maxit = 1, where = where, seed = 17731)
c1 <- complete(imp)

# library(profvis)
# profvis(complete(imp, "long"))

# library(microbenchmark)
# recall <- TRUE
# z1 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, recall = recall),
#                      mice:::complete.mids(imp, include = FALSE, recall = recall),
#                      mice:::complete.mids(imp, "long", recall = recall),
#                      mice:::complete.mids(imp, "broad", recall = recall),
#                      mice:::complete.mids(imp, "rep", recall = recall),
#                     times = 100)
# recall <- FALSE
# z2 <- microbenchmark(mice:::complete.mids(imp, baseR = FALSE, include = TRUE, recall = recall),
#                      mice:::complete.mids(imp, baseR = FALSE, recall = recall),
#                      mice:::complete.mids(imp, "long", baseR = FALSE, recall = recall),
#                      mice:::complete.mids(imp, "broad", baseR = FALSE, recall = recall),
#                      mice:::complete.mids(imp, "rep", baseR = FALSE, recall = recall),
#                     times = 100)
