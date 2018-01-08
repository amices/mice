
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

library(microbenchmark)
mild <- FALSE
z1 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, mild = mild),
                     mice:::complete.mids(imp, include = FALSE, mild = mild),
                     mice:::complete.mids(imp, "all", mild = mild),
                     mice:::complete.mids(imp, "long", mild = mild),
                     mice:::complete.mids(imp, "broad", mild = mild),
                     mice:::complete.mids(imp, "stacked", mild = mild),
                     mice:::complete.mids(imp, "rep", mild = mild),
                    times = 100)
mild <- TRUE
z2 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, mild = mild),
                     mice:::complete.mids(imp, include = FALSE, mild = mild),
                     mice:::complete.mids(imp, "all", mild = mild),
                     mice:::complete.mids(imp, "long", mild = mild),
                     mice:::complete.mids(imp, "broad", mild = mild),
                     mice:::complete.mids(imp, "stacked", mild = mild),
                     mice:::complete.mids(imp, "rep", mild = mild),
                     times = 100)
