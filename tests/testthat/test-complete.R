
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
milc <- FALSE
z1 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, milc = milc),
                     mice:::complete.mids(imp, include = FALSE, milc = milc),
                     mice:::complete.mids(imp, "all", milc = milc),
                     mice:::complete.mids(imp, "long", milc = milc),
                     mice:::complete.mids(imp, "broad", milc = milc),
                     mice:::complete.mids(imp, "stacked", milc = milc),
                     mice:::complete.mids(imp, "rep", milc = milc),
                    times = 100)
milc <- TRUE
z2 <- microbenchmark(mice:::complete.mids(imp, include = TRUE, milc = milc),
                     mice:::complete.mids(imp, include = FALSE, milc = milc),
                     mice:::complete.mids(imp, "all", milc = milc),
                     mice:::complete.mids(imp, "long", milc = milc),
                     mice:::complete.mids(imp, "broad", milc = milc),
                     mice:::complete.mids(imp, "stacked", milc = milc),
                     mice:::complete.mids(imp, "rep", milc = milc),
                     times = 100)
