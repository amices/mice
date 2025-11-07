# Test script for lmer pooling fix
# This tests whether pool() works with lmer objects without broom.mixed

library(mice)
library(lme4)

# Create simple test data with missing values
set.seed(123)
n <- 100
test_data <- data.frame(
  id = rep(1:20, each = 5),
  x = rnorm(n),
  y = rnorm(n)
)

# Introduce some missingness
test_data$y[sample(1:n, 20)] <- NA

# Impute
cat("Running imputation...\n")
imp <- mice(test_data, m = 5, print = FALSE, seed = 456)

# Fit mixed model
cat("Fitting mixed models to imputed data...\n")
fit <- with(imp, lmer(y ~ x + (1 | id)))

# Try to pool - this should work now without broom.mixed!
cat("\nAttempting to pool results (without broom.mixed)...\n")
pooled <- pool(fit)

cat("\n=== SUCCESS! ===\n")
cat("Pooled results:\n")
print(summary(pooled))

cat("\n✅ The fix works! pool() can now handle lmer objects without broom.mixed\n")
