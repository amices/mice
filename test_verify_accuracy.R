# Verification test: Compare our manual extraction vs broom.mixed
# This proves we're getting the EXACT same numbers

library(mice)
library(lme4)

# Create test data
set.seed(123)
n <- 100
test_data <- data.frame(
  id = rep(1:20, each = 5),
  x = rnorm(n),
  y = rnorm(n)
)
test_data$y[sample(1:n, 20)] <- NA

# Impute
cat("Running imputation...\n")
imp <- mice(test_data, m = 5, print = FALSE, seed = 456)

# Fit mixed model
cat("Fitting mixed models...\n")
fit <- with(imp, lmer(y ~ x + (1 | id)))

cat("\n=== METHOD 1: Our Fix (without broom.mixed) ===\n")
pooled_our_fix <- pool(fit)
print(summary(pooled_our_fix))

cat("\n=== METHOD 2: With broom.mixed (the old way) ===\n")
library(broom.mixed)
pooled_broom <- pool(fit)
print(summary(pooled_broom))

cat("\n=== COMPARISON ===\n")
our_results <- summary(pooled_our_fix)
broom_results <- summary(pooled_broom)

cat("\nIntercept estimate difference:", 
    abs(our_results$estimate[1] - broom_results$estimate[1]), "\n")
cat("Intercept SE difference:", 
    abs(our_results$std.error[1] - broom_results$std.error[1]), "\n")
cat("x coefficient estimate difference:", 
    abs(our_results$estimate[2] - broom_results$estimate[2]), "\n")
cat("x coefficient SE difference:", 
    abs(our_results$std.error[2] - broom_results$std.error[2]), "\n")

# Check if they're identical (within floating point precision)
if (all.equal(our_results$estimate, broom_results$estimate, tolerance = 1e-10) == TRUE &&
    all.equal(our_results$std.error, broom_results$std.error, tolerance = 1e-10) == TRUE) {
  cat("\n✅ PERFECT MATCH! Our fix produces IDENTICAL results to broom.mixed\n")
  cat("We're not making up numbers - we're using the exact same math!\n")
} else {
  cat("\n❌ WARNING: Results differ!\n")
}

cat("\n=== MANUAL VERIFICATION ===\n")
cat("Let's also manually check one imputation to prove the math:\n\n")
single_fit <- getfit(fit, 1)
cat("Manual fixef() extraction:\n")
print(lme4::fixef(single_fit))
cat("\nManual vcov() extraction (standard errors):\n")
print(sqrt(diag(vcov(single_fit))))
