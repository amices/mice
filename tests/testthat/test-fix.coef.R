model0 <- lm(Volume ~ Girth + Height, data = trees)
formula(model0)
coef(model0)
deviance(model0)

# refit same model
model1 <- fix.coef(model0)
formula(model1)
coef(model1)
deviance(model1)

# change the beta's
model2 <- fix.coef(model0, beta = c(-50, 5, 1))
coef(model2)
deviance(model2)

# compare predictions
plot(predict(model0), predict(model1)); abline(0,1)
plot(predict(model0), predict(model2)); abline(0,1)

# compare proportion explained variance
cor(predict(model0), predict(model0) + residuals(model0))^2
cor(predict(model1), predict(model1) + residuals(model1))^2
cor(predict(model2), predict(model2) + residuals(model2))^2

# extract offset from constrained model
summary(model2$model$offset)

# it also works with factors and missing data
model0 <- lm(bmi ~ age + hyp + chl, data = nhanes2)
model1 <- fix.coef(model0)
model2 <- fix.coef(model0, beta = c(15, -8, -8, 2, 0.2))

