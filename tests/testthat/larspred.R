# boys3 <- dplyr::bind_cols(boys, boys, boys, .name_repair = "unique")

imp <- mice::mice(boys3, maxit = 0L, m = 1L, remove.collinear = FALSE)
data <- data.matrix(complete(imp, action = 1L))
v <- 7
lrs <- lars::lars(x = data[, -v], y = data[, v], type = "lar")
plot(lrs)

# pred <- make.predictorMatrix(boys3, selection = "lars", np = 10)
# colSums(pred)
