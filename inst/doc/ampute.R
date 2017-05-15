## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "png", fig.align = "center")

## ---- out.width=700, echo=FALSE------------------------------------------
knitr::include_graphics("Figures/Scheme.png", auto_pdf = TRUE, dpi=600)

## ---- out.width = 700, echo = FALSE--------------------------------------
knitr::include_graphics("Figures/Flowchart.png", auto_pdf = TRUE, dpi=600)

## ---- message = FALSE, warning = FALSE-----------------------------------
require("mice")

## ------------------------------------------------------------------------
set.seed(2016)
testdata <- MASS::mvrnorm(n = 10000, mu = c(10, 5, 0), Sigma = matrix(data = c(1.0, 0.2, 0.2, 0.2, 1.0, 0.2, 0.2, 0.2, 1.0), nrow = 3, byrow = T))
testdata <- as.data.frame(testdata)
summary(testdata)

## ------------------------------------------------------------------------
result <- ampute(testdata)
result

## ------------------------------------------------------------------------
md.pattern(result$amp)

## ------------------------------------------------------------------------
result$prop

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.2, bycases = FALSE)
md.pattern(result$amp)

## ------------------------------------------------------------------------
result$prop

## ------------------------------------------------------------------------
mypatterns <- result$patterns
mypatterns

## ------------------------------------------------------------------------
mypatterns[2, 1] <- 0
mypatterns <- rbind(mypatterns, c(0, 1, 0))
mypatterns

## ------------------------------------------------------------------------
result <- ampute(testdata, patterns = mypatterns)
md.pattern(result$amp)

## ------------------------------------------------------------------------
result$freq

## ------------------------------------------------------------------------
myfreq <- c(0.7, 0.1, 0.1, 0.1)

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = myfreq, patterns = mypatterns)
md.pattern(result$amp)

## ------------------------------------------------------------------------
result$mech

## ------------------------------------------------------------------------
result$weights

## ------------------------------------------------------------------------
myweights <- result$weights
myweights[1, ] <- c(0, 0.8, 0.4)

## ------------------------------------------------------------------------
myweights[3, ] <- c(3, 1, 0)
myweights

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = myfreq, patterns = mypatterns, mech = "MNAR")
result$patterns
result$weights

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = myfreq, patterns = mypatterns, weights = myweights)

## ---- fig.width = 7, fig.height = 5--------------------------------------
lattice::bwplot(result, which.pat = c(1, 3), descriptives = TRUE)

## ---- include = FALSE----------------------------------------------------
require(BSDA)

## ------------------------------------------------------------------------
BSDA::tsum.test(mean.x = 0.39077, mean.y = -0.38992, s.x = sqrt(0.83774), s.y = sqrt(0.87721), n.x = 3473, n.y = 3493)

## ---- fig.width = 7, fig.height = 7--------------------------------------
lattice::xyplot(result, which.pat = 1)

## ---- include = FALSE----------------------------------------------------

logistic <- function(x){
  exp(x)/(1+exp(x))
} 
x1 <- seq(-3, 3, 0.01)
y1 <- logistic(-mean(x1)+x1[])
data2 <- as.data.frame(matrix(c(x1, y1), 601, 2))
names(data2) <- c("X1", "Y")
data2[602:1202, "X1"] <- x1
data2[602:1202, "Y"] <- logistic(-abs(x1[]-mean(x1))+0.75)
data2[1203:1803, "X1"] <- x1
data2[1203:1803, "Y"] <- logistic(abs(x1[]-mean(x1))-0.75)
data2[1804:2404, "X1"] <- x1
data2[1804:2404, "Y"] <- logistic(mean(x1)-x1[])
data2["Type"] <- c(rep("RIGHT", 601), rep("MID", 601), rep("TAIL", 601), rep("LEFT", 601))
plot1 <- xyplot(Y ~ X1, data2, group = Type, t = 'l', 
                lty=c(1,2,3,4), 
                cex = 0.5, col = rep("black", 4),
                xlab = "Standardized weighted sum scores", 
                ylab = "Probability",
                key=list(space="top", columns=4, lines = list(lty = c(1, 2, 3, 4)), text = list(c("LEFT", "MID", "RIGHT", "TAIL"))))

## ---- fig.width = 7, fig.height = 5, echo = FALSE------------------------
plot1

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = c(0.7, 0.1, 0.1, 0.1), patterns = mypatterns, weights = myweights, cont = TRUE, type = c("RIGHT", "TAIL", "MID", "LEFT"))

## ---- fig.width = 7, fig.height = 5--------------------------------------
bwplot(result, which.pat = 2, descriptives = FALSE)

## ---- fig.width = 7, fig.height = 7--------------------------------------
xyplot(result, which.pat = 4)

## ------------------------------------------------------------------------
result$weights

## ------------------------------------------------------------------------
myodds <- result$odds
myodds

## ---- fig.width = 7, fig.height = 5, echo = FALSE------------------------
len <- length(result$scores[[1]])
R <- sample(x = c(1, 0), size = len, 
            prob = c(100 / len, (len - 100) / len), replace = TRUE)
data3 <- matrix(NA, length(R[R == 1]), 3)
data3[, 1] <- result$scores[[1]][R == 1]
ng <- length(result$odds[1, ][!is.na(result$odds[1, ])])
quantiles <- quantile(data3[, 1], probs = seq(0, 1, by = 1 / ng))
Q <- rep(NA, length(data3[, 1]))
for (k in 1:ng) {
  Q <- replace(Q, data3[, 1] >= quantiles[k] 
               & data3[, 1] <= quantiles[k + 1], k)
}
data3[, 2] <- Q
for (l in 1:ng) {
data3[data3[, 2] == l, 3] <- (ng * result$prop * result$odds[1, l]) / 
  sum(result$odds[1, ], na.rm = TRUE)
}
data3 <- as.data.frame(data3)
names(data3) <- c("scores", "group", "prob")
plot2 <- xyplot(prob ~ scores, data = data3, groups = group,
                ylab = "Probability", xlab = "Standardized weighted sum scores",
                pch=c(1,2,3,4), 
                cex = 0.5, col = rep("black", 4),
                key=list(space="top", columns=4, title="", 
                         cex = 1, points = list(pch = c(1, 2, 3, 4)), 
                         text = list(c("Group1", "Group2", "Group3", "Group4"))))
dat <- result$data[result$cand == 1, "V2"]
data3["V2"] <-  dat[R == 1]
dat <- result$data[result$cand == 1, "V3"]
data3["V3"] <-  dat[R == 1]
plot3 <- xyplot(V3 ~ V2, data = data3, groups = group,
                ylab = "Variable V3", xlab = "Variable V2", 
                pch=c(1,2,3,4), 
                cex = 0.8, col = rep("black", 4),
                key=list(space="top", columns=4, title="", 
                         cex = 1, points = list(pch = c(1, 2, 3, 4)), 
                         text = list(c("Group1", "Group2", "Group3", "Group4"))))
plot2

## ---- fig.width = 7, fig.height = 5, echo = FALSE------------------------
plot3

## ------------------------------------------------------------------------
myodds[3, ] <- c(1, 0, 0, 1)
myodds[4, ] <- c(1, 1, 2, 2)
myodds <- cbind(myodds, matrix(c(NA, NA, NA, 1, NA, NA, NA, 1), nrow = 4, byrow = F))
myodds

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = c(0.7, 0.1, 0.1, 0.1), patterns = mypatterns, 
                 weights = myweights,cont = FALSE, odds = myodds, prop = 0.3)

## ---- fig.width = 7, fig.height = 5--------------------------------------
bwplot(result, which.pat = c(3, 4), descriptives = FALSE)

## ------------------------------------------------------------------------
result <- ampute(testdata, freq = c(0.7, 0.3), patterns = c(0, 0, 1, 0, 1, 0), weights = c(0, 0, 1, 1, 0, 1))

## ------------------------------------------------------------------------
ampdata1 <- ampute(testdata, patterns = c(0, 1, 1), prop = 0.2, mech = "MAR")$amp
ampdata2 <- ampute(testdata, patterns = c(1, 1, 0), prop = 0.8, mech = "MCAR")$amp

indices <- sample(x = c(1, 2), size = nrow(testdata), replace = TRUE, 
                  prob = c(1/2, 1/2))

ampdata <- matrix(NA, nrow = nrow(testdata), ncol = ncol(testdata))
ampdata[indices == 1, ] <- as.matrix(ampdata1[indices == 1, ])
ampdata[indices == 2, ] <- as.matrix(ampdata2[indices == 2, ])

md.pattern(ampdata)

## ------------------------------------------------------------------------
names(result)

## ------------------------------------------------------------------------
result$cand[1:30]

## ------------------------------------------------------------------------
result$scores[[1]][1:10]

## ------------------------------------------------------------------------
head(result$data)

## ------------------------------------------------------------------------
emptyresult <- ampute(testdata, run = FALSE)
emptyresult$amp

