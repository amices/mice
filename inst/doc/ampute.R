## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 5, fig.height = 5, 
                      fig.align = "center", dev = "pdf")

## ---- out.width = "600px", echo = FALSE, fig.cap="Figure 1: Step-by-step flowchart of R-function ampute()"----
knitr::include_graphics("Figures/Flowchart.pdf")

## ------------------------------------------------------------------------
require("mice")
set.seed(2016)
testdata <- MASS::mvrnorm(n = 500, mu = c(10, 5, 0), 
                    Sigma = matrix(data = c(1.0, 0.2, 0.2, 0.2, 1.0, 0.2, 
                                            0.2, 0.2, 1.0), nrow = 3, byrow = T))
testdata <- as.data.frame(testdata)
summary(testdata)

## ------------------------------------------------------------------------
result <- ampute(testdata)
result

## ------------------------------------------------------------------------
names(result)

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
result <- ampute(testdata, prop = 0.3, patterns = mypatterns)
md.pattern(result$amp)

## ------------------------------------------------------------------------
result$freq

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1))
md.pattern(result$amp)

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1), mech = "MCAR")
result$mech

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1))
myweights <- result$weights
myweights

## ------------------------------------------------------------------------
mypatterns

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1), mech = "MNAR")
result$weights

## ------------------------------------------------------------------------
myweights[1, ] <- c(0, 0.8, 0.4)

## ------------------------------------------------------------------------
myweights[3, ] <- c(3, 1, 0)
myweights

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1), weights = myweights)

## ------------------------------------------------------------------------
lattice::bwplot(result, which.pat = c(1, 3), descriptives = TRUE)

## ---- include = FALSE----------------------------------------------------
require(BSDA)

## ------------------------------------------------------------------------
BSDA::tsum.test(mean.x = 0.52879, mean.y = -0.22978,
                s.x = sqrt(0.85752), s.y = sqrt(0.89401),
                n.x = 2099, n.y = 4896)

## ------------------------------------------------------------------------
xyplot(result, which.pat = 1, colors = mdc(1:2))

## ---- fig.cap = "Figure 2: Adaptations of continuous logit functions", echo = FALSE----
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
                key=list(space="top", columns=4, lines = list(lty = c(1, 2, 3, 4)), text = list(c("LEFT", "MID", "RIGHT", "TAIL"), cex = 0.5),
                colors = mdc(1:2)))
plot1

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1), weights = myweights,
                 type = c("RIGHT", "TAIL", "MID", "LEFT"))

## ----echo = FALSE--------------------------------------------------------
lattice::bwplot(result, which.pat = 2, descriptives = FALSE)

## ----echo = FALSE--------------------------------------------------------
xyplot(result, which.pat = 4, colors = mdc(1:2))

## ------------------------------------------------------------------------
result$weights

## ------------------------------------------------------------------------
myodds <- result$odds
myodds

## ---- include = FALSE----------------------------------------------------
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
                         text = list(c("Group1", "Group2", "Group3", "Group4"), 
                                     cex = 0.5)))
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
                         text = list(c("Group1", "Group2", "Group3", "Group4"), 
                                     cex = 0.5)))

## ----fig.cap = "Figure 3: Probabilities to have missing values for the four groups in pattern 1 and the relation between the groups and the weighted sum scores", echo = FALSE----
plot2

## ----fig.cap = "Figure 4: Division of odds groups over variables $V_2$ and $V_3$", echo = FALSE----
plot3

## ------------------------------------------------------------------------
myodds[3, ] <- c(1, 0, 0, 1)
myodds[4, ] <- c(1, 1, 2, 2)
myodds <- cbind(myodds, matrix(c(NA, NA, NA, 1, NA, NA, NA, 1), nrow = 4, byrow = F))
myodds

## ------------------------------------------------------------------------
result <- ampute(testdata, prop = 0.3, patterns = mypatterns, 
                 freq = c(0.7, 0.1, 0.1, 0.1), weights = myweights,
                 cont = FALSE, odds = myodds)

## ------------------------------------------------------------------------
bwplot(result, which.pat = c(3, 4), descriptives = FALSE)

## ------------------------------------------------------------------------
ampdata1 <- ampute(testdata, patterns = c(0, 1, 1), prop = 0.2, mech = "MAR")$amp
ampdata2 <- ampute(testdata, patterns = c(1, 0, 1), prop = 0.5, mech = "MNAR")$amp
ampdata3 <- ampute(testdata, patterns = c(1, 1, 0), prop = 0.8, mech = "MCAR")$amp

indices <- sample(x = c(1, 2, 3), size = nrow(testdata), replace = TRUE, 
                  prob = c(1/3, 1/3, 1/3))

ampdata <- matrix(NA, nrow = nrow(testdata), ncol = ncol(testdata))
ampdata[indices == 1, ] <- as.matrix(ampdata1[indices == 1, ])
ampdata[indices == 2, ] <- as.matrix(ampdata2[indices == 2, ])
ampdata[indices == 3, ] <- as.matrix(ampdata3[indices == 3, ])

md.pattern(ampdata)

## ------------------------------------------------------------------------
emptyresult <- ampute(testdata, run = FALSE)
emptyresult$amp

## ------------------------------------------------------------------------
result$cand[1:30]

## ------------------------------------------------------------------------
result$scores[[1]][1:10]

## ------------------------------------------------------------------------
head(result$data)

