
#General example
data <- mvrnorm(n = 10000, 
                mu = c(10, 10, 10, 5, 5, 5),
                Sigma = nearPD(matrix(data = c(1.0, 0.2, 0.2, 0.5, 0.5, 0.5, 
                                               0.2, 1.0, 0.2, 0.5, 0.5, 0.5,
                                               0.2, 0.2, 1.0, 0.5, 0.5, 0.5, 
                                               0.5, 0.5, 0.5, 1.0, 0.2, 0.2, 
                                               0.5, 0.5, 0.5, 0.2, 1.0, 0.2, 
                                               0.5, 0.5, 0.5, 0.2, 0.2, 1.0), 
                                      nrow=6, byrow=TRUE))$mat)
data <- as.data.frame(data)
names(data) <- c("Y1", "Y2", "Y3", "X1", "X2", "X3")

#Simulation
#set.seed(999)
#data <- mvrnorm(n = 10000, mu = c(5, 15), Sigma = matrix(c(4, 2, 2, 3), 2, 2))
#data <- as.data.frame(data)

#Example Vignette