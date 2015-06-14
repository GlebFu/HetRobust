library(htmlTable)
library(PerformanceAnalytics)
library(reshape2)
library(plyr)
rm(list = ls())

# Distributions used in generating data
b1 <- runif(100000, 0, 1)
b2 <- rnorm(100000, 0, 1)
b3 <- rchisq(100000, 1)
b4 <- rnorm(100000, 0, 1)
b5 <- runif(100000, 0, 1)

# Intercept and four independant variables based on distributions
x0 <- 1
x1 <- 1 + b1
x2 <- 3 * b1 + .6 * b2
x3 <- 2 * b1 + .6 * b3
x4 <- .1 * x1 + .9 * x3 - .8 * b4 + 4 * b5

# One dummy variable created by splitting x2
xD <- ifelse(x2 > 1.6, 1, 0)

# X matrix
X <- cbind(x0, x1, x2, x3, x4, xD)

# X matrix with standardized x's
Xsd <- scale(X)
Xsd[,1] <- x0

charactaristics <- function(x) {
  round(cbind(min(x), max(x), mean(x), sd(x)),2)
}

# Expected charactaristics of x4 are -.28, 15.28, 3.59 and 1.69 
# for minimum, maximum, mean and variance respectively
htmlTable(t(apply(X,2,charactaristics)),
          header = c("Min", "Max", "Mean", "Var"))

# Expected cor of x4 with other covariates should be, in order:
# .32, .27, .56, .23
round(cor(X[,-1]),2)


# Three types of homoscedastistic errors:
En <- rnorm(100000, 0, 1)
Ech <- rchisq(100000, 5)
Et <- rt(100000, 5)


sqrt(8/5) # Reported skewness of chi error distribution
skewness(Ech) # Replicated skewness of chi error distribution

12/5 # Reported excess kurtosis of chi error distribution
kurtosis(Ech, method = "excess") # Replicated excess kurtosis of chi error distribution

0 # Reported skewness of t error distribution
skewness(Et) # Replicated skewness of t error distribution

6 # # # Reported excess kurtosis of t error distribution
kurtosis(Et, method = "excess") # # # Replicated excess kurtosis of t error distribution

E0 <- En
E1 <- sqrt(x1) * En
E2 <- sqrt(x3 + 1.6) * En
E3 <- sqrt(x3) * sqrt(x4 + 2.5) * En #  x4 can be less than -2.5 per my simulation
E4 <- sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * En
E5 <- ifelse(xD == 1, 1.5 * En, En)
E6 <- ifelse(xD == 1, 4 * En, En)

source(file = "Long Ervin Replication.R")

# Demo
error(erlist = "E4", 
      X = cbind(rep(1,5),
                runif(5, 0, 1), 
                runif(5, 0, 1),
                runif(5, 0, 1),
                runif(5, 0, 1),
                runif(5, 0, 1)), 
      Edist = rnorm(5))

# Demo
model(Y = runif(15, 0, 1), 
      X = cbind(rep(1,15),
                runif(15, 0, 1), 
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1)), 
      trueB = c(1, 1, 0, 0 ,0 ,0),
      whichX = c(T, T, T, F, F, F))

# Demo
test_mod <- gdm(n = 10, 
                B = c(1, 1, 0, 0, 0, 0), 
                Estructs = c("E0", "E1", "E2"),
                whichX = c(T, T, T, T, F, F),
                Edist = "En")
test_mod$E0

# replication demo
testrep <- replicate(2, {
  model <- gdm(n = 10, 
               B = c(1, 1, 0, 1, 0, 0), 
               Estructs = c("E0", "E1", "E2"),
               whichX = c(T, T, T, T, F, F),
               Edist = "En")
  
  estimates <- ldply(model, estimate)
  estimates <- melt(estimates, id.vars = c(".id", "Adjustment", "Beta"))
  return (estimates$value)
})


testimate <- ldply(test_mod, estimate)
melt(testimate, id.vars = c(".id", "Adjustment", "Beta"))
melt(testimate)

runSim(iterations = 2, 
       n = 5, 
       B = "1 1 1 0 0 0", 
       Estructs = "E0 E1 E2", 
       whichX = "T T T T F F", 
       Edist = "En", 
       seed = NULL)

test <- F
replicate(2, {
  x <- sample(1:5, 1)
  y <- x + 10
  if(test) return(y)
  print(ls())
  return(x)
})