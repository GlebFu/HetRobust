library(htmlTable)
library(PerformanceAnalytics)

#Distributions used in generating data
b1 <- runif(100000, 0, 1)
b2 <- rnorm(100000, 0, 1)
b3 <- rchisq(100000, 1)
b4 <- rnorm(100000, 0, 1)
b5 <- runif(100000, 0, 1)

#Four independant variables based on distributions
x0 <- 1
x1 <- 1 + b1
x2 <- 3 * b1 + .6 * b2
x3 <- 2 * b1 + .6 * b3
x4 <- .1 * b1 + .9 * b3 - .8 * b4 + 4 * b5

#One dummy variable created by splitting x2
xD <- ifelse(x2 > 1.6, 1, 0)

#X matrix
X <- cbind(x0, x1, x2, x3, x4, xD)

#X matrix with standardized x's
Xsd <- scale(X)
Xsd[,1] <- x0

charactaristics <- function(x) {
  round(cbind(min(x), max(x), mean(x), sd(x)),2)
}

#Expected charactaristics of x4 are -.28, 15.28, 3.59 and 1.69 
#for minimum, maximum, mean and variance respectively
htmlTable(t(apply(X,2,charactaristics)),
          header = c("Min", "Max", "Mean", "Var"))

#Expected cor of x4 with other covariates should be, in order:
#.32, .27, .56, .23
round(cor(X),2)


#Three types of homoscedastistic erros:
Eni <- rnorm(100000, 0, 1)
Echi <- rchisq(100000, 5)
Eti <- rt(100000, 5)


sqrt(8/5) #Reported skewness of chi error distribution
skewness(Echi) #Replicated skewness of chi error distribution

12/5 #Reported excess kurtosis of chi error distribution
kurtosis(Echi, method = "excess") #replicated excess kurtosis of chi error distribution

skewness(Eti) #Reported e
kurtosis(Eti, method = "excess")


