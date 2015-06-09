library(htmlTable)
library(PerformanceAnalytics)
library(reshape2)
library(plyr)
rm(list = ls())

#Distributions used in generating data
b1 <- runif(100000, 0, 1)
b2 <- rnorm(100000, 0, 1)
b3 <- rchisq(100000, 1)
b4 <- rnorm(100000, 0, 1)
b5 <- runif(100000, 0, 1)

#Intercept and four independant variables based on distributions
x0 <- 1
x1 <- 1 + b1
x2 <- 3 * b1 + .6 * b2
x3 <- 2 * b1 + .6 * b3
x4 <- .1 * b1 + .9 * b3 - .8 * b4 + 4 * b5 #not accurate

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
round(cor(X[,-1]),2)


#Three types of homoscedastistic errors:
En <- rnorm(100000, 0, 1)
Ech <- rchisq(100000, 5)
Et <- rt(100000, 5)


sqrt(8/5) #Reported skewness of chi error distribution
skewness(Ech) #Replicated skewness of chi error distribution

12/5 #Reported excess kurtosis of chi error distribution
kurtosis(Ech, method = "excess") #Replicated excess kurtosis of chi error distribution

0 #Reported skewness of t error distribution
skewness(Et) #Replicated skewness of t error distribution

6 ###Reported excess kurtosis of t error distribution
kurtosis(Et, method = "excess") ###Replicated excess kurtosis of t error distribution

E0 <- En
E1 <- sqrt(x1) * En
E2 <- sqrt(x3 + 1.6) * En
E3 <- sqrt(x3) * sqrt(x4 + 2.5) * En # x4 can be less than -2.5 per my simulation
E4 <- sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * En
E5 <- ifelse(xD == 1, 1.5 * En, En)
E6 <- ifelse(xD == 1, 4 * En, En)



rm(list = ls())

#Returns a vector of response errors to be added to a vector of responses
###erlist = mnemonic of error structure (character)
###X = matrix of covariates
###Edist = matrix of error distributions
error <- function(erlist, X, Edist) {
  x1 <- X[,2]
  x2 <- X[,2]
  x3 <- X[,3]
  x4 <- X[,4]
  xD <- X[,5]
  
  En <- Edist[,1]
  Ech <- Edist[,2]
  Et <- Edist[,3]
  
  switch(erlist,
         E0En = En,
         E1En = sqrt(x1) * En,
         E2En = sqrt(x3 + 1.6) * En,
         E3En = sqrt(x3) * sqrt(x4 + 2.5) * En,
         E4En = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * En,
         E5En = ifelse(xD == 1, 1.5 * En, En),
         E6En = ifelse(xD == 1, 4 * En, En),
         E0Ech = Ech,
         E1Ech = sqrt(x1) * Ech,
         E2Ech = sqrt(x3 + 1.6) * Ech,
         E3Ech = sqrt(x3) * sqrt(x4 + 2.5) * Ech,
         E4Ech = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Ech,
         E5Ech = ifelse(xD == 1, 1.5 * Ech, Ech),
         E6Ech = ifelse(xD == 1, 4 * Ech, Ech),
         E0Et = Et,
         E1Et = sqrt(x1) * Et,
         E2Et = sqrt(x3 + 1.6) * Et,
         E3Et = sqrt(x3) * sqrt(x4 + 2.5) * Et,
         E4Et = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Et,
         E5Et = ifelse(xD == 1, 1.5 * Et, Et),
         E6Et = ifelse(xD == 1, 4 * Et, Et),
  )
}

#Demo
error(erlist = "E4Et", 
      X = cbind(rep(1,5),
                runif(5, 0, 1), 
                runif(5, 0, 1),
                runif(5, 0, 1),
                runif(5, 0, 1),
                runif(5, 0, 1)), 
      Edist = cbind(rnorm(5),
                    rchisq(5,5),
                    rt(5,5)))

#Calculates a list of values for use in the estimation function
###Y = vector of response values
###X = matrix of covariates
###trueB = vector of known coeficients
###whichX = specifies which X strings to use in estimation
model <- function(Y, X, trueB, whichX) {
  Y <- Y
  X <- X[,whichX]
  B <- trueB[whichX]
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- solve(t(X) %*% X)
  invX <- M %*% t(X)
  coefs <- invX %*% Y
  e <- Y - X %*% coefs
  
  H <- X %*% invX
  h <- diag(H)
  I <- diag(1, n)
  W <- 1 / sqrt(1 - h)
  w <- 1 - h
  
  vars <- list(X = X, Y = Y, B = B, invX = invX, H = H, h = h, I = I, W = W, w = w, 
               e = e, coefs = coefs, n = n, p = p, M = M)
  
  return(vars)
}

#Demo
model(Y = runif(15, 0, 1), 
      X = cbind(rep(1,15),
                runif(15, 0, 1), 
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1)), 
      trueB = c(1, 1, 0, 0 ,0 ,0),
      whichX = c(T, T, T, F, F, F))

#Generates random data and returns a list of values for use in estimation
###n = Sample size
###B = Vector of coeficients
###Estructs = Character string of error structures to be used in data generation separated by " "
###whichX = Boolean vector indicating which coeficients to use in model estimation
gdm <- function (n, B, Estructs, whichX){
  Estructs <- unlist(strsplit(Estructs, " ")) 
  
  #Distributions used in generating data
  b1 <- runif(n, 0, 1)
  b2 <- rnorm(n, 0, 1)
  b3 <- rchisq(n, 1)
  b4 <- rnorm(n, 0, 1)
  b5 <- runif(n, 0, 1)
  
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
  
  #Three types of homoscedastistic error distributions:
  En <- rnorm(n, 0, 1)
  Ech <- rchisq(n, 5)
  Et <- rt(n, 5)
  
  Edist <- cbind(En, Ech, Et)
  
  #Generate DV with no error
  Y <- t(B %*% t(X))
  
  #Generate error matrix
  Es <- lapply(Estructs, error, X, Edist)
  
  #Add error to DV
  Y <- lapply(Es, function(Ers) Y + Ers)
  
  values <- lapply(Y, model, X, B, whichX)
  
  names(values) <- Estructs
  
  return(values)
}

#Demo
test_mod <- gdm(n = 10, 
                B = c(1, 1, 0, 0, 0, 0), 
                Estructs = c("E0En E1Et E2Ech"),
                whichX = c(T, T, T, F, F, F))
str(test_mod)
test_mod$E0En


### ESTIMATE FUNCTION IN PROGRESS ###

#cidf calculates the t-statistic, lower bound, and upper bound and returns dataframe
cidf <- function(adjust) {
  t <- qt(.975, adjust$df)
  
  ci <- t(t(cbind(lb = adjust$Coef, ub = adjust$Coef)) + matrix(c(-1,1)) %*% (t*adjust$sd_e))
  
  cidf = data.frame(t = t, lb = ci[,1] , ub = ci[,2])
  
  return(cidf)
}

#estimation function takes model and vector of adjustments
#need to design function to calculate appropriate adjustments given vector of adjustments
#adjustment function will return a dataframe of adjustment name, parameter, estimated coefficient, standard error, degrees of freedom

estimate <- function(model, adjust) {
  M <- model$M
  X <- model$X
  e <- model$e
  w <- model$w
  n <- model$n
  p <- model$p
  coefs <- model$coefs
  
  
  HC2 <- data.frame(Adjustment = rep("HC2", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w)) %*% X %*% M)), 
                    df = rep(n-p,p))
  HC3 <- data.frame(Adjustment = rep("HC3", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w^2)) %*% X %*% M)), 
                    df = rep(n-p,p))
  
  sdf <- rbind(HC2,HC3)
  
  cidf <- cidf(sdf)
  return(cbind(sdf, cidf))
}


testimate <- ldply(test_mod, estimate)
melt(testimate)

#replication demo
testrep <- replicate(2, {
  model <- gdm(n = 10, 
               B = c(1, 1, 0, 1, 0, 0), 
               Estructs = c("E0En E1Et E2Ech"),
               whichX = c(T, T, T, T, F, F))
  
  estimates <- ldply(model, estimate)
  estimates <- melt(estimates, id.vars = c(".id", "Adjustment", "Beta"))
  return (estimates$value)
})

