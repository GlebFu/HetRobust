library(htmlTable)
library(PerformanceAnalytics)
library(reshape2)
library(plyr)
library(Pusto)
library(dplyr)
library(ggplot2)
rm(list = ls())

# Calculates a list of values for use in the estimation function
### Y = vector of response values
### X = matrix of covariates
### trueB = vector of known coeficients
### whichX = specifies which X strings to use in estimation
model <- function(Y, X, trueB, whichX) {
  Y <- Y
  X <- X[, whichX]
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

gdm <- function(n, B, Estruct, whichX, Edist) {
  
  # Distributions used in generating data
  b1 <- runif(n, 0, 1)
  b2 <- rnorm(n, 0, 1)
  b3 <- rchisq(n, 1)
  b4 <- rnorm(n, 0, 1)
  b5 <- runif(n, 0, 1)
  
  # Four independant variables based on distributions
  x0 <- 1
  x1 <- 1 + b1
  x2 <- 3 * b1 + .6 * b2
  x3 <- 2 * b1 + .6 * b3
  x4 <- .1 * x1 + .9 * x3 - .8 * b4 + 4 * b5
  x2[x2 < -2.5] <- -2.5
  x4[x4 < -2.5] <- -2.5
  
  # One dummy variable created by splitting x2
  xD <- ifelse(x2 > 1.6, 1, 0)
  
  # X matrix
  X <- cbind(x0, x1, x2, x3, x4, xD)
  
  # Three types of homoscedastistic error distributions:
  Edist <- switch(Edist,
                  En = rnorm(n, 0, 1),
                  Ech = rchisq(n, 5) - 5,
                  Et = rt(n, 5))
  
  # Seven tyoes of error structures
  error <- switch(Estruct,
                  E0 = Edist,
                  E1 = sqrt(x1) * Edist,
                  E2 = sqrt(x3 + 1.6) * Edist,
                  E3 = sqrt(x3) * sqrt(x4 + 2.5) * Edist,
                  E4 = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Edist,
                  E5 = ifelse(xD == 1, 1.5 * Edist, Edist),
                  E6 = ifelse(xD == 1, 4 * Edist, Edist),
           )
  
  # Generate DV with no error
  Y <- t(B %*% t(X)) + error
  
  values <- model(Y, X, B, whichX)
  
  return(values)
}


pValues <- function(adjust) {
  
  
  t <- qt(.975, adjust$df)
  
  ci <- t(t(cbind(lb = adjust$Coef, ub = adjust$Coef)) + matrix(c(-1,1)) %*% (t*adjust$sd_e))
  
  captured <- adjust$B > ci[,1] & adjust$B < ci[,2]
  
  DNRNull <- 0 > ci[,1] & 0 < ci[,2]

  cidf = data.frame(t = t, lb = ci[,1] , ub = ci[,2], captured, DNRNull)
    
  return(cidf)
}

f_p <- function(i, invX, W, e, h, H, I, w) {
  
  c <- invX[i,]
  A <- diag((W * c)^2)
  
  var_B <- t(e) %*% A %*% e
  
  o4 <- e^4 / (3 * (w)^2)
  o2o2 <- e^2 %*% t(e)^2 / (2*H^2 + (w) %*%t (w))
  diag(o2o2) <- o4
  sigma_hat <- o2o2
  B <- ((I-H) %*% A %*% (I-H))
  
  num <- ((var_B)^2)
  den <- sum(as.vector(t(B)) * as.vector(B * sigma_hat))
  
  return(num/den)
}


# estimation function takes model and vector of adjustments
# need to design function to calculate appropriate adjustments given vector of adjustments
# adjustment function will return a dataframe of adjustment name, parameter, estimated coefficient, standard error, degrees of freedom
estimate <- function(adjust, model) {
  M <- model$M
  X <- model$X
  e <- model$e
  h <- model$h
  w <- model$w
  n <- model$n
  p <- model$p
  coefs <- model$coefs
  B <- model$B
  I <- model$I
  H <- model$H
  invX <- model$invX
  
  
  sdf <- switch(adjust,
                "HOM" = list(sd_e = sqrt(diag(sum(e^2 / (n - p)) * M)), 
                             df = rep(n - p, p)),
                
                "HC0" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2)) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC1" = list(sd_e = sqrt((n / (n - p))*diag(M %*% t(X) %*% diag(as.vector(e^2)) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC2" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w)) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC3" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^2)) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC4" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^pmin(n * h / p, 4))) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC4m" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^(pmin(n * h / p, 1) + pmin(n*h / p,1.5)))) %*% X %*% M)), 
                              df = rep(n - p, p)),
                
                "HC5" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^(pmin(n * h / p, pmax(4, .7 * n * max(h) / p))))) %*% X %*% M)), 
                             df = rep(n - p, p)),
                
                "HC2_fp" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w)) %*% X %*% M)), 
                                df = sapply(1:p, f_p, invX = invX, W = 1 / sqrt(w), e = e, h = h, H = H, I = I, w = w)),
                
                "HC3_fp" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^2)) %*% X %*% M)), 
                                df = sapply(1:p, f_p, invX = invX, W = 1 / sqrt(w^2), e = e, h = h, H = H, I = I, w = w)),
                
                "HC4_fp" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^pmin(n * h / p, 4))) %*% X %*% M)), 
                                df = sapply(1:p, f_p, invX = invX, W = 1 / sqrt(w^pmin(n * h / p, 4)), e = e, h = h, H = H, I = I, w = w)),
                
                "HC4m_fp" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^(pmin(n * h / p, 1) + pmin(n*h / p,1.5)))) %*% X %*% M)), 
                                 df = sapply(1:p, f_p, invX = invX, W = 1 / sqrt(w^(pmin(n * h / p, 1) + pmin(n*h / p,1.5))), e = e, h = h, H = H, I = I, w = w)),
                
                "HC5_fp" = list(sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w^(pmin(n * h / p, pmax(4, .7 * n * pmax(h) / p))))) %*% X %*% M)), 
                                df = sapply(1:p, f_p, invX = invX, W = 1 / sqrt(w^(pmin(n * h / p, pmax(4, .7 * n * pmax(h) / p)))), e = e, h = h, H = H, I = I, w = w)),
                )
  
                
  size <- 2 * pt(-abs((coefs - B) / sdf$sd_e), sdf$df)
  power <- 2 * pt(-abs(coefs / sdf$sd_e), sdf$df)
  pValues <- c(size, power)
  names(pValues) <- paste(rep("B", p),0:(p-1), rep(c(" Size", " Power"), each = p), sep = "")
  return(pValues)
}


performance <- function(x, results, iterations) {
  results <- results[x,]
  results <- lapply(results, function(x) x < .05)
  Reduce("+", results)/iterations
}


runSim <- function(iterations, n, B, Estruct, whichX, Edist, adjust, seed = NULL) {
  require(reshape2)
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  adjust <- unlist(strsplit(adjust, " "))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- replicate(iterations, {
                    model <- gdm(n = n, 
                                 B = B, 
                                 Estruct = Estruct,
                                 whichX = whichX,
                                 Edist = Edist)
                    
                    estimates <- lapply(adjust, estimate, model)
                    names(estimates) <- adjust
                    return(estimates)
  })
  
  results <- sapply(adjust, performance, reps, iterations)
  vars <- Reduce(rbind, strsplit(rownames(results), " "))
  results <- data.frame(Parameter = vars[,1], Measure = vars[,2], results, stringsAsFactors = F)
    
  return(results)
}

