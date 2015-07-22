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
model_leg <- function(Y, X, trueB, whichX) {
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
  
  vars <- list(X = X, Y = Y, B = B, invX = invX, H = H, h = h, I = I, 
               e = e, coefs = coefs, n = n, p = p, M = M)
  
  return(vars)
}

gdm_leg <- function(n = 25, B = c(1, 1, 1, 1, 0, 0), Estruct = "E0", whichX = c(T, T ,T ,T ,T ,F), Edist = "En") {
  
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
                  Ech = (rchisq(n, 5) - 5) / sqrt(10),
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
  
  values <- model_leg(Y, X, B, whichX)
  
  return(values)
}


f_p <- function(i, invX, W, e, h, H, I, w) {
  
  c <- invX[i,]
  A <- diag((W * c)^2)
  
  var_B <- t(e) %*% A %*% e
  
  o4 <- e^4 / (3 * w^2)
  o2o2 <- e^2 %*% t(e)^2 / (2 * H^2 + w %*% t(w))
  diag(o2o2) <- o4
  sigma_hat <- o2o2
  B <- ((I-H) %*% A %*% (I-H))
  
  num <- ((var_B)^2)
  den <- sum(as.vector(t(B)) * as.vector(B * sigma_hat))
  
  return(num/den)
}

saddlepoint_pval <- function(i, invX, W, I, H, e, w, t) {
  t <- t[i]
  c <- invX[i,]
  A <- diag((W * c)^2)
  eig <- eigen(A %*% (I - H) %*% diag(as.vector(e^2 / w)) %*% (I - H))$values
  g <- c(1, -t^2 * eig / sum(eig))
  s_eq <- function(s) sum(g / (1 - 2 * g * s))
  s_range <- if(s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
  s <- uniroot(s_eq, s_range)$root
  r <- sign(s) * sqrt(sum(log(1 / 2 * g * s)))
  q <- s * sqrt(2 * sum(g^2 / (1 - 2 * g * s)^2))
  p_val <- 1 - pnorm(r) - dnorm(r) * (1 / r - 1 / q)
  c(s = s, p_val = p_val)  
}

# estimation function takes model and vector of adjustments
# need to design function to calculate appropriate adjustments given vector of adjustments
# adjustment function will return a dataframe of adjustment name, parameter, estimated coefficient, standard error, degrees of freedom
estimate_leg <- function(estimator, f_p, saddle, model) {
  M <- model$M
  X <- model$X
  e <- model$e
  h <- model$h
  n <- model$n
  p <- model$p
  coefs <- model$coefs
  B <- model$B
  I <- model$I
  H <- model$H
  invX <- model$invX
  
  
  switch(estimator, 
         HOM = {
              w <- NULL
              W <- 1 / sqrt(w)
         }, 
         HC0 = {
              w <- 1
              W <- 1 / sqrt(w)
         }, 
         HC1 = {
              w <- (n / (n - p))
              W <- 1 / sqrt(w)
         }, 
         HC2 = {
              w <- 1 - h
              W <- 1 / sqrt(w)
         }, 
         HC3 = {
              w <- (1 - h)^2
              W <- 1 / sqrt(w)
         }, 
         HC4 = {
              d <- pmin(n * h / p, 4)
              w <- (1 - h)^d
              W <- 1 / sqrt(w)
         }, 
         HC4m = {
              d <- pmin(n * h / p, 1) + pmin(n*h / p,1.5)
              w <- (1 - h)^d
              W <- 1 / sqrt(w)
         }, 
         HC5 = {
              d <- pmin(n * h / p, pmax(4, .7 * n * max(h) / p))
              w <- (1 - h)^d/2
              W <- 1 / sqrt(w)
         })
  
  if(is.null(w)) {
    sd_e <- sqrt(diag(sum(e^2 / (n - p)) * M)) 
  } else {
    sd_e <- sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2 / w)) %*% X %*% M))
  }
  
  if(f_p) {
    df <- sapply(1:p, f_p, invX = invX, W = W, e = e, h = h, H = H, I = I, w = w)
  } else {
    df <- rep(n - p, p)
  }
  
  t_s <- (coefs - B) / sd_e
  t_p <- coefs / sd_e
  
  if(saddle) {
    size <- sapply(1:p, saddlepoint_pval, invX = invX, W = W, I = I, H = H, e = e, w = w, t = t_s)
    power <- sapply(1:p, saddlepoint_pval, invX = invX, W = W, I = I, H = H, e = e, w = w, t = t_p)               
  } else {
    size <- 2 * pt(-sapply(t_s, abs), df)
    power <- 2 * pt(-sapply(t_p, abs), df)
  }
  
  pValues <- c(size, power)
  names(pValues) <- paste(rep("B", p),0:(p-1), rep(c(" Size", " Power"), each = p), sep = "")
  return(pValues)
}


performance <- function(x, results, iterations) {
  results <- results[x,]
  results <- lapply(results, function(x) x < .05)
  Reduce("+", results)/iterations
}


runSim <- function(iterations, n, B, Estruct, whichX, Edist, estimator, seed = NULL) {
  require(reshape2)
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  estimator <- unlist(strsplit(estimator, " "))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- replicate(iterations, {
                    model <- gdm(n = n, 
                                 B = B, 
                                 Estruct = Estruct,
                                 whichX = whichX,
                                 Edist = Edist)
                    
                    estimates <- lapply(estimator, estimate, model)
                    names(estimates) <- estimator
                    return(estimates)
  })
  
  results <- sapply(estimator, performance, reps, iterations)
  vars <- Reduce(rbind, strsplit(rownames(results), " "))
  results <- data.frame(Parameter = vars[,1], Measure = vars[,2], results, stringsAsFactors = F)
    
  return(results)
}

