rm(list=ls())
source("t-test-and-simulation-functions-JEP.R")

MacKinnon_dgm <- function(n = 25, B = c(1, 1, 1, 1, 0), whichX = c(F, F, F, F, T), g = 0, zg = 1) {
  
  trueB <- B
  p <- length(B) - 1
  # Distributions used in generating data
  X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))
  colnames(X) <- paste("x", 0:p, sep = "")
  eta <- rnorm(n)
  
  XB <- as.vector(X %*% B)
  sigma <- zg * XB^g
  
  # Generate DV
  Y <- XB + eta * sigma
  
  estimate_model(Y, X, trueB, whichX)
}

HC <- "HC2"
alphas <- c(.005, .01, .05, .10)
checks <- as.matrix(expand.grid(x0 = c(TRUE,FALSE), 
                      x1 = c(TRUE,FALSE),
                      X2 = c(TRUE,FALSE),
                      X3 = c(TRUE,FALSE),
                      x4 = c(TRUE,FALSE)))[-32,]

pvals <- function(whichX, power) {
  set.seed(3)
  model <- MacKinnon_dgm(whichX = whichX)
  estimate(HC, model, alphas, power = power)
}

p_all <- plyr::adply(checks, 1, pvals, power = FALSE)

library(dplyr)

range_diff <- function(x) diff(range(x))
p_all %>%
  group_by(coef, criterion) %>%
  summarise_each(funs(range_diff), naive:saddle_H)
