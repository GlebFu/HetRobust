rm(list = ls())

#-----------------------------
# Data-generating model
#-----------------------------

estimate_model <- function(Y, X, trueB, whichX) {
  
  X <- X[, whichX]
  B <- trueB[whichX]
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- solve(t(X) %*% X)
  X_M <- X %*% M
  coefs <- colSums(Y * X_M)
  e <- Y - as.vector(X %*% coefs)
  
  H <- X_M %*% t(X)
  h <- diag(H)
  
  values <- list(X = X, Y = Y, B = B, X_M = X_M, H = H, h = h, e = e, coefs = coefs, n = n, p = p, M = M)
  
  return(values)
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
                  Ech = (rchisq(n, 5) - 5) / sqrt(10),
                  Et = rt(n, 5))
  
  # Seven types of error structures
  error <- switch(Estruct,
                  E0 = Edist,
                  E1 = sqrt(x1) * Edist,
                  E2 = sqrt(x3 + 1.6) * Edist,
                  E3 = sqrt(x3) * sqrt(x4 + 2.5) * Edist,
                  E4 = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Edist,
                  E5 = ifelse(xD == 1, 1.5 * Edist, Edist),
                  E6 = ifelse(xD == 1, 4 * Edist, Edist)
  )
  
  # Generate DV
  Y <- as.vector(X %*% B) + error
  
  values <- estimate_model(Y, X, B, whichX)
  
  return(values)
}

#-----------------------------------
# p-value from a two-sided t-test 
#-----------------------------------

t_test <- function(coef, sd, df) {
  2 * pt(abs(coef / sd), df = df, lower.tail = FALSE)
}

#-----------------------------------
# Satterthwaite degrees of freedom 
#-----------------------------------

Satterthwaite <- function(V_b, X_M, omega, e, H, n, p) {
  
  sigma_hat <- tcrossprod(e^2) / (2 * H^2 + tcrossprod(rep(omega, length.out = n)^2))
  diag(sigma_hat) <- (omega * e)^4 / 3
  I_H <- diag(n) - H

  A_vec <- X_M^2 / omega^2
  
  V_V <- function(a) {
    B <- I_H %*% (a * I_H)
    sum(as.vector(t(B)) * as.vector(B * sigma_hat))
  }
  den <- apply(A_vec, 2, V_V)
  
  return(V_b^2 / den)
}

# function that provides full results based on Satterthwaite df

Satt_results <- function(model, HC) {
  M <- model$M
  X <- model$X
  e <- as.vector(model$e)
  h <- model$h
  n <- model$n
  p <- model$p
  coefs <- as.vector(model$coefs)
  B <- model$B
  H <- model$H
  X_M <- model$X_M
  
  omega <- switch(HC,
                  HC0 = 1,
                  HC1 = sqrt((n - p) / n),
                  HC2 = sqrt(1 - h),
                  HC3 = (1 - h),
                  HC4 = (1 - h)^(pmin(h * n / p, 4) / 2),
                  HC4m = (1 - h)^((pmin(h * n / p, 1) + pmin(h * n / p, 1.5))/2),
                  HC5 = (1 - h)^(pmin(h * n / p, pmax(4, .7 * n * max(h) / p)) / 4)
  )
  
  V_b <- colSums((X_M * e / omega)^2)
  df <- Satterthwaite(V_b, X_M, omega, e, H, n, p)  
  p_val <- t_test(coefs, sd = sqrt(V_b), df = df)
  data.frame(b = coefs, sd = sqrt(V_b), df = df, p_val = p_val)
}

#-----------------------------------
# testing function
#-----------------------------------

estimate <- function(HC, tests, model) {
  M <- model$M
  X <- model$X
  e <- as.vector(model$e)
  h <- model$h
  n <- model$n
  p <- model$p
  coefs <- as.vector(model$coefs)
  B <- model$B
  H <- model$H
  X_M <- model$X_M
  
  omega <- switch(HC,
                   HC0 = 1,
                   HC1 = sqrt((n - p) / n),
                   HC2 = sqrt(1 - h),
                   HC3 = (1 - h),
                   HC4 = (1 - h)^(pmin(h * n / p, 4) / 2),
                   HC4m = (1 - h)^((pmin(h * n / p, 1) + pmin(h * n / p, 1.5))/2),
                   HC5 = (1 - h)^(pmin(h * n / p, pmax(4, .7 * n * max(h) / p)) / 4)
  )
  
  V_b <- colSums((X_M * e / omega)^2)
  coefs_to_test <- c(coefs - B, coefs)
  
  # testing
  pValues <- data.frame(HC = HC, coef = rep(colnames(X), 2), criterion = rep(c("size","power"), each = p))
  if ("naive" %in% tests) pValues$naive <- t_test(coefs_to_test, sd = sqrt(V_b), df = n - p)
  if ("Satt" %in% tests) pValues$Satt <- t_test(coefs_to_test, sd = sqrt(V_b), 
                                               df = Satterthwaite(V_b, X_M, omega, e, H, n, p))
  
  pValues
}

#-----------------------------------
# simulation driver
#-----------------------------------

runSim <- function(iterations, n, B, whichX, Estruct, Edist, HC, tests, alpha, seed = NULL) {
  require(plyr)
  require(reshape2)
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- rdply(iterations, {
    model <- gdm(n = n, 
                 B = B, 
                 Estruct = Estruct,
                 whichX = whichX,
                 Edist = Edist)
    
    ldply(HC, estimate, tests = tests, model = model)
  })
  
  # performance calculations
  
  reps <- melt(reps, id.vars = c("HC","coef","criterion"), measure.vars = tests, variable.name = "test")
  ddply(reps, .(HC,coef,criterion,test), summarize, 
        p01 = mean(value < .01),
        p05 = mean(value < .05),
        p10 = mean(value < .10))
}



HC <- c("HC0","HC1","HC2","HC3","HC4","HC4m","HC5")
tests <- c("naive","Satt")
iterations <- 1000
n <- 1000
B <- "1 1 1 1 0 0"
whichX <- "T T T T T F"
Estruct <- "E0"
Edist <- "En"

system.time(result <- runSim(iterations, n, B, whichX, Estruct, Edist, HC, tests))

#---------------------------------
# check against sandwich
#---------------------------------

B <- as.numeric(unlist(strsplit(B, " ")))
whichX <- as.logical(unlist(strsplit(whichX, " ")))
model <- gdm(n = 25, B = B, Estruct = Estruct, whichX = whichX, Edist = Edist)
my_results <- acast(subset(ldply(HC, estimate, tests = "naive", model), criterion == "power"), coef ~ HC, value.var = "naive")

p_vals <- function(model, HC) {
  require(sandwich)
  dat <- with(model, data.frame(Y, X))
  m_fit <- lm(Y ~ x1 + x2 + x3 + x4, data = dat)
  V_b <- diag(vcovHC(m_fit, type = HC))
  t_test(coef = coef(m_fit), sd = sqrt(V_b), df = m_fit$df.residual)
}

sandwich_results <- sapply(HC, p_vals, model = model)
all.equal(my_results, sandwich_results, check.attributes = FALSE)
my_results - sandwich_results

lapply(HC, Satt_results, model = model)
