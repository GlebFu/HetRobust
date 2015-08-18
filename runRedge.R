source("SSTP.R")
library(plyr)
library(Pusto)

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

gdm <- function(n = 25, B = c(1, 1, 1, 1, 0, 0), Estruct = "E0", whichX = c(T, T ,T ,T ,T ,F), Edist = "En") {
  
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
  if ("saddle" %in% tests) {
    pValues$saddle_V1 <- saddle(coef = coefs_to_test, sd = sqrt(V_b),
                                X_M = X_M, omega = omega, e = e,
                                H = H, n = n, approx = "model")
    pValues$saddle_V2 <- saddle(coef = coefs_to_test, sd = sqrt(V_b),
                                X_M = X_M, omega = omega, e = e,
                                H = H, n = n, approx = "empirical")
  }
  
  if ("edgeKC" %in% tests) {
    v <- sapply(1:p, nu_q, Xmat = X)
    pValues$edgeKC <- edgePVal(coefs_to_test/sqrt(V_b), v)
  }
  
  if("edgeR" %in% tests) {
    
    pValues$edgeR_V1 <- sapply(1:(2*p), edgeR, coefs_to_test/sqrt(V_b), X, n, M, I = diag(n), H, omega, e, sigma = diag(n))
    
    pValues$edgeR_V2 <- sapply(1:(2*p), edgeR, coefs_to_test/sqrt(V_b), X, n, M, I = diag(n), H, omega, e)
  }
  
  pValues
}

#-----------------------------------
# simulation driver
#-----------------------------------

runSim <- function(iterations, n, B, whichX, Estruct, Edist, HC, tests, seed = NULL) {
  require(plyr)
  require(reshape2)
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  HC <- as.character(unlist(strsplit(HC, " ")))
  tests <- as.character(unlist(strsplit(tests, " ")))
  
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
  
  if ("saddle" %in% tests) tests <- c(tests[tests != "saddle"], paste0("saddle_V",1:2))
  
  reps <- melt(reps, id.vars = c("HC","coef","criterion"), measure.vars = tests, variable.name = "test")
  ddply(reps, .(HC,coef,criterion,test), summarize, 
        p01 = mean(ifelse(is.na(value), F, value < .01)),
        p05 = mean(ifelse(is.na(value), F, value < .05)),
        p10 = mean(ifelse(is.na(value), F, value < .10)),
        percentNA = mean(is.na(value)))
}

#-----------------------------
# Run Rothenberg Edgeworth Correction
#-----------------------------


testmod <- lapply(c(25, 50, 100, 250, 500), gdm)

lapply(testmod, estimate, HC = "HC1", tests = "edgeR")

