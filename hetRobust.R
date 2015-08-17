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
#Saddlepoint approximation
#-----------------------------------

saddlepoint_pval <- function(t, Q) {
  eig <- pmax(0, eigen(Q, symmetric = TRUE, only.values=TRUE)$values)
  g <- c(1, -t^2 * eig / sum(eig))
  s_eq <- function(s) sum(g / (1 - 2 * g * s))
  s_range <- if (s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
  s <- uniroot(s_eq, s_range)$root
  if (s != 0) {
    r <- sign(s) * sqrt(sum(log(1 - 2 * g * s)))
    q <- s * sqrt(2 * sum(g^2 / (1 - 2 * g * s)^2))
    p_val1 <- 1 - pnorm(r) - dnorm(r) * (1 / r - 1 / q)
    p_val2 <- 0.5 - sum(g^3) / (3 * sqrt(pi) * sum(g^2)^(3/2))
    p_val <- min(p_val1, p_val2)
  } else {
    p_val <- 0.5 - sum(g^3) / (3 * sqrt(pi) * sum(g^2)^(3/2))
  }
  p_val
}

saddle <- function(coef, sd, X_M, omega, e, H, n, approx = "model") {
  t_stats <- coef / sd
  I_H <- diag(n) - H
  A_sqrt_vec <- X_M / omega
  if (approx == "model") {
    Qs <- apply(A_sqrt_vec, 2, function(x) tcrossprod(x) * I_H)
  } else {
    Qs <- apply(A_sqrt_vec, 2, function(x) tcrossprod(tcrossprod(x, e / omega) * I_H))
  }
  Qs <- lapply(data.frame(Qs), matrix, nrow = n, ncol = n)
  mapply(saddlepoint_pval, t = t_stats, Q = Qs)
}


#-----------------------------------
#Edgeworth KC approximation
#-----------------------------------

nu_q <- function(q, Xmat) {
  XX_tX <- chol2inv(chol(t(Xmat) %*% Xmat)) %*% t(Xmat)
  H <- Xmat %*% XX_tX
  h_i <- diag(H)
  g_q <- XX_tX[q,]
  sum(g_q^2)^2 / (sum(g_q^4) + sum(((g_q^2 / (1 - h_i)) %*% t(g_q^2 / (1 - h_i))) * H))
}

f_alpha <- function(a, nu) {
  z_a <- qnorm(1 - a / 2)
  a + (z_a^3 + z_a) * dnorm(z_a) / (2 * nu)
}

edgeKC_adj <- function(alpha, nu) {
  alpha <- uniroot(function(a) f_alpha(a, nu = nu) - alpha, lower = 10^-12, upper = 1)$root
  qnorm(1 - alpha / 2)
}

edgeKC_approx <- function(alpha, X) {
  n <- nrow(X)
  p <- ncol(X)
  XX_tX <- chol2inv(chol(t(X) %*% X)) %*% t(X)
  H <- X %*% XX_tX
  h_i <- diag(H)
  z_alpha <- qnorm(1 - alpha / 2)
  crit <- function(q) {
    g_q <- XX_tX[q,]
    nu <- sum(g_q^2)^2 / (sum(g_q^4) + sum(((g_q^2 / (1 - h_i)) %*% t(g_q^2 / (1 - h_i))) * H))  
    qt(1 - alpha / 2, df = n - p) + (1 / nu - sum(g_q^2)^2 / n) * (z_alpha^3 + z_alpha) / 4
  }
  sapply(1:p, crit)
}

edgePVal <- function(tHC, v) {
  2*(1-pnorm(abs(tHC))) + dnorm(tHC)/(2*v)*(abs(tHC)^3 + abs(tHC))
}

#-----------------------------------
# Rothenberg 1988
#-----------------------------------
edgeR <- function(q, tHC, X, n, M, I, H, omega, e, sigma = NULL) {
  I_H <- I - H
  
  # Restarts q to count from 1
  if(q > length(tHC)/2) q <- q-length(tHC)/2 
  
  tHC <- tHC[q]
  
  if(is.null(sigma)) sigma <- diag((e/omega)^2)
  
  Q <- I_H %*% sigma %*% I_H
  
  g_q <- (X %*% M)[,q]
  
  z_q <- ((I_H) %*% sigma %*% (X %*% M))[,q]
  
  a <- sum(g_q^2 * z_q^2 / omega^2) / sum(g_q^2 * (e/omega)^2)^2
  
  b <- sum(g_q^2 * diag(Q)^2 / omega^2) / sum(g_q^2 * ((e/omega)^2)) - 1
  
  v_q <- sum(g_q^2 * (e/omega)^2)^2 / (sum(g_q^4 * (e/omega)^4) / 3)
  
  pvalue <- 2 * (1 - pnorm(abs(tHC) / 2 * (2 - (1 + tHC^2) / (2 * v_q) - a * (tHC^2) - 1) - b))
  
  return(pvalue)
  
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



