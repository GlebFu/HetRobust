
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

nu_q <- function(q, X_M, H, h) {
  g_q <- X_M[,q]
  sum(g_q^2)^2 / (sum(g_q^4 * (1 - 2 * h) / (1 - h)^2) + sum(tcrossprod(g_q^2 / (1 - h)) * H^2))
}


edgePVal <- function(tHC, v) {
  2 * (1 - pnorm(abs(tHC))) + (abs(tHC)^3 + abs(tHC)) * dnorm(tHC) / (2 * v)
}

#-----------------------------------
# Rothenberg 1988
#-----------------------------------

edgeR <- function(coefs, V_b, X_M, n, H, h, omega, e, approx = "model") {
  
  tHC <- coefs / sqrt(V_b)
  
  if (approx=="model") {
    q_ii <- -h
    a_vec <- 0
    b_vec <- colSums((X_M / omega)^2 * q_ii) / colSums(X_M^2) - 1
  } else {
    sigma <- (e/omega)^2
    I_H <- diag(nrow=n) - H
    q_ii <- colSums(I_H^2 * sigma) - sigma
    z_mat <- I_H %*% (sigma * X_M)
    a_vec <- colSums((X_M * z_mat / omega)^2) / V_b^2
    b_vec <- colSums((X_M / omega)^2 * q_ii) / V_b - 1
  }
  nu <- 6 * V_b^2 / colSums((X_M * e / omega)^4)
  
  2 * (1 - pnorm(abs(tHC) / 2 * (2 - (1 + tHC^2) / nu + a_vec * (tHC^2 - 1) + b_vec)))
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
    v <- sapply(1:p, nu_q, X_M = X_M, H = H, h = h)
    pValues$edgeKC <- edgePVal(coefs_to_test/sqrt(V_b), v)
  }
  
  if("edgeR" %in% tests) {
    pValues$edgeR_V1 <- edgeR(coefs_to_test, V_b, X_M, n, H, h, omega, e, approx = "model")
    pValues$edgeR_V2 <- edgeR(coefs_to_test, V_b, X_M, n, H, h, omega, e, approx = "empirical")
  }
  
  pValues
}
