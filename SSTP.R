
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
  sum(g_q^2)^2 / (sum(g_q^4 * (1 - 2 * h_i) / (1 - h_i)^2) + sum(tcrossprod(g_q^2 / (1 - h_i)) * H^2))
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









