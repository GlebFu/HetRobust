library(microbenchmark)

#--------------------------------
# Kauermann & Carroll 
#--------------------------------

# degrees of freedom
nu_q <- function(q, Xmat) {
  XX_tX <- chol2inv(chol(t(X) %*% X)) %*% t(X)
  H <- X %*% XX_tX
  h_i <- diag(H)
  g_q <- XX_tX[q,]
  sum(g_q^2)^2 / (sum(g_q^4) + sum(((g_q^2 / (1 - h_i)) %*% t(g_q^2 / (1 - h_i))) * H))
}

f_alpha <- function(a, nu) {
  z_a <- qnorm(1 - a / 2)
  a + (z_a^3 + z_a) * dnorm(z_a) / (2 * nu)
}

crit_adj <- function(alpha, nu) {
  alpha <- uniroot(function(a) f_alpha(a, nu = nu) - alpha, lower = 10^-12, upper = 1)$root
  qnorm(1 - alpha / 2)
}

crit_approx <- function(alpha, X) {
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
  
n <- 25
p <- 6
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

alpha <- 0.05
v <- sapply(1:p, nu_q, Xmat = X)
(crit1 <- sapply(v, crit_adj, alpha = alpha))
(crit2 <- crit_approx(alpha, X = X))

edgePVal <- function(tHC) {
  2*(1-qnorm(abs(tHC))) + qnorm(tHC)/(2*v)*(abs(tHC)^3 + abs(tHC))
}
edgePVal(.05)
#--------------------------------
# Rothenberg 1988
#--------------------------------

n <- 25
p <- 6
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

q <- 1
alpha <- 0.05
t_alpha <- qnorm(1 - alpha)
sigma_i <- sqrt(rchisq(n, df = 1))
Sigma <- diag(sigma_i^2)
XX_tX <- chol2inv(chol(t(X) %*% X)) %*% t(X)
M <- diag(n) - X %*% XX_tX
Q <- M %*% Sigma %*% M - Sigma
x_i <- XX_tX[q,]
xSigmax <- as.numeric(t(x_i) %*% Sigma %*% x_i)
z_i <- M %*% Sigma %*% x_i / sqrt(xSigmax)
V_W <- 2 * n * sum(x_i^4 * sigma_i^4) / xSigmax^2
a <- n * sum(x_i^2 * z_i^2) / xSigmax
b <- n * sum(x_i^2 * diag(Q)) / xSigmax

t_2 <- t_alpha * (1 + ((1 + t_alpha^2) * V_W / 4 - a * (t_alpha^2 - 1) - b) / (2 * n))
