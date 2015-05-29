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
v

alpha <- 0.05
v <- sapply(1:p, nu_q, Xmat = X)
(crit1 <- sapply(v, crit_adj, alpha = alpha))
(crit2 <- crit_approx(alpha, X = X))
