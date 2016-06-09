n <- 20
p <- 5
const <- c(rep(0, p), 1)
X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))

M <- chol2inv(chol(crossprod(X)))
H <- X %*% M %*% t(X)
h_i <- diag(H)
omega <- 1 / (1 - h_i)
g_i <- as.vector(X %*% M %*% const)
A_vec <- omega * g_i^2

eigen(crossprod(X))$values
eigen(M)$values
eigen(H)$values
eigen(diag(n) - H)$values
eig_set <- eigen(diag(A_vec) %*% (diag(n) - H))
lambda <- eig_set$values
hist(lambda)
eig <- pmax(0, lambda[1:(n - p - 1)])

tstat <- 1.02
g <- c(1, -tstat^2 * eig / sum(eig))
s_eq <- function(s) sum(g / (1 - 2 * g * s))
s_range <- if (s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
(s <- uniroot(s_eq, s_range)$root)

saddle_pval <- function(s) {
  r <- sign(s) * sqrt(sum(log(1 - 2 * g * s)))
  q <- s * sqrt( 2 * sum(g^2 / (1 - 2 * g * s)^2))
  if (s!=0) {
    1 - pnorm(r) - dnorm(r) * (1 / r - 1 / q)
  } else {
    0.5 - sum(g^3) / (3 * sqrt(pi) * sum(g^2)^(3/2))
  }
}

s_seq <- seq(1 / (2 * min(g)), 1 / 2, 0.01)
(s_pval <- saddle_pval(s))
sad_vals <- sapply(s_seq, saddle_pval)
plot(sad_vals ~ s_seq, ylim = c(0,1))
points(saddle_pval(0) ~ 0, pch = 2)
abline(h = 0)
abline(v = 0)
abline(v = s, col = "red")


saddle_pval2 <- function(tstat, eig) {
  g <- c(1, -tstat^2 * eig / sum(eig))
  s_eq <- function(s) sum(g / (1 - 2 * g * s))
  s_range <- if (s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
  s <- uniroot(s_eq, s_range)$root
  r <- sign(s) * sqrt(sum(log(1 - 2 * g * s)))
  q <- s * sqrt( 2 * sum(g^2 / (1 - 2 * g * s)^2))
  p1 <- 1 - pnorm(r) - dnorm(r) * (1 / r - 1 / q)
  p2 <- 0.5 - sum(g^3) / (3 * sqrt(pi) * sum(g^2)^(3/2))
  p <- if (s!=0) p1 else p2
  p_adj <- if (abs(s) > .01) p1 else p2
  c(p = p, q = q, r = r, s = s, p_adj = p_adj)
}

t_seq <- seq(0.05, 2, 0.001)
p_vals <- sapply(t_seq, saddle_pval2, eig = eig)
plot(p_vals["p",] ~ t_seq, type = "l", ylim = c(0,1))
lines(p_vals["p_adj",] ~ t_seq, col = "green")
u <- 1 / p_vals["r",] - 1 / p_vals["q",]
plot(u ~ t_seq, type = "l", ylim = c(0,1))

cbind(t_seq, p_vals["s",], u)[940:960,]
