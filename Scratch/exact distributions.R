library(CompQuadForm)

n <- 20
p <- 5
const <- c(rep(0, p), 1)
X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))

M <- chol2inv(chol(crossprod(X)))
H <- X %*% M %*% t(X)
h_i <- diag(H)
omega <- 1 / (1 - h_i)
g <- as.vector(X %*% M %*% const)
sigma <- rchisq(n, df = 8)

#---------------------------
# Setup stuff
#---------------------------

# setup calculations

setup_calcs <- function(X, const) {
  M <- chol2inv(chol(crossprod(X)))
  H <- X %*% M %*% t(X)
  g <- as.vector(X %*% M %*% const)
  list(H = H, g = g)
}

# Satterthwaite degrees of freedom

Satterthwaite_df <- function(g, H, omega, sigma = NULL) {
  if (is.null(sigma)) {
    h_ii <- diag(H)
    sum((1 - h_ii) * omega * g^2)^2 / (sum((1 - 2 * h_ii) * omega^2 * g^4) + sum(H^2 * tcrossprod(omega) * tcrossprod(g^2)))
  } else {
    I_H <- diag(nrow(H)) - H
    a_vec <- omega * g^2
    B <- I_H %*% (a_vec * I_H)
    sum(diag(B) * sigma)^2 / sum(B^2 * tcrossprod(sigma))
  }
}

# Edgeworth constants

Edgeworth_constants <- function(g, H, omega, sigma = NULL) {
  
}

df <- Satterthwaite_df(g, H, omega)
alpha <- 0.05

#---------------------------
# Compute critical values 
#---------------------------

# Satterthwaite critical value
Satterthwaite_critical <- function(df, alpha) qt(1 - alpha / 2, df = df)

Satterthwaite_critical(df, alpha)

# Kauermann-Carroll critical values

alpha_approx <- function(a, df, alpha) {
  z_a <- qnorm(1 - a / 2)
  a - alpha + dnorm(z_a) * (z_a^3 + z_a) / (2 * df)
}

KC_critical_A <- function(df, alpha) {
  a_tilde <- uniroot(alpha_approx, df = df, lower = alpha^10, upper = alpha, alpha = alpha)$root
  qnorm(1 - a_tilde / 2)
}

KC_critical_B <- function(df, alpha, g, n, p) {
  z_a <- qnorm(1 - alpha / 2)
  qt(1 - alpha / 2, n - p) + (z_a^3 + z_a) * (1 / df - sum(g^2)^2 / (n - p)) / 4
}

KC_critical_A(df, alpha)
KC_critical_B(df, alpha, g, n, p)

# Rothenberg critical values


# saddlepoint critical values




