library(sandwich)
library(lmtest)
n <- 40
p <- 4
gamma <- 1
beta <- c(1,1,1,1,0)
const <- c(0,0,0,0,1)
X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))
sigma_sq_unscaled <- (X %*% beta)^(2 * gamma)
sigma <- sqrt(sigma_sq_unscaled / mean(sigma_sq_unscaled))
y <- X %*% beta + rnorm(n, sd = sigma)

lm_fit <- lm.fit(X, y)
beta_hat <- lm_fit$coefficients
res <- lm_fit$residuals
M <- n * chol2inv(chol(crossprod(X)))
H <- X %*% M %*% t(X) / n
h_i <- diag(H)
omega <- rep(1, n)
g_i <- as.vector(X %*% M %*% const)
res_sq <- res^2
V_HC <- sum(omega * g_i^2 * res_sq) / n^2
t_HC <- sum(const * beta_hat) / sqrt(V_HC)

# lm_long <- lm(y ~ X)
# all.equal(h_i, hatvalues(lm_long), check.attributes = FALSE)
# vcov_HC <- vcovHC(lm_long, type = "HC0")
# all.equal(V_HC, sum(tcrossprod(const) * vcov_HC))
# all.equal(t_HC, coeftest(lm_long, vcov. = vcov_HC)[5,3])

z_i <- res_sq * g_i - as.vector(H %*% (res_sq * g_i))
q_i <- diag(H %*% (res_sq * H)) - 2 * res_sq * diag(H)

a_emp <- sum(omega * g_i^2 * z_i^2) / sum(g_i^2 * res_sq)^2
b_emp <- sum(omega * q_i * g_i^2) / sum(g_i^2 * res_sq)
nu_emp <- 6 * sum(omega * g_i^2 * res_sq)^2 / sum(omega^2 * g_i^4 * res_sq^2)
p_emp_direct <- 2 * (1 - pnorm(0.5 * abs(t_HC) * (2 - (1 + t_HC^2) / nu_emp + a_emp * (t_HC^2 - 1) + b_emp)))

b_hom <- - sum(omega * h_i * g_i^2) / sum(g_i^2)
nu_hom <- 2 * sum(omega * g_i^2)^2 / sum(omega^2 * g_i^4)
p_hom_direct <- 2 * (1 - pnorm(0.5 * abs(t_HC) * (2 - (1 + t_HC^2) / nu_hom + b_hom)))
  
t_crit <- function(alpha, a, b, nu) {
  if (alpha == 0) return(Inf)
  z_a <- qnorm(1 - alpha / 2)
  0.5 * z_a * (2 + z_a^2 * max(0, 1 / nu - a) + 1 / nu  + a - b)
}

p_crit <- function(t_HC, a, b, nu) 
  uniroot(function(alpha) t_crit(alpha, a = a, b = b, nu = nu) - abs(t_HC), 
                        lower = 0, upper = 1)$root

p_emp_crit <- p_crit(t_HC, a = a_emp, b = b_emp, nu = nu_emp)
p_hom_crit <- p_crit(t_HC, a = 0, b = b_hom, nu = nu_hom)

2 * pnorm(abs(t_HC), lower.tail = FALSE)
data.frame(t = t_HC, 
           version = c("emp","hom"), 
           df = c(nu_emp, nu_hom), 
           a = c(a_emp, 0),
           b = c(b_emp, b_hom),
           direct = c(p_emp_direct, p_hom_direct),
           crit = c(p_emp_crit, p_hom_crit))
1 / nu_emp - a_emp
