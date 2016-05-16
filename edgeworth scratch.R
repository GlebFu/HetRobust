library(dplyr)
library(tidyr)
rm(list=ls())

t_crit <- function(alpha, a, b, nu) {
  if (alpha == 0) return(Inf)
  z_a <- qnorm(1 - alpha / 2)
  0.5 * z_a * (2 + z_a^2 * max(0, 1 / nu - a) + 1 / nu  + a - b)
}

p_crit <- function(t_HC, a, b, nu) uniroot(function(alpha) t_crit(alpha, a = a, b = b, nu = nu) - abs(t_HC), lower = 0, upper = 1)$root

sim_MacKinnon <- function(n, p, gamma, beta, const) {
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
  
  f_i <- as.vector(X %*% M %*% colSums(g_i * res_sq * X)) / n
  q_i <- colSums(res_sq * H^2) - 2 * res_sq * diag(H)
  
  a_emp <- sum(omega * g_i^2 * (g_i^2 * res_sq^2 / 3 + f_i^2 - 2 * g_i * f_i * res_sq)) / sum(g_i^2 * res_sq)^2
  b_emp <- sum(omega * q_i * g_i^2) / sum(g_i^2 * res_sq)
  nu_emp <- 6 * sum(omega * g_i^2 * res_sq)^2 / sum(omega^2 * g_i^4 * res_sq^2)
  p_emp_direct <- 2 * (1 - pnorm(0.5 * abs(t_HC) * (2 - (1 + t_HC^2) / nu_emp + a_emp * (t_HC^2 - 1) + b_emp)))
  
  b_hom <- - sum(omega * h_i * g_i^2) / sum(g_i^2)
  nu_hom <- 2 * sum(omega * g_i^2)^2 / sum(omega^2 * g_i^4)
  p_hom_direct <- 2 * (1 - pnorm(0.5 * abs(t_HC) * (2 - (1 + t_HC^2) / nu_hom + b_hom)))
  
  p_emp_crit <- p_crit(t_HC, a = a_emp, b = b_emp, nu = nu_emp)
  p_hom_crit <- p_crit(t_HC, a = 0, b = b_hom, nu = nu_hom)
  
  df <- c(nu_emp, nu_hom)
  
  data.frame(t = t_HC, 
             version = c("emp","hom"), 
             df = df, 
             a = c(a_emp, 0),
             b = c(b_emp, b_hom),
             direct = c(p_emp_direct, p_hom_direct),
             crit = c(p_emp_crit, p_hom_crit),
             tdist = 2 * pt(abs(t_HC), df = df, lower.tail = FALSE))
}

reject_rate <- function(p, alpha) {
  error <- sapply(alpha, function(a) mean(p < a))
  data.frame(alpha = alpha, error = error)
}

run_sim <- function(reps, alpha, n, p, gamma, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  beta <- c(rep(1, p), 0)
  const <- c(rep(0, p), 1)
  replicate(reps, sim_MacKinnon(n, p, gamma, beta, const), simplify = FALSE) %>%
    bind_rows() %>%
    select(version, direct, crit, tdist) %>%
    gather("approx","p", direct, crit, tdist) %>% 
    group_by(version, approx) %>%
    do(reject_rate(.$p, alpha))
}

source_obj <- ls()

alpha <- c(0.005, 0.01, 0.05, 0.10)
params <- expand.grid(gamma = seq(0,2,0.5), 
                      n = seq(20,100,10))
params$reps <- 20000
params$p <- 4
params$seed <- 20160516 + 1:nrow(params)
nrow(params)
head(params)

library(Pusto)
cluster <- start_parallel(source_obj = source_obj, libraries = c("dplyr","tidyr"))

system.time(results <- plyr::mdply(params, .fun = run_sim, alpha = alpha, .parallel = TRUE))

stopCluster(cluster)

save(results, file = "Hausman-Palmer-replication.Rdata")

results %>% 
  spread(alpha, error) %>%
  filter(approx == "crit") %>%
  select(gamma, version, approx, 5:8)

results %>% 
  spread(alpha, error) %>%
  filter(approx == "tdist") %>%
  select(gamma, version, approx, 5:8)

library(ggplot2)

ggplot(results, aes(n, error, color = approx, linetype = version)) + 
  geom_line() + 
  geom_hline(aes(yintercept = alpha), color = "black") + 
  facet_grid(alpha ~ gamma, labeller = "label_both", scales = "free_y") + 
  theme_minimal()
