rescale_sd <- function(p, gamma, beta, iterations) {
  X <- cbind(1, matrix(exp(rnorm(iterations * p)), iterations, p))
  sigma_sq_unscaled <- (X %*% beta)^(2 * gamma)
  1 / sqrt(mean(sigma_sq_unscaled))
}

gamma <- seq(0,2,0.1)
f <- sapply(gamma, rescale_sd, p = 4, beta = c(1,1,1,1,0), iterations = 10^6)
cbind(gamma, f)
plot(f ~ gamma, type = "l")

sim_MacKinnon <- function(n, p, beta, gamma, f) {
  X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))
  sigma <- f * (X %*% beta)^gamma
  y <- X %*% beta + rnorm(n, sd = sigma)
  
  lm_fit <- lm.fit(X, y)
  beta_hat <- lm_fit$coefficients
  res <- lm_fit$residuals
  M <- chol2inv(chol(crossprod(X)))
  sd_hom <- sqrt(diag(M) * sum(res^2) / (n - p - 1))
  sd_het <- sqrt(diag(crossprod(res * X %*% M)) * n / (n - p - 1))
  cbind(hom = sd_hom, het = sd_het)
}

n <- 40
p <- 4
beta <- c(1,1,1,1,0)
gamma <- 1
f <- rescale_sd(p, gamma, beta, iterations = 10^7)
SD_replicates <- replicate(10000, sim_MacKinnon(n, p, beta, gamma, f))
SD_comparison <- apply(SD_replicates, c(1,2), mean)
SD_comparison[,"het"] / SD_comparison[,"hom"]
