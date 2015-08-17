#-----------------------------
# NA producing Models
#-----------------------------

rm(list = ls())

source("hetRobust.R")
load("Problem Models.Rdata")

estimate(HC = "HC5", tests = "saddle", model = badmods[[1]])
estimate(HC = "HC1", tests = "saddle", model = badmods[[2]])
estimate(HC = "HC0", tests = "saddle", model = badmods[[3]])
estimate(HC = "HC0", tests = "saddle", model = badmods[[4]])

HC <- "HC5"
tests <- "saddle"
model <- badmods[[1]]
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

# saddlepoints
saddle(coef = coefs_to_test, sd = sqrt(V_b), X_M = X_M, omega = omega, e = e, H = H, n = n, approx = "model")
saddle(coef = coefs_to_test, sd = sqrt(V_b), X_M = X_M, omega = omega, e = e, H = H, n = n, approx = "empirical")

coef <- coefs_to_test
sd <- sqrt(V_b)
approx <- "model"
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

# saddlepoint for one coefficient
k <- 1
saddlepoint_pval(t = t_stats[k], Q = Qs[[k]])
t <- t_stats[k]
Q <- Qs[[k]]
eig <- pmax(0, eigen(Q, symmetric = TRUE, only.values=TRUE)$values)
g <- c(1, -t^2 * eig / sum(eig))
s_eq <- function(s) sum(g / (1 - 2 * g * s))
s_range <- if (s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
s <- uniroot(s_eq, s_range)$root

p_val <- function(s) {
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

s_seq <- seq(s - 0.1,s + 0.1,0.001)
s_seq <- s_seq[s_seq > s_range[1] & s_seq < s_range[2]]
p_vals <- sapply(s_seq, p_val)
plot(p_vals ~ s_seq, type = "l")
points(s, p_val(s), col = "red")
points(s, p_val(0), col = "blue")

#-----------------------------
# pValues
#-----------------------------
rm(list = ls())
source("hetRobust.R")

testmod <- gdm(n = 25, 
               B = c(1, 1, 1, 1, 0, 0), 
               Estruct = "E0", 
               whichX = c(T, T ,T ,T ,T ,F), 
               Edist = "En")

estimate(HC = "HC0",
         tests = c("naive", "Satt", "saddle", "edgeKC"),
         model = testmod)

debug(estimate)
