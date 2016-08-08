
#-----------------------------------
# p-value from a two-sided t-test 
#-----------------------------------

t_test <- function(t_stats, df) {
  2 * pt(abs(t_stats), df = df, lower.tail = FALSE)
}

#-----------------------------------
# Satterthwaite degrees of freedom 
#-----------------------------------

Satterthwaite_empirical <- function(sd, X_M, omega, e, H, I_H) {
  
  sigma_hat <- tcrossprod(omega * e^2) / (2 * tcrossprod(omega) * H^2 + 1)
  diag(sigma_hat) <- (omega^2) * (e^4) / 3
  
  A_vec <- omega * X_M^2
  
  V_V <- function(a) {
    B <- I_H %*% (a * I_H)
    sum(as.vector(t(B)) * as.vector(B * sigma_hat))
  }
  den <- apply(A_vec, 2, V_V)
  
  return(sd^4 / den)
}

Satterthwaite_model <- function(X_M, omega, H, h) {
  
  df_A <- function(g) {
    x <- (1 - h) * omega * g^2
    B <- H^2 * tcrossprod(omega * g^2)
    diag(B) <- x^2
    sum(x)^2 / sum(B)
  }
  
  apply(X_M, 2, df_A)
}

#-----------------------------------
#Saddlepoint approximation
#-----------------------------------

saddlepoint_pval <- function(t, Q, d) {
  
  eig <- pmax(0, eigen(Q, symmetric = TRUE, only.values=TRUE)$values[1:d])
  g <- c(1, -t^2 * eig / sum(eig))
  
  s_eq <- function(s) sum(g / (1 - 2 * g * s))
  s_range <- if (s_eq(0) > 0) c(1 / (2 * min(g)), 0) else c(0, 1 / (2 * max(g)))
  s <- uniroot(s_eq, s_range)$root
  
  if (abs(s) > .01) {
    r <- sign(s) * sqrt(sum(log(1 - 2 * g * s)))
    q <- s * sqrt(2 * sum(g^2 / (1 - 2 * g * s)^2))
    p_val <- 1 - pnorm(r) - dnorm(r) * (1 / r - 1 / q)
  } else {
    p_val <- 0.5 - sum(g^3) / (3 * sqrt(pi) * sum(g^2)^(3/2))
  }
  
  return(p_val)
}

saddle <- function(t_stats, X_M, omega, e, H, I_H, n, p, approx = "model") {
  if (!all(is.finite(t_stats))) {
    return(rep(1,length(t_stats)))
  } else {
    A_vec <- omega * X_M^2
    if (approx == "model") {
      Qs <- apply(A_vec, 2, function(x) I_H %*% (x * I_H))
    } else {
      Qs <- apply(A_vec, 2, function(x) (e^2 * I_H) %*% (x * I_H))
    }
    Qs <- lapply(data.frame(Qs), matrix, nrow = n, ncol = n)
    p_vals <- mapply(saddlepoint_pval, t = t_stats, Q = Qs, d = n - p)
    return(p_vals)
  }
}


#------------------------------------------------
# Kauermann-Carroll Edgeworth approximations
#------------------------------------------------

KC_pval <- function(t_stats, df) {
  2 * (1 - pnorm(abs(t_stats))) + (abs(t_stats)^3 + abs(t_stats)) * dnorm(t_stats) / (2 * df)
}

KC_CI <- function(t_stats, df, m, n, p, alphas) {
  z_crit <- qnorm(1 - alphas / 2)
  t_crit <- qt(1 - alphas / 2, df = n - p)
  
  test_t <- function(t, df, m) {
    z_alpha <- t_crit + (z_crit^3 + z_crit) * (1 / df - m^2 / (n - p)) / 4
    min(c(alphas[abs(t) > z_alpha],1))
  }
  
  mapply(test_t, t = t_stats, df = df, m = m)
}

#-------------------------------------------
# Rothenberg Edgeworth approximations
#-------------------------------------------

Rothenberg_pvals <- function(t_stats, sd, X_M, H, h, I_H, omega, e, df, alphas, approx = "model") {
  
  X_M_sq <- X_M^2
  
  if (approx=="model") {
    a_vec <- 0
    b_vec <- -colSums(omega * h * X_M_sq) / colSums(X_M_sq)
  } else {
    sigma <- e^2
    q_ii <- colSums(I_H^2 * sigma) - sigma
    f_i <- I_H %*% (sigma * X_M)
    a_vec <- colSums(omega * f_i^2 * X_M_sq) / sd^4
    b_vec <- colSums(omega * q_ii * X_M_sq) / sd^2
  }
  
  
  p_val <- 2 * (1 - pnorm(abs(t_stats) * pmax(0, 2 - (1 + t_stats^2) / (2 * df) + a_vec * (t_stats^2 - 1) + b_vec) / 2))
  p_val <- pmin(1, p_val)
  
  z_crit <- qnorm(1 - alphas / 2)
  test_t <- function(t, df, a, b) {
    z_alpha <- z_crit + (z_crit^3 + z_crit) / (4 * df) - z_crit * (a * (z_crit^2 - 1) + b) / 2
    min(c(alphas[abs(t) > z_alpha],1))
  }
  CI <- mapply(test_t, t = t_stats, df = df, a = a_vec, b = b_vec)
  
  data.frame(p_val, CI)
}

#-----------------------------------
# testing function
#-----------------------------------

estimate <- function(HC, model, alphas, power) {
  
  omega <- with(model, switch(HC,
                              HC0 = rep(1,n),
                              HC1 = rep(n / (n - p), n),
                              HC2 = 1 / (1 - h),
                              HC3 = (1 - h)^(-2),
                              HC4 = (1 - h)^(-pmin(h * n / p, 4)),
                              HC4m = (1 - h)^(-(pmin(h * n / p, 1) + pmin(h * n / p, 1.5))),
                              HC5 = (1 - h)^(-pmin(h * n / p, pmax(4, .7 * n * max(h) / p)) / 2)
  ))
  
  sd <- with(model, sqrt(colSums(omega * (X_M * e)^2)))
  
  if (power) {
    coefs_to_test <- with(model, c(coefs - B, coefs))
    pValues <- data.frame(HC = HC, 
                          coef = names(coefs_to_test), 
                          criterion = rep(c("size","power"), each = length(model$coefs)))
  } else {
    coefs_to_test <- with(model, coefs - B)
    pValues <- data.frame(HC = HC,
                          coef = names(coefs_to_test),
                          criterion = "size")
  }
  
  t_stats <- coefs_to_test / sd
  
  # naive t tests
  pValues$naive <- t_test(t_stats = t_stats, df = model$n - model$p)
  
  # Satterthwaite tests
  df_E <- Satterthwaite_empirical(sd = sd, X_M = model$X_M, 
                                  omega = omega, e = model$e, 
                                  H = model$H, I_H = model$I_H)
  df_H <- Satterthwaite_model(X_M = model$X_M, omega = omega, 
                              H = model$H, h = model$h)
  
  pValues$Satt_E <- t_test(t_stats = t_stats, df = df_E)
  pValues$Satt_H <- t_test(t_stats = t_stats, df = df_H)
  
  # Kauermann-Carroll edgeworth approximations
  pValues$KCp_E <- KC_pval(t_stats = t_stats, df = df_E)
  pValues$KCp_H <- KC_pval(t_stats = t_stats, df = df_H)
  pValues$KCCI_E <- KC_CI(t_stats = t_stats, df = df_E, 
                          m = model$M_diag, n = model$n, p = model$p, alphas = alphas)
  pValues$KCCI_H <- KC_CI(t_stats = t_stats, df = df_H, 
                          m = model$M_diag, n = model$n, p = model$p, alphas = alphas)
  
  # Rothenberg's edgeworth approximations
  Roth_E <- Rothenberg_pvals(t_stats, sd = sd, X_M = model$X_M, 
                             H = model$H, h = model$h, I_H = model$I_H, 
                             omega = omega, e = model$e, 
                             df = df_E, alphas = alphas, approx = "empirical")
  pValues$Rp_E <- Roth_E$p_val
  pValues$RCI_E <- Roth_E$CI
  
  Roth_H <- Rothenberg_pvals(t_stats, sd = sd, X_M = model$X_M, 
                             H = model$H, h = model$h, I_H = model$I_H, 
                             omega = omega, e = model$e, 
                             df = df_H, alphas = alphas, approx = "model")
  pValues$Rp_H <- Roth_H$p_val
  pValues$RCI_H <- Roth_H$CI
  
  # saddlepoint approximations
  pValues$saddle_E <- saddle(t_stats = t_stats, X_M = model$X_M, 
                             omega = omega, e = model$e,
                             H = model$H, I_H = model$I_H, n = model$n, 
                             p = model$p, approx = "empirical")
  pValues$saddle_H <- saddle(t_stats = t_stats, X_M = model$X_M, 
                             omega = omega, e = model$e, 
                             H = model$H, I_H = model$I_H, n = model$n, 
                             p = model$p, approx = "model")
  
  # round(pValues[1:5,-(1:3)],4)
  # round(pValues[6:10,-(1:3)],4)
  return(pValues)
  
}

#-----------------------------
# Fit regression model
#-----------------------------

estimate_model <- function(Y, X, trueB, whichX) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- chol2inv(chol(t(X) %*% X))
  X_M <- X %*% M
  coefs <- colSums(Y * X_M)
  names(coefs) <- colnames(X)
  
  e <- Y - as.vector(X %*% coefs)
  
  H <- X_M %*% t(X)
  I_H <- diag(n) - H
  h <- diag(H)
  
  values <- list(X = X, 
                 Y = Y, 
                 B = trueB[whichX], 
                 coefs = coefs[whichX], 
                 M_diag = diag(M)[whichX], 
                 X_M = X_M[, whichX, drop=FALSE], 
                 H = H, 
                 h = h, 
                 I_H = I_H, 
                 e = e, 
                 n = n, 
                 p = p)
  
  return(values)
}

#-----------------------------------
# calculate rejection rates
#-----------------------------------

reject_rates <- function(p_vals, alpha) {
  mean(ifelse(is.na(p_vals), FALSE, p_vals <= alpha))
}
# 
# reject_rates1 <- function(p_vals, alphas) {
#   rejects <- lapply(alphas, function(a) ifelse(is.na(p_vals), FALSE, p_vals <= a))
#   data.frame(percent_NA = mean(is.na(p_vals)), 
#              lapply(rejects, mean))
# }


#-----------------------------------
# calculate rejection rates
#-----------------------------------

OLS <- function(model, power) {
  if (power) {
    coefs_to_test <- with(model, c(coefs - B, coefs))
    pValues <- data.frame(HC = "OLS", 
                          coef = names(coefs_to_test), 
                          criterion = rep(c("size","power"), each = length(model$coefs)))
  } else {
    coefs_to_test <- with(model, coefs - B)
    pValues <- data.frame(HC = "OLS",
                          coef = names(coefs_to_test),
                          criterion = "size")
  }
  
  sd <- with(model, sqrt(sum(e^2) / (n - p) * M_diag))
  
  t_stats <- coefs_to_test / sd
  pValues$naive <- t_test(t_stats, df = model$n - model$p)
  
  pValues
}

#-----------------------------------
# simulation driver
#-----------------------------------

# iterations <- 40
# n <- 25
# B <- "1 1 1 1 0"
# whichX <- "T T T T T"
# g <- 0
# zg <- 1
# HC <- "HC0 HC1 HC2 HC3 HC4 HC4m HC5"
# alpha_string <- ".005 .01 .05 .10"
# seed <- 1234
# 
# t0 <- runSim(dgm = Long_dgm,
#         iterations = iterations,
#         n = n,
#         B = B,
#         whichX = whichX,
#         HC = HC,
#         alpha_string = alpha_string,
#         seed = seed)

# t1 <- runSim2(dgm = Long_dgm,
#         iterations = iterations,
#         n = n,
#         B = B,
#         whichX = whichX,
#         HC = HC,
#         alpha_string = alpha_string,
#         seed = seed)


runSim <- function(dgm, iterations, n, B, whichX, HC, alpha_string, 
                   power = FALSE, seed = NULL,...) {
  require(reshape2)
  require(plyr)
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("The plyr package must be installed")
  }
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  HC <- as.character(unlist(strsplit(HC, " ")))
  alphas <- as.numeric(unlist(strsplit(alpha_string, " ")))
  names(alphas) <- paste0("p",as.character(unlist(strsplit(alpha_string, " "))))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- plyr::rdply(iterations, {
    model <- dgm(n = n, B = B, whichX = whichX, ...)
    results <- plyr::ldply(HC, estimate, model = model, alphas = alphas, power = power)
    oresults <- OLS(model, power = power)
    merge(results, oresults, all = T)
  })
  

  # performance calculations
  repsL <- melt(reps[,-1], measure.vars = names(reps[5:ncol(reps)]), variable.name = "test", value.name = "pval")
  ddply(repsL, .(HC, coef, criterion, test), summarize, 
        percent_NA = mean(is.na(pval)),
        p.005 = reject_rates(pval, .005), 
        p.010 = reject_rates(pval, .010), 
        p.050 = reject_rates(pval, .050), 
        p.100 = reject_rates(pval, .100))
  #aggregate(pval ~ HC + coef + criterion + test, repsL, function(x) reject_rates(x, alphas))
}

# runSim2 <- function(dgm, iterations, n, B, whichX, HC, alpha_string, 
#                    power = FALSE, seed = NULL,...) {
#   require(tidyr)
#   require(dplyr)
#   if (!requireNamespace("plyr", quietly = TRUE)) {
#     stop("The plyr package must be installed")
#   }
#   
#   B <- as.numeric(unlist(strsplit(B, " ")))
#   whichX <- as.logical(unlist(strsplit(whichX, " ")))
#   HC <- as.character(unlist(strsplit(HC, " ")))
#   alphas <- as.numeric(unlist(strsplit(alpha_string, " ")))
#   names(alphas) <- paste0("p",as.character(unlist(strsplit(alpha_string, " "))))
#   
#   if (!is.null(seed)) set.seed(seed)
#   
#   reps <- plyr::rdply(iterations, {
#     model <- dgm(n = n, B = B, whichX = whichX, ...)
#     results <- plyr::ldply(HC, estimate, model = model, alphas = alphas, power = power)
#     oresults <- OLS(model, power = power)
#     merge(results, oresults, all = T)
#   })
#   
#   # performance calculations
#   reps %>% 
#     select(-1) %>%
#     gather("test","pval", naive, ends_with("_E"), ends_with("_H")) %>%
#     group_by(HC, coef, criterion, test) %>%
#     do(reject_rates1(.$pval, alphas = alphas))
# }