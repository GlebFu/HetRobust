
# Updated 2017-04-10 by JEP
# Updated 2017-04-02 by JEP

#-----------------------------------
# p-value from a two-sided t-test 
#-----------------------------------

t_test <- function(t_stats, df) {
  2 * pt(abs(t_stats), df = df, lower.tail = FALSE)
}

#-----------------------------------
# Satterthwaite degrees of freedom 
#-----------------------------------

Satterthwaite_empirical <- function(sd, X_M, omega, e_sq, H, I_H) {
  
  sigma_hat <- tcrossprod(omega * e_sq) / (2 * tcrossprod(omega) * H^2 + 1)
  diag(sigma_hat) <- (omega * e_sq)^2 / 3
  
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

Satterthwaite_true <- function(X_M, omega, sigma_sq, I_H) {
  
  A_vec <- omega * X_M^2
  
  df_A <- function(a) {
    BSigma <- (sigma_sq * I_H) %*% (a * I_H)
    sum(diag(BSigma))^2 / sum(BSigma^2)
  }
  
  apply(A_vec, 2, df_A)
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

saddle <- function(t_stats, X_M, omega, e_sq, H, I_H, n, p, approx = "model") {
  if (!all(is.finite(t_stats))) {
    return(rep(1,length(t_stats)))
  } else {
    A_vec <- omega * X_M^2
    if (approx == "model") {
      Qs <- apply(A_vec, 2, function(x) I_H %*% (x * I_H))
    } else {
      Qs <- apply(A_vec, 2, function(x) (e_sq * I_H) %*% (x * I_H))
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


z_alpha_HC <- function(alpha, t_stat, df, m, n, p) { 
  z_crit <- qnorm(1 - alpha / 2)
  t_crit <- qt(1 - alpha / 2, df = n - p)
  abs(t_stat) - t_crit - (z_crit^3 + z_crit) * (1 / df - m^2 / (n - p)) / 4
}

find_p_HC <- function(t_stat, df, m, n, p, LB = 10^-6) {
  z_bounds <- z_alpha_HC(c(LB,1 - LB), t_stat = t_stat, df = df, m = m, n = n, p = p)
  if (prod(sign(z_bounds)) < 0) {
    p <- uniroot(z_alpha_HC, interval = c(LB, 1 - LB), 
                 t_stat = t_stat, df = df, m = m, n = n, p = p)$root  
  } else if (all(z_bounds > 0)) {
    p <- LB
  } else {
    p <- 1 - LB
  }
  p
}

KC_CI_exact <- function(t_stats, df, m, n, p) {
  mapply(find_p_HC, t_stat = t_stats, df = df, m = m, MoreArgs = list(n = n, p = p))
}

#-------------------------------------------
# Rothenberg Edgeworth approximations
#-------------------------------------------

z_alpha_Roth <- function(alpha, t_stat, df, a, b) { 
  z_crit <- qnorm(1 - alpha / 2)
  abs(t_stat) - (z_crit + (z_crit^3 + z_crit) / (4 * df) - z_crit * (a * (z_crit^2 - 1) + b) / 2)
}

find_p_Roth <- function(t_stat, df, a, b, LB = 10^-6) {
  z_init <- z_alpha_Roth(LB, t_stat = t_stat, df = df, a = a, b = b)
  if (z_init < 0) {
    p <- uniroot(z_alpha_Roth, interval = c(LB, 1 - LB), 
                 t_stat = t_stat, df = df, a = a, b = b)$root  
  } else {
    p <- LB
  }
  p
}

Rothenberg_pvals <- function(t_stats, sd, X_M, H, h, I_H, omega, e, df, alphas, approx = "model", exact = FALSE) {
  
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

  if (exact) {
    CI <- mapply(find_p_Roth, t_stat = t_stats, df = df, a = a_vec, b = b_vec)
  } else {
    z_crit <- qnorm(1 - alphas / 2)
    test_t <- function(t, df, a, b) {
      z_alpha <- z_crit + (z_crit^3 + z_crit) / (4 * df) - z_crit * (a * (z_crit^2 - 1) + b) / 2
      min(c(alphas[abs(t) > z_alpha],1))
    }
    CI <- mapply(test_t, t = t_stats, df = df, a = a_vec, b = b_vec)  
  }
  
  
  data.frame(p_val, CI)
}

#-----------------------------------
# testing functions
#-----------------------------------

test_HC <- function(HC, model, tests, alphas, span = 0.75) {
  
  omega <- with(model, switch(HC,
                              HC0 = rep(1, n),
                              HC1 = rep(n / (n - p), n),
                              HC2 = 1 / (1 - h),
                              HC3 = (1 - h)^(-2),
                              HC4 = (1 - h)^(-pmin(h * n / p, 4)),
                              HC4m = (1 - h)^(-(pmin(h * n / p, 1) + pmin(h * n / p, 1.5))),
                              HC5 = (1 - h)^(-pmin(h * n / p, pmax(4, .7 * n * max(h) / p)))
  ))
  
  sd <- with(model, sqrt(colSums(omega * (X_M * e)^2)))
  
  pValues <- data.frame(coef = names(model$coefs))
  
  t_stats <- model$coefs / sd
  
  # naive t tests
  
  if ("naive" %in% tests) {
    pValues$naive <- t_test(t_stats = t_stats, df = model$n - model$p)
  }

  # smoothed errors
  if (any(c("Satt_S","KCp_S","KCCI_S","Rp_S","RCI_S","saddle_S") %in% tests)) {
    e_sq_S <- predict(loess(omega * model$e^2 ~ model$X[,"x1"], span = span, 
                            loess.control = loess.control(statistics = "none")))
  }
  
  # degrees of freedom
  
  if (any(c("Satt_E","KCp_E","KCCI_E","Rp_E","RCI_E") %in% tests)) {
    df_E <- Satterthwaite_empirical(sd = sd, X_M = model$X_M, 
                                    omega = omega, e_sq = model$e^2, 
                                    H = model$H, I_H = model$I_H)
  }
  
  
  if (any(c("Satt_H","KCp_H","KCCI_H","Rp_H","RCI_H") %in% tests)) {
    df_H <- Satterthwaite_model(X_M = model$X_M, omega = omega, 
                                H = model$H, h = model$h)
  }
  
  if (any(c("Satt_S","KCp_S","KCCI_S","Rp_S","RCI_S") %in% tests)) {
    df_S <- Satterthwaite_true(X_M = model$X_M, omega = omega, sigma_sq = e_sq_S, I_H = model$I_H)
  }
  
  if (any(c("Satt_T","KCp_T","KCCI_T","Rp_T","RCI_T") %in% tests)) {
    df_T <- Satterthwaite_true(X_M = model$X_M, omega = omega, sigma_sq = model$sigma^2, I_H = model$I_H)
  }
  
  # Satterthwaite tests

  if ("Satt_E" %in% tests) pValues$Satt_E <- t_test(t_stats = t_stats, df = df_E)
  if ("Satt_H" %in% tests) pValues$Satt_H <- t_test(t_stats = t_stats, df = df_H)
  if ("Satt_S" %in% tests) pValues$Satt_S <- t_test(t_stats = t_stats, df = df_S)
  if ("Satt_T" %in% tests) pValues$Satt_T <- t_test(t_stats = t_stats, df = df_T)

  # Kauermann-Carroll edgeworth approximations
  if ("KCp_E" %in% tests) pValues$KCp_E <- KC_pval(t_stats = t_stats, df = df_E)
  if ("KCp_H" %in% tests) pValues$KCp_H <- KC_pval(t_stats = t_stats, df = df_H)
  if ("KCCI_E" %in% tests) {
    pValues$KCCI_E <- KC_CI_exact(t_stats = t_stats, df = df_E, 
                                  m = model$M_diag, n = model$n, p = model$p)
  }
  if ("KCCI_H" %in% tests) {
    pValues$KCCI_H <- KC_CI_exact(t_stats = t_stats, df = df_H, 
                                  m = model$M_diag, n = model$n, p = model$p)
  }
  
  # Rothenberg's edgeworth approximations
  
  if ("Rp_E" %in% tests | "RCI_E" %in% tests) {
    a <- if (is.vector(alphas)) alphas else alphas$alphas[[which(alphas$test=="Rp_E")]]
    Roth_E <- Rothenberg_pvals(t_stats, sd = sd, X_M = model$X_M, 
                               H = model$H, h = model$h, I_H = model$I_H, 
                               omega = omega, e = model$e, 
                               df = df_E, alphas = alphas, approx = "empirical", exact = TRUE)
    if ("Rp_E" %in% tests) pValues$Rp_E <- Roth_E$p_val
    if ("RCI_E" %in% tests) pValues$RCI_E <- Roth_E$CI
  }
  if ("Rp_H" %in% tests | "RCI_H" %in% tests) {
    a <- if (is.vector(alphas)) alphas else alphas$alphas[[which(alphas$test=="Rp_H")]]
    Roth_H <- Rothenberg_pvals(t_stats, sd = sd, X_M = model$X_M, 
                               H = model$H, h = model$h, I_H = model$I_H, 
                               omega = omega, e = model$e, 
                               df = df_H, alphas = alphas, approx = "model", exact = TRUE)
    if ("Rp_H" %in% tests) pValues$Rp_H <- Roth_H$p_val
    if ("RCI_H" %in% tests) pValues$RCI_H <- Roth_H$CI
  }
  
  # saddlepoint approximations
  
  if ("saddle_E" %in% tests) {
    pValues$saddle_E <- saddle(t_stats = t_stats, X_M = model$X_M, 
                               omega = omega, e_sq = model$e^2,
                               H = model$H, I_H = model$I_H, n = model$n, 
                               p = model$p, approx = "empirical")
  }
  if ("saddle_H" %in% tests) {
    pValues$saddle_H <- saddle(t_stats = t_stats, X_M = model$X_M, 
                             omega = omega, e_sq = model$e^2, 
                             H = model$H, I_H = model$I_H, n = model$n, 
                             p = model$p, approx = "model")
  }
  if ("saddle_S" %in% tests) {
    pValues$saddle_S <- saddle(t_stats = t_stats, X_M = model$X_M, 
                               omega = omega, e_sq = e_sq_S,
                               H = model$H, I_H = model$I_H, n = model$n, 
                               p = model$p, approx = "empirical")
  }
  if ("saddle_T" %in% tests) {
    pValues$saddle_T <- saddle(t_stats = t_stats, X_M = model$X_M, 
                               omega = omega, e_sq = model$sigma^2,
                               H = model$H, I_H = model$I_H, n = model$n, 
                               p = model$p, approx = "empirical")
  }
  
  return(pValues)
  
}


test_hom <- function(model) {
  
  sd <- with(model, sqrt(sum(e^2) / (n - p) * M_diag))
  t_stats <- model$coefs / sd
  data.frame(
    coef = names(model$coefs),
    naive = t_test(t_stats, df = model$n - model$p)
  )
  
}

run_tests <- function(HC, model, tests, alphas, span = 0.75) {
  if (HC == "hom") {
    test_hom(model)
  } else {
    test_HC(HC, model, tests, alphas, span)
  }
}

#-----------------------------------
# calculate rejection rates
#-----------------------------------

reject_rates <- function(p_vals, alphas, ..., adjusted_alpha = FALSE) {
  p_vals <- p_vals$p_val
  rejects <- lapply(alphas, function(a) ifelse(is.na(p_vals), FALSE, p_vals <= a))
  rejection_rates <- data.frame(percent_NA = mean(is.na(p_vals)), lapply(rejects, mean))
  
  if (adjusted_alpha) {
    adjusted_alphas <- quantile(p_vals, alphas)
    rejection_rates <- rbind(rejection_rates, c(NA, adjusted_alphas))
    rejection_rates$stat <- c("rejection rate", "adjusted alpha")
  }
  
  rejection_rates
}

calculate_rejections <- function(x, alphas, ..., adjusted_alpha = FALSE) {
  
  y <- 
    unnest(x) %>%
    gather("test", "p_val", -1) %>%
    group_by_(.dots = c("coef", "test")) %>%
    nest(.key = "p_vals") 
  
  if (is.vector(alphas)) {
    y$alphas <- rep(list(alphas), nrow(y))
  } else {
    y <- left_join(y, alphas, by = c("coef","test"))
  }
  
  y %>%
    invoke_rows(.f = reject_rates, adjusted_alpha = adjusted_alpha) %>%
    select_(.dots = c("coef","test",".out")) %>%
    unnest()
}

#-----------------------------
# Fit regression model
#-----------------------------

estimate_model <- function(Y, X, whichX) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- chol2inv(chol(t(X) %*% X))
  X_M <- X %*% M
  coefs <- colSums(Y * X_M)
  names(coefs) <- colnames(X)
  
  e <- Y - as.vector(X %*% coefs)
  
  H <- X_M %*% t(X)
  h <- diag(H)
  I_H <- - H
  diag(I_H) <- 1 - h
  
  values <- list(X = X, 
                 Y = Y, 
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
# simulation driver
#-----------------------------------

run_sim <- function(dgm, iterations, n, alphas, HCtests, span = 0.75, adjusted_alpha = FALSE, seed = NULL, ...) {
  
  require(purrr, quietly = TRUE, warn.conflicts = FALSE)
  require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  
  if (!is.null(seed)) set.seed(seed)
  
  # handle alphas as vector vs. as tibble
  
  if (is.vector(alphas)) {
    test_alphas <- HCtests
    test_alphas$alphas <- rep(list(alphas), nrow(test_alphas))
  } else {
    test_alphas <- 
      alphas %>%
      group_by_(.dots = "HC") %>%
      nest(.key = "alphas") %>%
      right_join(HCtests, by = "HC")
  }
  
  # simulate
  
  res <- 
    rerun(.n = iterations, {
      model <- dgm(n = n, ...)
      invoke_rows(.f = run_tests, .d = test_alphas, 
                  model = model, span = span,
                  .to = "res")
    }) %>%
    bind_rows() %>%
    select_(.dots = c("HC", "res"))
  
  res %>%
    group_by_(.dots = "HC") %>%
    nest(.key = "x") %>%
    left_join(test_alphas, by = "HC") %>%
    invoke_rows(.f = calculate_rejections, adjusted_alpha = adjusted_alpha, .to = "reject_rates") %>%
    select_(.dots = c("HC", "reject_rates")) %>%
    unnest()
  
}
