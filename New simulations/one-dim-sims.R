library(tibble)
library(tidyr)
library(purrr)
library(dplyr)

rm(list=ls())

# x_skew <- c(0.5, 1, 2)
# 8 / x_skew^2
# f <- function(x, v) dchisq(v + x * sqrt(2 * v), df = v) * sqrt(2 * v)
# curve(f(x, 2) , from = -4, to = 4)
# curve(f(x, 8) , add = TRUE, col = "blue")
# curve(f(x, 32) , add = TRUE, col = "red")
# 
# curve(exp(0 * x), from = -4, to = 4, ylim = c(0,2))
# curve(exp(0.1 * x), add = TRUE)
# curve(exp(0.2 * x), add = TRUE)
# curve(exp(-0.1 * x), add = TRUE)
# curve(exp(-0.2 * x), add = TRUE)
# df <- c(2, 8, 32, 64)
# cols <- c("red","green","blue","purple")
# for(i in seq_along(df)) {
#   abline(v = (qchisq(p = 0.1, df = df[i]) - df[i]) / sqrt(2 * df[i]), col = cols[i])
#   abline(v = (qchisq(p = 0.9, df = df[i]) - df[i]) / sqrt(2 * df[i]), col = cols[i])
# }
  
source("New simulations/t-test-and-simulation-functions.R")

one_dim_dgm <- function(n = 25, B = c(0, 0), whichX = c(F, T), 
                        x_skew = 0.1, z = 0, e_dist = "norm", ...) {
  
  v <- 8 / x_skew^2
  x <- (rchisq(n, df = v) - v ) / sqrt(2 * v)
  
  e <- switch(e_dist,
              norm = rnorm(n, 0, 1),
              chisq = (rchisq(n, 5) - 5) / sqrt(10),
              t5 = rt(n, 5) * sqrt(3 / 5)
              )
  
  sigma <- exp(z * x)
  
  Y <- B[1] + B[2] * x + sigma * e
  
  fitted_model <- estimate_model(Y = Y, X = cbind(x0 = 1, x1 = x), 
                                 trueB = B, whichX = whichX)
  
  fitted_model$sigma <- sigma
  
  fitted_model
}

# HC <- "HC2"
# tests <- c("Satt_E","Satt_H","saddle_E","saddle_H","saddle_S","saddle_T")
# alphas <- c(.005, .01, .05, .10)
# z <- -0.1
# 
# model <- one_dim_dgm(n = 100, x_skew = 3, z = z)
# dat <- with(model, data.frame(Y, X))
# dat <- dat[order(dat$x1),]
# lm_fit <- lm(Y ~ x1, data = dat)
# dat$e <- residuals(lm_fit)
# dat$y_hat <- fitted.values(lm_fit)
# dat$sigma <- (1 - 4 * z)^z * exp(2 * z * dat$x1)
# dat$e_sq_S <- predict(loess(e^2 ~ y_hat, data = dat, span = 0.25, statistics = "none"))
# plot(density(dat$x1))
# plot(Y ~ x1, data = dat)
# 
# spans <- seq(0.1, 0.5, 0.1)
# names(spans) <- c("red","orange","yellow","green","blue")
# plot(e^2 ~ x1, data = dat)
# lines(sigma^2 ~ x1, data = dat, col = "green")
# for (i in seq_along(spans)) {
#   lines(predict(loess(e^2 ~ y_hat, data = dat, span = spans[i], statistics = "none")) ~ dat$x1,
#         col = names(spans)[i])
# }
# 
# 
# run_tests(HC, model, tests, alphas)
# test_hom(model)
# 
# HCtests <-
#   tribble(
#     ~HC, ~tests,
#     "hom", list(),
#     "HC2", list("Satt_E","Satt_H","saddle_E","saddle_H","saddle_S","saddle_T"),
#     "HC4", list("naive")
#   )
# 
# alphas <- c(.005, .010, .050, .100)
# names(alphas) <- paste0("p", alphas)
# 
# 
# system.time(
#   test_sims <- run_sim(dgm = one_dim_dgm,
#                        iterations = 100, n = 50,
#                        alphas = alphas,
#                        HCtests = HCtests,
#                        B = c(0, 0), whichX = c(F, T),
#                        x_skew = 1, z = 0.1, e_dist = "norm",
#                        span = 0.2, adjusted_alpha = TRUE)
# )
# 
# test_sims %>%
#   filter(stat=="rejection rate") %>% select(-stat)
# test_sims %>%
#   filter(stat=="adjusted alpha") %>% select(-stat)

#-----------------------------
# Simulation design - size
#-----------------------------

set.seed(20170327)

size_factors <- list(
  n = c(25, 50, 100),
  z = seq(0, 0.2, 0.02),
  x_skew = c(0.5, 1, 2),
  e_dist = c("norm", "chisq", "t5"),
  subset = 1:1
)

lengths(size_factors)
prod(lengths(size_factors))

iterations <- 10

size_design <- 
  size_factors %>%
  cross_d() %>%
  mutate(
    iterations = iterations,
    span = 0.2,
    seed = round(runif(1) * 2^30) + row_number()
  )

size_design

alphas <- c(.005, .010, .050, .100)
names(alphas) <- paste0("p", alphas)
sqrt(alphas * (1 - alphas) / iterations)
sqrt(alphas * (1 - alphas) / iterations) / alphas

size_HCtests <-
  tribble(~ HC, ~ tests,
          "HC0", list("naive", "Rp_E", "Rp_H", "RCI_E", "RCI_H"),
          "HC1", list("naive"),
          "HC2", list("naive","Satt_E", "Satt_H",
                      "saddle_E", "saddle_H", "saddle_S", "saddle_T",
                      "KCp_E", "KCp_H", "KCCI_E", "KCCI_H"),
          "HC3", list("naive"),
          "HC4", list("naive"),
          "HC4m", list("naive"),
          "HC5", list("naive"),
          "hom", list()
          )


#-----------------------------
# Run size simulations
#-----------------------------


source_obj <- ls()

library(Pusto)
library(multidplyr)
cluster <- start_parallel(source_obj = source_obj, packages = "purrr")

system.time(
  size_results <- 
    size_design %>%
    partition(cluster = cluster) %>%
    do(invoke_rows(.d = ., .f = run_sim, dgm = one_dim_dgm, 
                   alphas = alphas, HCtests = size_HCtests, 
                   .to = "res")) %>%
    collect() %>% ungroup() %>%
    select(-PARTITION_ID) %>%
    arrange(seed)
)

session_info <- sessionInfo()

save(size_design, size_HCtests, alphas, size_results, session_info, 
     file = "New simulations/one-dim-sim-size-results.Rdata")


#-----------------------------
# Simulation design - power
#-----------------------------

iterations <- 50

focal_design <- 
  size_design %>%
  select(-iterations) %>%
  filter(z %in% c(0, 0.1, 0.2)) %>%
  mutate(iterations = iterations)

focal_design

power_factors <- list(
  beta = seq(-2, 2, 0.4),
  subset = 1:1
)

c(nrow(focal_design), lengths(power_factors))
prod(c(nrow(focal_design), lengths(power_factors)))

power_design <- 
  focal_design %>%
  select(-seed) %>%
  full_join(cross_d(power_factors)) %>%
  mutate(
    seed = round(runif(1) * 2^30) + row_number()
  )

power_design
nrow(size_design)
nrow(focal_design)
nrow(power_design)

focal_HCtests <-
  tribble(~ HC, ~ tests,
          "HC2", list("Satt_H", "saddle_E","saddle_H", "KCCI_H"),
          "HC4", list("naive")
  )

#---------------------------------
# size adjustment calculations
#---------------------------------

source_obj <- ls()

library(Pusto)
library(multidplyr)
cluster <- start_parallel(source_obj = source_obj, packages = "purrr")

system.time(
  focal_size_results <- 
    focal_design %>%
    partition(cluster = cluster) %>%
    do(invoke_rows(.d = ., .f = run_sim, dgm = one_dim_dgm, 
                   alphas = alphas, HCtests = focal_HCtests,
                   adjusted_alpha = TRUE,
                   .to = "res")) %>%
    collect() %>% ungroup() %>%
    select(-PARTITION_ID) %>%
    arrange(seed)
)


alphas_nominal <- alphas
names(alphas_nominal) <- paste0("n", alphas)
  
adjusted_alphas <- 
  focal_size_results %>%
  unnest() %>%
  filter(stat == "adjusted alpha") %>%
  select(n, z, x_skew, e_dist, span, HC, coef, test, starts_with("p0")) %>%
  rowwise() %>%
  nest(starts_with("p0"), .key = "alphas") %>%
  rowwise() %>%
  mutate(alphas = list(c(unlist(alphas), alphas_nominal))) %>%
  ungroup() %>%
  group_by(n, z, x_skew, e_dist, span) %>%
  nest(HC:alphas, .key = "alphas")

save(adjusted_alphas, file = "New simulations/one-dim-sim-adjusted_alphas.Rdata")

#-----------------------------
# Run power simulations
#-----------------------------

power_design_full  <- 
  power_design %>%
  inner_join(adjusted_alphas)
power_design_full

source_obj <- ls()

library(Pusto)
library(multidplyr)
cluster <- start_parallel(source_obj = source_obj, packages = "purrr")

system.time(
  power_results <- 
    power_design_full %>%
    partition(cluster = cluster) %>%
    do(invoke_rows(.d = ., .f = run_sim, dgm = one_dim_dgm, 
                   HCtests = focal_HCtests,
                   .to = "res")) %>%
    collect() %>% ungroup() %>%
    select(-PARTITION_ID) %>%
    arrange(seed)
)

session_info <- sessionInfo()

save(power_design, focal_HCtests, alphas, adjusted_alphas, power_results, session_info,
     file = "New simulations/one-dim-sim-power-results.Rdata")
