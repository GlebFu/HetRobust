library(tibble)
library(tidyr)
library(purrr)
library(dplyr)

rm(list=ls())

source("New simulations/t-test-and-simulation-functions.R")

one_dim_dgm <- function(n = 25, B = c(0, 0), whichX = c(F, T), 
                        x_skew = 0.1, z = 0, e_dist = "norm", ...) {
  
  v <- 64 / x_skew^2
  x <- (rchisq(n, df = v) - v ) / sqrt(2 * v)
  
  e <- switch(e_dist,
              norm = rnorm(n, 0, 1),
              chisq = (rchisq(n, 5) - 5) / sqrt(10),
              t5 = rt(n, 5) * sqrt(3 / 5)
              )
  
  sigma <- (1 - 4 * z)^z * exp(2 * z * x)
  
  Y <- B[1] + B[2] * x + sigma * e
  
  estimate_model(Y = Y, 
                 X = cbind(x0 = 1, x1 = x), 
                 trueB = B, 
                 whichX = whichX)
}

HC <- "HC2"
tests <- c("Satt_E","Satt_H","saddle_E","saddle_H")
alphas <- c(.005, .01, .05, .10)
model <- one_dim_dgm()
run_tests(HC, model, tests, alphas)

HCtests <- 
  tribble(
    ~HC, ~tests,
    "hom", list(),
    "HC0", list("Rp_E", "Rp_H"),
    "HC2", list("Satt_E","Satt_H","saddle_E","saddle_H")
  )

alphas <- c(.005, .010, .050, .100)
names(alphas) <- paste0("p", alphas)

system.time(
  test_sims <- run_sim(dgm = one_dim_dgm,
                       iterations = 100, n = 25,
                       alphas = alphas,
                       HCtests = HCtests,
                       B = c(0, 0), whichX = c(T, T), 
                       x_skew = 4, z = 0, e_dist = "norm")
)

test_sims

#-----------------------------
# Simulation design
#-----------------------------

set.seed(20170320)

size_factors <- list(
  n = c(25, 50, 100),
  z = seq(-0.1, 0.2, 0.025),
  x_skew = c(1, 3, 5),
  e_dist = c("norm", "chisq", "t5"),
  subset = 1:1
)

lengths(size_factors)
prod(lengths(size_factors))

size_design <- 
  size_factors %>%
  cross_d() %>%
  mutate(
    iterations = 20000,
    seed = round(runif(1) * 2^30) + row_number()
  )

size_design

alphas <- c(.005, .010, .050, .100)
names(alphas) <- paste0("p", alphas)

HCtests <-
  tribble(~ HC, ~ tests,
          "HC0", list("naive", "Rp_E", "Rp_H", "RCI_E", "RCI_H"),
          "HC2", list("Satt_E", "Satt_H", "saddle_E", "saddle_H", "KCp_E", "KCp_H", "KCCI_E", "KCCI_H"),
          "HC3", list("naive"),
          "HC4", list("naive"),
          "HC4m", list("naive"),
          "HC5", list("naive"),
          "hom", list()
          )


#-----------------------------
# Run simulation
#-----------------------------

library(Pusto)


source_obj <- ls()
cluster <- start_parallel(source_obj = source_obj)

system.time(
  results <- plyr::mdply(size_design, .fun = run_sim, 
                         dgm = one_dim_dgm, alphas = alphas,
                         HCtests = HCtests, 
                         .parallel = TRUE)
)

stopCluster(cluster)

save(size_design, HCtests, alphas, results, file = "New simulations/one-dim-sim-20170317.Rdata")




