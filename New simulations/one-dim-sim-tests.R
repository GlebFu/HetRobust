#---------------------------------------------------------
# get command-line arguments for number of replications
#---------------------------------------------------------

args <- commandArgs(TRUE)
print(args)

if (length(args) == 0) {
  print("No arguments supplied.")
  size_iterations = 50
  power_iterations = 20
} else {
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}
print(size_iterations)
print(power_iterations)

#---------------------------------------------------------
# setup
#---------------------------------------------------------

library(tibble)
library(tidyr)
library(purrr)
library(dplyr)

source("New simulations/t-test-and-simulation-functions.R")

#---------------------------------------------------------
# data-generating model
#---------------------------------------------------------

one_dim_dgm <- function(n = 25, beta = 0, whichX = c(F, T), 
                        x_skew = 0.1, z = 0, e_dist = "norm", ...) {
  
  v <- 8 / x_skew^2
  x <- (rchisq(n, df = v) - v ) / sqrt(2 * v)
  
  e <- switch(e_dist,
              norm = rnorm(n, 0, 1),
              chisq = (rchisq(n, 5) - 5) / sqrt(10),
              t5 = rt(n, 5) * sqrt(3 / 5)
  )
  
  sigma <- exp(z * x)
  
  Y <- beta * x + sigma * e
  
  fitted_model <- estimate_model(Y = Y, X = cbind(x0 = 1, x1 = x), whichX = whichX)
  
  fitted_model$sigma <- sigma
  
  fitted_model
}

#-----------------------------
# Simulation design - size
#-----------------------------

set.seed(20170404)

subsets <- 1

size_factors <- list(
  n = c(25, 50, 100),
  z = seq(0, 0.2, 0.02),
  x_skew = c(0.5, 1, 2),
  e_dist = c("norm", "chisq", "t5"),
  subset = 1:subsets
)

lengths(size_factors)
prod(lengths(size_factors))


size_design <- 
  size_factors %>%
  cross_d() %>%
  mutate(
    iterations = size_iterations / subsets,
    span = 0.2,
    seed = round(runif(1) * 2^30) + row_number()
  )

size_design

alphas <- c(.005, .010, .050, .100)
names(alphas) <- paste0("p", alphas)
sqrt(alphas * (1 - alphas) / size_iterations)
sqrt(alphas * (1 - alphas) / size_iterations) / alphas

size_HCtests <-
  tribble(~ HC, ~ tests,
          "HC0", list("naive", "Rp_E", "Rp_H", "RCI_E", "RCI_H"),
          "HC1", list("naive"),
          "HC2", list("naive","Satt_E", "Satt_H", "Satt_S", "Satt_T",
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
# library(parallel)
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
run_date <- date()

# save(size_design, size_HCtests, alphas, size_results, session_info, run_date, 
#      file = "New simulations/one-dim-sim-size-results.Rdata")


#-----------------------------
# Simulation design - power
#-----------------------------

focal_design <-
  size_design %>%
  filter(subset == 1, z %in% c(0, 0.1, 0.2)) %>%
  select(-iterations) %>%
  mutate(iterations = size_iterations)

focal_design

power_factors <- list(
  beta = seq(-3, 3, 0.2),
  subset = 1
)

c(nrow(focal_design), lengths(power_factors))
prod(c(nrow(focal_design), lengths(power_factors)))

power_design <-
  focal_design %>%
  select(-seed, -iterations) %>%
  full_join(cross_d(power_factors)) %>%
  select(-subset) %>%
  mutate(
    seed = round(runif(1) * 2^30) + row_number(),
    iterations = power_iterations
  )

power_design
nrow(size_design)
nrow(focal_design)
nrow(power_design)
nrow(size_design) / 68 / 2
nrow(focal_design) / 68 / 2
nrow(power_design) / 68 / 2

focal_HCtests <-
  tribble(~ HC, ~ tests,
          "HC2", list("Satt_H", "saddle_E","saddle_H", "KCCI_H", "KCCI_E"),
          "HC4", list("naive")
  )

#---------------------------------
# size adjustment calculations
#---------------------------------

# source_obj <- ls()
# 
# library(Pusto)
# library(multidplyr)
# cluster <- start_parallel(source_obj = source_obj, packages = "purrr")

parallel::clusterExport(cluster, "focal_HCtests")

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

# save(adjusted_alphas, file = "New simulations/one-dim-sim-adjusted_alphas.Rdata")
# load("New simulations/one-dim-sim-adjusted_alphas.Rdata")

#-----------------------------
# Run power simulations
#-----------------------------

power_design_full  <-
  power_design %>%
  inner_join(adjusted_alphas)
power_design_full

# source_obj <- ls()
# 
# library(Pusto)
# library(multidplyr)
# cluster <- start_parallel(source_obj = source_obj, packages = "purrr")

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
run_date <- date()

# save(power_design, focal_HCtests, alphas, adjusted_alphas, power_results, session_info, run_date,
#      file = "New simulations/one-dim-sim-power-results.Rdata")

gc(verbose = TRUE)
