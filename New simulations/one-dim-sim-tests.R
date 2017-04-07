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

subsets <- 4

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
cluster <- start_parallel(source_obj = source_obj, packages = "purrr", register = TRUE)

# packages <- "purrr"
# cluster <- snow::getMPIcluster()
# snow::clusterExport(cluster, source_obj)
# library_calls <- lapply(packages, function(lib) call("library", lib))
# snow::clusterExport(cluster, "library_calls", envir = environment())
# snow::clusterEvalQ(cluster, lapply(library_calls, eval))

library(multidplyr)

size_results <-
  size_design %>%
  partition(cluster = cluster) %>%
  do(invoke_rows(.d = ., .f = run_sim, dgm = one_dim_dgm,
                 alphas = alphas, HCtests = size_HCtests,
                 .to = "res")) %>%
  collect() %>% ungroup() %>%
  select(-PARTITION_ID) %>%
  arrange(seed)

(session_info <- sessionInfo())
(run_date <- date())

# stopCluster(cluster)
rm(cluster, size_design, size_results)

