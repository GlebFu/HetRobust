library(purrr)
library(dplyr)
library(multidplyr)

# some function to be run in parallel

f <- function(x, y, iterations = 2, ...) {
  tibble(z = paste0(rep(x, times = y), collapse = ""), iterations = iterations)
}

f("A", 4)


cluster <- create_cluster(parallel::detectCores() - 1)
set_default_cluster(cluster)
cluster_library(cluster, "purrr")
cluster_library(cluster, "tibble")
cluster_assign_value(cluster, "f", f)


list(x = LETTERS[1:5], y = 1:4) %>%
  cross_d() %>%
  mutate(
    iterations = 20,
    seed = round(runif(1) * 2^30) + row_number()
  ) %>%
  group_by(x, y) %>%
  partition(cluster = cluster) %>%
  do(invoke(f, .x = .)) %>%
  collect()






library(multidplyr)

# cluster <- start_parallel(source_obj = source_obj, libraries = c("tidyr","dplyr","purrr"))
# set_default_cluster(cluster)

source_obj <- ls()

cluster <- create_cluster(parallel::detectCores() - 1)
set_default_cluster(cluster)
cluster_library(cluster, "tibble")
cluster_library(cluster, "tidyr")
cluster_library(cluster, "dplyr")
cluster_library(cluster, "purrr")
parallel::clusterExport(cluster, source_obj)

results <-
  size_design %>%
  group_by(subset, n, x_skew, z, e_dist) %>%
  partition(cluster = cluster) %>%
  do(invoke(run_sim, .x = ., dgm = one_dim_dgm, alphas = alphas, HCtests = HCtests)) %>%
  collect()
# 
# stopCluster(cluster)