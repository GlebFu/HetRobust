source("edgeKCrep.R")

library(plyr)
library(Pusto)

set.seed(20150812)

design <- list(n = c(25, 40),
               mdl = 1:3,
               xDist = c("unif", "norm", "lap"),
               HC = "HC2 HC3",
               tests = "naive edgeKC")

params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 2000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/edgeKC 20150812.csv")

results <- read.csv("Results/edgeKC 20150812.csv")

library(ggplot2)
library(dplyr)


filter(results, criterion == "size", coef == "x1", (test != "edgeKC" | HC != "HC3")) %>%
  select(n, mdl, xDist, test, p05, HC) %>%
  mutate(test = paste(test, HC),
         p05 = 1 - p05,
         mdl = factor(mdl),
         xDist = factor(xDist, levels = c("unif","norm","lap"))) ->
  covProb
  

ggplot(covProb, aes(x = mdl, y = p05, shape = test, color = test)) +
  geom_point() +
  facet_wrap(~n * xDist) +
  theme_minimal() +
  scale_shape(solid = FALSE)
  
