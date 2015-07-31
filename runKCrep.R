source("edgeKCrep.R")

library(plyr)
library(Pusto)

set.seed(20150731)

design <- list(n = c(25, 40),
               mdl = 1:3,
               xDist = c("unif", "norm", "lap"),
               HC = "HC2",
               tests = "naive edgeKC")

params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 2000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/edgeKC 20150731.csv")

library(ggplot2)
library(dplyr)


covProb <- filter(results, criterion == "size", coef == "x1") %>%
              select(n, mdl, xDist, test, p05)

covProb$p05 <- 1 - covProb$p05

ggplot(covProb, aes(x = mdl,
                    y = p05,
                    color = test)) +
  geom_line() +
  facet_wrap(~n * xDist)
