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

library(ggplot2)
library(dplyr)


covProb <- filter(results, criterion == "size", coef == "x1") %>%
              select(n, mdl, xDist, test, p05, HC)

covProb$test <- paste(covProb$test, covProb$HC)

covProb <- filter(covProb, test != "edgeKC HC3")
covProb$test <- factor(covProb$test)
covProb$test <- factor(covProb$test, levels(covProb$test)[c(3,1,2)], ordered = T)

covProb$p05 <- 1 - covProb$p05

covProb$xDist <- factor(covProb$xDist, levels = c("unif", "norm", "lap"))

ggplot(covProb, aes(x = mdl,
                    y = p05)) +
  geom_point(aes(shape = test)) +
  facet_wrap(~n * xDist) +
  theme_minimal() +
  scale_shape(solid = FALSE)
  
