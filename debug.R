rm(list = ls())

load("Problem Models.Rdata")
badmods <- badmods[1]
save(badmods, file = "Problem Models.Rdata")

rm(list = ls())

source("hetRobust debug.R")

library(plyr)
library(Pusto)

results <- runSim(iterations = 100,
                  n = 50,
                  B = "1 1 1 1 0 0",
                  whichX = "T T T T T F",
                  Estruct = "E0",
                  Edist = "En",
                  HC = "HC5",
                  test = "saddle",
                  seed = 899087467)

load("Problem Models.Rdata")

debug(estimate)
debug(saddle)
debug(saddlepoint_pval)

estimate(HC = "HC5", tests = "saddle", model = badmods[[2]])


undebug(estimate)
undebug(saddle)
undebug(saddlepoint_pval)


load("Replication Results.Rdata")

write.csv(reps, file = "Replication Results.csv")
write.csv(reps, file = "Replication Results2.csv")

write.csv(results, file = "Results/20150728 debug.csv")