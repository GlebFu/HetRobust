rm(list = ls())
source("hetRobust.R")

testmod <- lapply(c(25, 50, 100, 250, 500), gdm)

lapply(testmod, estimate, HC = "HC1", tests = "edgeR")

