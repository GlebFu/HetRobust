#-----------------------------
# NA producing Models
#-----------------------------

rm(list = ls())

source("hetRobust.R")
load("Problem Models.Rdata")

debug(saddlepoint_pval)

estimate(HC = "HC5", tests = "saddle", model = badmods[[1]])
estimate(HC = "HC1", tests = "saddle", model = badmods[[2]])
estimate(HC = "HC0", tests = "saddle", model = badmods[[3]])
estimate(HC = "HC0", tests = "saddle", model = badmods[[4]])

undebug(saddlepoint_pval)

#-----------------------------
# pValues
#-----------------------------
rm(list = ls())
source("hetRobust.R")

testmod <- gdm(n = 25, 
               B = c(1, 1, 1, 1, 0, 0), 
               Estruct = "E0", 
               whichX = c(T, T ,T ,T ,T ,F), 
               Edist = "En")

estimate(HC = "HC0",
         tests = c("naive", "Satt", "saddle", "edgeKC"),
         model = testmod)

debug(estimate)
