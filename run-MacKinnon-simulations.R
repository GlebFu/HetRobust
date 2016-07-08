library(Pusto)
rm(list=ls())
source("t-test-and-simulation-functions.R")

MacKinnon_dgm <- function(n = 25, B = c(1, 1, 1, 1, 0), whichX = c(F, F, F, F, T), g = 0, zg = 1) {

  trueB <- B
  p <- length(B) - 1
  # Distributions used in generating data
  X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))
  colnames(X) <- paste("x", 0:p, sep = "")
  eta <- rnorm(n)
  
  XB <- as.vector(X %*% B)
  sigma <- zg * XB^g
  
  # Generate DV
  Y <- XB + eta * sigma
  
  estimate_model(Y, X, trueB, whichX)
}

# model <- MacKinnon_dgm()
# HC <- "HC2"
# alphas <- c(.005, .01, .05, .10)
# estimate(HC, model, alphas)
# 
# system.time(test_sims <- runSim(dgm = MacKinnon_dgm, 
#                      iterations = 10, n = 500, 
#                      B = "1 1 1 1 0", whichX = "T T T T T", 
#                      HC = "HC0 HC2 HC3", 
#                      alpha_string = ".005 .01 .05 .10", 
#                      g = 0, zg = 1))
# test_sims

#-----------------------------
# Run MacKinnon Simulation
#-----------------------------
set.seed(20160708)

load("scale.rdata")
design <- list(n = seq(20,100,20),
               B = "1 1 1 1 0",
               whichX = "F F F F T",
               g = seq(0, 2, 0.2),
               HC = "HC0 HC1 HC2 HC3 HC4 HC4m HC5",
               alpha_string = ".005 .010 .050 .100")

params <- expand.grid(design, stringsAsFactors = FALSE)

params <- merge(params, scale)

params$iterations <- 1000
params$seed <- round(runif(1) * 2^30) + 1:nrow(params)

source_obj <- ls()
cluster <- start_parallel(source_obj = source_obj)

system.time(results <- plyr::mdply(params, .fun = runSim, 
                                   dgm = MacKinnon_dgm,
                                   .parallel = TRUE))

stopCluster(cluster)

write.csv(results, file = "Results/MacKinnon/20160708.csv")

#-------------------------------
# Large-n simulations
#-------------------------------

set.seed(20160707)

load("scale.rdata")
design <- list(n = 1000,
               B = "1 1 1 1 0",
               whichX = "T T T T T",
               g = seq(0, 1, 0.5),
               HC = "HC0 HC2 HC3",
               alpha_string = ".005 .010 .050 .100",
               rep = 1:7)

params <- expand.grid(design, stringsAsFactors = FALSE)

params <- merge(params, scale)

params$iterations <- 1429
params$seed <- round(runif(1) * 2^30) + 1:nrow(params)

source_obj <- ls()
cluster <- start_parallel(source_obj = source_obj)

system.time(results <- plyr::mdply(params, .fun = runSim, 
                                   dgm = MacKinnon_dgm,
                                   .parallel = TRUE))

stopCluster(cluster)

save(results, file = "MacKinnon n = 1000 results.Rdata")
