#-----------------------------
# Data-generating model
#-----------------------------

Long_dgm <- function(n = 25, B = c(1, 1, 1, 1, 0, 0), Estruct = "E0", whichX = c(T, T ,T ,T ,T ,F), Edist = "En") {
  
  # Distributions used in generating data
  b1 <- runif(n, 0, 1)
  b2 <- rnorm(n, 0, 1)
  b3 <- rchisq(n, 1)
  b4 <- rnorm(n, 0, 1)
  b5 <- runif(n, 0, 1)
  
  # Four independant variables based on distributions
  x0 <- 1
  x1 <- 1 + b1
  x2 <- 3 * b1 + .6 * b2
  x3 <- 2 * b1 + .6 * b3
  x4 <- .1 * x1 + .9 * x3 - .8 * b4 + 4 * b5
  x2[x2 < -2.5] <- -2.5
  x4[x4 < -2.5] <- -2.5
  
  # One dummy variable created by splitting x2
  xD <- ifelse(x2 > 1.6, 1, 0)
  
  # X matrix
  X <- cbind(x0, x1, x2, x3, x4)
  
  # Three types of homoscedastistic error distributions:
  Edist <- switch(Edist,
                  En = rnorm(n, 0, 1),
                  Ech = (rchisq(n, 5) - 5) / sqrt(10),
                  Et = rt(n, 5))
  
  # Seven types of error structures
  error <- switch(Estruct,
                  E0 = Edist,
                  E1 = sqrt(x1) * Edist,
                  E2 = sqrt(x3 + 1.6) * Edist,
                  E3 = sqrt(x3) * sqrt(x4 + 2.5) * Edist,
                  E4 = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Edist,
                  E5 = ifelse(xD == 1, 1.5 * Edist, Edist),
                  E6 = ifelse(xD == 1, 4 * Edist, Edist)
  )
  
  # Generate DV
  Y <- as.vector(X %*% B) + error
  
  estimate_model(Y, X, B, whichX)

}

#-----------------------------
# Run Long & Ervin Simulation
#-----------------------------

set.seed(20160808)

design <- list(n = c(25, 50, 100, 250, 500),
               B = "1 1 1 1 0 ",
               whichX = "T T T T T ",
               Estruct = c("E0", "E1", "E2", "E3", "E4", "E5", "E6"),
               Edist = c("En", "Ech", "Et"),
               HC = "HC0 HC1 HC2 HC3 HC4 HC4m HC5",
               alpha_string = ".010 .050 .100",
               power = T)



params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 20000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()

library(Rmpi)
library(snow)
library(foreach)
library(iterators)
library(doSNOW)
library(plyr)

cluster <- getMPIcluster()
registerDoSNOW(cluster)

clusterExport(cluster, source_obj)

clusterEvalQ(cluster, source("t-test-and-simulation-functions.R"))
clusterEvalQ(cluster, library(plyr))
clusterEvalQ(cluster, library(reshape2))


system.time(results <- plyr::mdply(params, .fun = runSim, 
                                   dgm = Long_dgm,
                                   .parallel = T))

stopCluster(cluster)

write.csv(results, file = "20160808.csv")


