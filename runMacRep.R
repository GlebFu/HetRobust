source("SSTP.R")
library(plyr)
library(Pusto)


#-----------------------------
# Data-generating model
#-----------------------------

estimate_model <- function(Y, X, trueB, whichX) {
  
  #X <- X[, whichX]
  B <- trueB[whichX]
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- solve(t(X) %*% X)
  X_M <- X %*% M
  coefs <- colSums(Y * X_M)
  e <- Y - as.vector(X %*% coefs)
  
  H <- X_M %*% t(X)
  h <- diag(H)
  
  values <- list(X = X, Y = Y, B = B, X_M = X_M, H = H, h = h, e = e, coefs = coefs, n = n, p = p, M = M)
  
  return(values)
}

gdm <- function(n = 25, B = c(1, 1, 1, 1, 0, 0), whichX = c(T, T ,T ,T ,T ,F), g = 0, zg = 1) {
  B <- B[whichX]
  p <- sum(whichX) - 1
  # Distributions used in generating data
  X <- cbind(1, matrix(exp(rnorm(n * p)), n, p))
  colnames(X) <- paste("x", 0:p, sep = "")
  eta <- rnorm(n)
  
  sigma <- zg * (X %*% B)^g
  
  # Generate DV
  Y <- as.vector(X %*% B) + eta * sigma
  
  values <- estimate_model(as.vector(Y), X, B, whichX)
  
  return(values)
}


#-----------------------------------
# simulation driver
#-----------------------------------

runSim <- function(iterations, n, B, whichX, g, zg, HC, tests, seed = NULL) {
  require(plyr)
  require(reshape2)
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  HC <- as.character(unlist(strsplit(HC, " ")))
  tests <- as.character(unlist(strsplit(tests, " ")))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- rdply(iterations, {
    model <- gdm(n = n, 
                 B = B,
                 whichX = whichX,
                 g = g,
                 zg = zg)
    
    ldply(HC, estimate, tests = tests, model = model)
  })
  
  # performance calculations
  
  if ("saddle" %in% tests) tests <- c(tests[tests != "saddle"], paste0("saddle_V",1:2))
  
  reps <- melt(reps, id.vars = c("HC","coef","criterion"), measure.vars = tests, variable.name = "test")
  ddply(reps, .(HC,coef,criterion,test), summarize, 
        p005 = mean(ifelse(is.na(value), F, value < .005)),
        p01 = mean(ifelse(is.na(value), F, value < .01)),
        p05 = mean(ifelse(is.na(value), F, value < .05)),
        p10 = mean(ifelse(is.na(value), F, value < .10)),
        percentNA = mean(is.na(value)))
}

#-----------------------------
# Run MacKinnon Replication
#-----------------------------
set.seed(20160628)

load("scale.rdata")
design <- list(n = seq(20,100,20),
               B = "1 1 1 1 0",
               whichX = "T T T T T",
               g = seq(0,2,0.05),
               HC = "HC1 HC2 HC3 HC4",
               tests = "naive")



params <- expand.grid(design, stringsAsFactors = F)

params <- merge(params, scale)

params$iterations <- 10000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/MacKinnon/20160629Mac.csv")

#-----------------------------
# Run MacKinnon Simulation
#-----------------------------
set.seed(20160628)

load("scale.rdata")
design <- list(n = seq(20,100,20),
               B = "1 1 1 1 0",
               whichX = "T T T T T",
               g = seq(0,2,0.05),
               HC = "HC0 HC1 HC2 HC3 HC4 HC4m HC5",
               tests = "naive Satt saddle edgeKC")

design2 <- list(n = seq(20,100,20),
                B = "1 1 1 1 0",
                whichX = "T T T T T",
                g = seq(0,2,0.05),
                HC = "OLS",
                tests = "naive")

params <- rbind(expand.grid(design, stringsAsFactors = F),
                expand.grid(design2, stringsAsFactors = F))

params <- merge(params, scale)

params$iterations <- 100
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/MacKinnon/20160629.csv")
