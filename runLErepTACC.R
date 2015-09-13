library(plyr)


#-----------------------------
# Data-generating model
#-----------------------------

estimate_model <- function(Y, X, trueB, whichX) {
  
  X <- X[, whichX]
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

gdm <- function(n = 25, B = c(1, 1, 1, 1, 0, 0), Estruct = "E0", whichX = c(T, T ,T ,T ,T ,F), Edist = "En") {
  
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
  X <- cbind(x0, x1, x2, x3, x4, xD)
  
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
  
  values <- estimate_model(Y, X, B, whichX)
  
  return(values)
}


#-----------------------------------
# simulation driver
#-----------------------------------

runSim <- function(iterations, n, B, whichX, Estruct, Edist, HC, tests, seed = NULL) {
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
                 Estruct = Estruct,
                 whichX = whichX,
                 Edist = Edist)
    
    ldply(HC, estimate, tests = tests, model = model)
  })
  
  # performance calculations
  
  if ("saddle" %in% tests) tests <- c(tests[tests != "saddle"], paste0("saddle_V",1:2))
  
  reps <- melt(reps, id.vars = c("HC","coef","criterion"), measure.vars = tests, variable.name = "test")
  ddply(reps, .(HC,coef,criterion,test), summarize, 
        p01 = mean(ifelse(is.na(value), F, value < .01)),
        p05 = mean(ifelse(is.na(value), F, value < .05)),
        p10 = mean(ifelse(is.na(value), F, value < .10)),
        percentNA = mean(is.na(value)))
}

#-----------------------------
# Run Long & Ervin Simulation
#-----------------------------




set.seed(20150911)

design <- list(n = c(25, 50, 100, 250, 500),
               B = "1 1 1 1 0 0",
               whichX = "T T T T T F",
               Estruct = c("E0", "E1", "E2", "E3", "E4", "E5", "E6"),
               Edist = c("En", "Ech", "Et"),
               HC = "HC0 HC1 HC2 HC3 HC4 HC4m HC5",
               tests = "naive Satt saddle edgeKC")

design2 <- list(n = c(25, 50, 100, 250, 500),
               B = "1 0 1 1 0 1",
               whichX = "T F T T T T",
               Estruct = c("E0", "E1", "E2", "E3", "E4", "E5", "E6"),
               Edist = c("En", "Ech", "Et"),
               HC = "HC0 HC1 HC2 HC3 HC4 HC4m HC5",
               tests = "naive Satt saddle edgeKC")

params <- rbind(expand.grid(design, stringsAsFactors = F), 
                expand.grid(design2, stringsAsFactors = F))

<<<<<<< HEAD
params$iterations <- 10000
=======
params$iterations <- 100
>>>>>>> 5ca3dd43e9be89addf413efb29426f34754ff1c6
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

clusterEvalQ(cluster, source("SSTP.R"))
clusterEvalQ(cluster, library(plyr))
clusterEvalQ(cluster, library(reshape2))
#clusterEvalQ(cluster, library(compiler))
#clusterEvalQ(cluster, enableJIT(3))

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

<<<<<<< HEAD
write.csv(results, file = "Results/20150911.csv")
=======
write.csv(results, file = "2015098.csv")
>>>>>>> 5ca3dd43e9be89addf413efb29426f34754ff1c6


