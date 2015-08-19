library(plyr)
library(Pusto)
rm(list=ls())
source("SSTP.R")

#-----------------------------
# Data-generating model
#-----------------------------

estimate_model <- function(Y, X, B) {
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

gdm <- function(n = 20, mdl = 1, xDist = "norm", B = c(0,1)) {
  require(Runuran)
  
  x1 <- switch(xDist,
               unif = runif(n, 0, 1),
               norm = rnorm(n),
               lap = urlaplace(n))
  
  X <- cbind(x0 = 1, x1)
  
  e <- switch(mdl,
              "1" = rnorm(n, 0, .2),
              "2" = rnorm(n, 0, .2 + exp(x1/2)/2),
              "3" = rnorm(n, 0, sqrt(.1 + x1^2)))
  
  Y = as.vector(X %*% B) + e
  
  values <- estimate_model(Y, X, B)
  
  return(values)
}

#-----------------------------------
# simulation driver
#-----------------------------------

runSim <- function(iterations, n, mdl, xDist, HC, tests, seed = NULL) {
  require(plyr)
  require(reshape2)
  
  HC <- as.character(unlist(strsplit(HC, " ")))
  tests <- as.character(unlist(strsplit(tests, " ")))
  
  if (!is.null(seed)) set.seed(seed)
  
  reps <- rdply(iterations, {
    model <- gdm(n = n, 
                 mdl = mdl,
                 xDist = xDist)
    
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
# Run Kauermann & Carroll Simulation
#-----------------------------

set.seed(20150812)

design <- list(n = c(25, 40),
               mdl = 1:3,
               xDist = c("unif", "norm", "lap"),
               HC = "HC2 HC3",
               tests = "naive Satt edgeKC saddle")

params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 10000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

save(results, file = "Results/edgeKC simulations.Rdata")
load("Results/edgeKC simulations.Rdata")

library(ggplot2)
library(dplyr)
library(tidyr)
summary(results$percentNA)
filter(results, percentNA > 0)

filter(results, criterion == "size") %>%
  select(n, mdl, xDist, test, HC, coef, p01:p10) %>%
  gather("alpha","reject_rate",p01:p10) %>%
  mutate(test_HC = paste(test, HC),
         coverage = 1 - reject_rate,
         mdl = factor(mdl),
         xDist = factor(xDist, levels = c("unif","norm","lap"))) ->
  covProb

# recreate KC Figure 1  

filter(covProb, coef == "x1", alpha=="p05", 
       test_HC %in% c("naive HC2","naive HC3", "edgeKC HC2")) %>%
ggplot(aes(x = mdl, y = coverage, shape = test, color = test)) +
  geom_point() +
  geom_hline(yintercept = .95, linetype = "dashed") +
  facet_wrap(~n * xDist) +
  theme_minimal() +
  scale_shape(solid = FALSE)
  

# compare all the tests

alphas <- data.frame(alpha = unique(covProb$alpha), nominal = c(.99,.95,.90))

ggplot(covProb, aes(test_HC, coverage, fill = test_HC)) + 
  geom_boxplot() + 
  geom_hline(data = alphas, aes(yintercept = nominal), linetype="dashed") + 
  facet_grid(alpha ~ n, scales = "free_y") + 
  theme_bw() + theme(legend.position = "bottom")