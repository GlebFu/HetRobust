library(htmlTable)
library(PerformanceAnalytics)
library(reshape2)
library(plyr)
library(Pusto)
library(dplyr)
library(ggplot2)
rm(list = ls())

# Returns a vector of response errors to be added to a vector of responses
### erlist = mnemonic of error structure (character)
### X = matrix of covariates
### Edist = matrix of error distributions
error <- function(erlist, X, Edist) {
  x1 <- X[,2]
  x2 <- X[,3]
  x3 <- X[,4]
  x4 <- X[,5]
  xD <- X[,6]
  
  switch(erlist,
         E0 = Edist,
         E1 = sqrt(x1) * Edist,
         E2 = sqrt(x3 + 1.6) * Edist,
         E3 = sqrt(x3) * sqrt(x4 + 2.5) * Edist,
         E4 = sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * Edist,
         E5 = ifelse(xD == 1, 1.5 * Edist, Edist),
         E6 = ifelse(xD == 1, 4 * Edist, Edist),
  )
}

# Calculates a list of values for use in the estimation function
### Y = vector of response values
### X = matrix of covariates
### trueB = vector of known coeficients
### whichX = specifies which X strings to use in estimation
model <- function(Y, X, trueB, whichX) {
  Y <- Y
  X <- X[, whichX]
  B <- trueB[whichX]
  
  n <- nrow(X)
  p <- ncol(X)
  
  M <- solve(t(X) %*% X)
  invX <- M %*% t(X)
  coefs <- invX %*% Y
  e <- Y - X %*% coefs
  
  H <- X %*% invX
  h <- diag(H)
  I <- diag(1, n)
  W <- 1 / sqrt(1 - h)
  w <- 1 - h
  
  vars <- list(X = X, Y = Y, B = B, invX = invX, H = H, h = h, I = I, W = W, w = w, 
               e = e, coefs = coefs, n = n, p = p, M = M)
  
  return(vars)
}

gdm <- function(n, B, Estructs, whichX, Edist) {
  
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
  
  # One dummy variable created by splitting x2
  xD <- ifelse(x2 > 1.6, 1, 0)
  
  # X matrix
  X <- cbind(x0, x1, x2, x3, x4, xD)
  
  # X matrix with standardized x's
  Xsd <- scale(X)
  Xsd[,1] <- x0
  
  # Three types of homoscedastistic error distributions:
  Edist <- switch(Edist,
                  En = rnorm(n, 0, 1),
                  Ech = rchisq(n, 5),
                  Et = rt(n, 5))
  
  # Generate DV with no error
  Y <- t(B %*% t(X))
  
  # Generate error matrix
  Es <- lapply(Estructs, error, X, Edist)
  
  # Add error to DV
  Y <- lapply(Es, function(Ers) Y + Ers)
  
  values <- lapply(Y, model, X, B, whichX)
  
  names(values) <- Estructs
  
  return(values)
}

###  ESTIMATE FUNCTION IN PROGRESS ### 

# cidf calculates the t-statistic, lower bound, and upper bound and returns dataframe
cidf <- function(adjust) {
  t <- qt(.975, adjust$df)
  
  ci <- t(t(cbind(lb = adjust$Coef, ub = adjust$Coef)) + matrix(c(-1,1)) %*% (t*adjust$sd_e))
  
  captured <- adjust$B > ci[,1] & adjust$B < ci[,2]
  
  DNRNull <- 0 > ci[,1] & 0 < ci[,2]

  cidf = data.frame(t = t, lb = ci[,1] , ub = ci[,2], captured, DNRNull)
    
  return(cidf)
}

# estimation function takes model and vector of adjustments
# need to design function to calculate appropriate adjustments given vector of adjustments
# adjustment function will return a dataframe of adjustment name, parameter, estimated coefficient, standard error, degrees of freedom
estimate <- function(model, adjust) {
  M <- model$M
  X <- model$X
  e <- model$e
  h <- model$h
  w <- model$w
  n <- model$n
  p <- model$p
  coefs <- model$coefs
  B <- model$B
  
  HC0 <- data.frame(Adjustment = rep("HC0", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2)) %*% X %*% M)), 
                    df = rep(n-p,p),
                    B = B)
  
  HC1 <- data.frame(Adjustment = rep("HC1", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt((n/(n-p))*diag(M %*% t(X) %*% diag(as.vector(e^2)) %*% X %*% M)), 
                    df = rep(n-p,p),
                    B = B)
  
  HC2 <- data.frame(Adjustment = rep("HC2", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w)) %*% X %*% M)), 
                    df = rep(n-p,p),
                    B = B)
  
  HC3 <- data.frame(Adjustment = rep("HC3", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w^2)) %*% X %*% M)), 
                    df = rep(n-p,p),
                    B = B)
  
  HC4 <- data.frame(Adjustment = rep("HC4", p),
                    Beta = paste(rep("B", p),0:(p-1), sep = ""),
                    Coef = coefs,
                    sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w^min(n*h/p,4))) %*% X %*% M)), 
                    df = rep(n-p,p),
                    B = B)
  
  HC4m <- data.frame(Adjustment = rep("HC4m", p),
                     Beta = paste(rep("B", p),0:(p-1), sep = ""),
                     Coef = coefs,
                     sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w^(min(n*h/p,1) + min(n*h/p,1.5)))) %*% X %*% M)), 
                     df = rep(n-p,p),
                     B = B)
  
  HC5<- data.frame(Adjustment = rep("HC5", p),
                   Beta = paste(rep("B", p),0:(p-1), sep = ""),
                   Coef = coefs,
                   sd_e = sqrt(diag(M %*% t(X) %*% diag(as.vector(e^2/w^(min(n*h/p,max(4,.7*n*max(h)/p))))) %*% X %*% M)), 
                   df = rep(n-p,p),
                   B = B)
  
  
  sdf <- rbind(HC0, HC1, HC2, HC3, HC4, HC4m, HC5)
  
  cidf <- cidf(sdf)
  return(cbind(sdf, cidf))
}


performance <- function(results) {
  cbind(M = apply(results, 1, mean, na.rm = T), Var = apply(results, 1, var, na.rm = T))
}


runSim <- function(step, iterations, n, B, Estructs, whichX, Edist, seed = NULL) {
  require(reshape2)
  
  # print(paste(Sys.time(), "Step", step, "- Begin"))
  
  B <- as.numeric(unlist(strsplit(B, " ")))
  Estructs <- unlist(strsplit(Estructs, " ")) 
  whichX <- as.logical(unlist(strsplit(whichX, " ")))
  
  if (!is.null(seed)) set.seed(seed)
  
  factors <- NULL
  first <- T
  
  reps <- replicate(iterations, {
                    model <- gdm(n = n, 
                                 B = B, 
                                 Estructs = Estructs,
                                 whichX = whichX,
                                 Edist = Edist)
                    
                    estimates <- ldply(model, estimate)
                    estimates <- melt(estimates, id.vars = c(".id", "Adjustment", "Beta"))
                    
                    if(!first) return (estimates$value)
                    
                    env <- parent.env(environment())
                    env$factors <- estimates[,c(".id", "Adjustment", "Beta", "variable")]
                    env$first <- F
    
                    return(estimates$value)
  })
  
  results <- performance(reps)
  results <- cbind(factors, results)
  
  # print(paste(Sys.time(), " -Step ", step, " - End - ", round(step/18,2)*100, "% Complete", sep = ""))
  
  return(results)
}

set.seed(20150616)

design <- list(n = c(25, 50, 100, 250, 500, 1000),
               B = "1 1 1 1 0 0",
               Estructs = "E0 E1 E2 E3 E4 E5 E6",
               whichX = "T T T T T F",
               Edist = c("En", "Ech", "Et"))

params <- expand.grid(design, stringsAsFactors = F)

params <- cbind(step = 1:nrow(params), params)

params$iterations <- 1000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/20150616.csv")

#results <- read.csv("Results/20150615.csv")
#resultsW <- reshape(results, timevar = "variable", direction = "wide", v.name = c("M", "Var"), idvar = "IDVAR")
#write.csv(resultsW, file = "Results/20150615W.csv")

# Figure 1 Size
B3results <- filter(results, Beta == "B3", Edist == "Ech", variable == "captured") %>%
  select(n, Adjustment, M, .id)
B3results$M <- 1 - B3results$M
B3results$n <- factor(B3results$n)
ggplot(B3results, aes(x = n,
                      y = M,
                      group = Adjustment,
                      color = Adjustment)) +
  geom_line() +
  facet_wrap(~.id)

# Figure 1 Power
B3results <- filter(results, Beta == "B3", Edist == "Ech", variable == "DNRNull", .id == "E0") %>%
                select(n, Adjustment, M, .id)
B3results$M <- 1 - B3results$M
B3results$n <- factor(B3results$n)
ggplot(B3results, aes(x = n,
                      y = M,
                      group = Adjustment,
                      color = Adjustment)) +
  geom_line()

# Figure 2
B3results <- filter(results, Edist == "Et", variable == "captured", .id == "E2", Beta != "B0") %>%
  select(n, Adjustment, M, .id, Beta)
B3results$M <- 1 - B3results$M
B3results$n <- factor(B3results$n)
ggplot(B3results, aes(x = n,
                      y = M,
                      group = Adjustment,
                      color = Adjustment)) +
  geom_line() +
  facet_wrap(~Beta)

# Figure 3
B3results <- filter(results, Edist == "Et", variable == "rejectNull", .id == "E3", Beta == "B1" | Beta == "B3") %>%
  select(n, Adjustment, M, .id, Beta)
B3results$M <- 1 - B3results$M
B3results$n <- factor(B3results$n)
ggplot(B3results, aes(x = n,
                      y = M,
                      group = Adjustment,
                      color = Adjustment)) +
  geom_line() +
  facet_wrap(~Beta)
