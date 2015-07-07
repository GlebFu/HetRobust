library(htmlTable)
library(PerformanceAnalytics)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
rm(list = ls())

# Distributions used in generating data
b1 <- runif(1000000, 0, 1)
b2 <- rnorm(1000000, 0, 1)
b3 <- rchisq(1000000, 1)
b4 <- rnorm(1000000, 0, 1)
b5 <- runif(1000000, 0, 1)

# Intercept and four independant variables based on distributions
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

# X matrix with standardized x's
Xsd <- scale(X)
Xsd[,1] <- x0

charactaristics <- function(x) {
  round(cbind(min(x), max(x), mean(x), sd(x)),2)
}

# Expected charactaristics of x4 are -.28, 15.28, 3.59 and 1.69 
# for minimum, maximum, mean and variance respectively
htmlTable(t(apply(X,2,charactaristics)),
          header = c("Min", "Max", "Mean", "Var"))

# Expected cor of x4 with other covariates should be, in order:
# .32, .27, .56, .23
round(cor(X[,-1]),2)


# Three types of homoscedastistic errors:
En <- rnorm(100000, 0, 1)
Ech <- rchisq(100000, 5)
Et <- rt(100000, 5)


sqrt(8/5) # Reported skewness of chi error distribution
skewness(Ech) # Replicated skewness of chi error distribution

12/5 # Reported excess kurtosis of chi error distribution
kurtosis(Ech, method = "excess") # Replicated excess kurtosis of chi error distribution

0 # Reported skewness of t error distribution
skewness(Et) # Replicated skewness of t error distribution

6 # # # Reported excess kurtosis of t error distribution
kurtosis(Et, method = "excess") # # # Replicated excess kurtosis of t error distribution

E0 <- En
E1 <- sqrt(x1) * En
E2 <- sqrt(x3 + 1.6) * En
E3 <- sqrt(x3) * sqrt(x4 + 2.5) * En #  x4 can be less than -2.5 per my simulation
E4 <- sqrt(x1) * sqrt(x2 + 2.5) * sqrt(x3) * En
E5 <- ifelse(xD == 1, 1.5 * En, En)
E6 <- ifelse(xD == 1, 4 * En, En)

source(file = "Long Ervin Replication.R")

# Demo
model(Y = runif(15, 0, 1), 
      X = cbind(rep(1,15),
                runif(15, 0, 1), 
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1),
                runif(15, 0, 1)), 
      trueB = c(1, 1, 0, 0 ,0 ,0),
      whichX = c(T, T, T, F, F, F))

# Demo
testmod <- gdm(n = 10, 
                B = c(1, 1, 1, 1, 0, 0), 
                Estruct = "E0",
                whichX = c(T, T, T, T, T, F),
                Edist = "En")


#Demo
testimate <- estimate("HOM", model)
testimate <- lapply(c("HOM", "HC1"), estimate, test_mod)

estimate("HC3", model)

# replication demo
testrep <- replicate(1000, {
  model <- gdm(n = 1000, 
               B = c(1, 1, 0, 1, 0, 0), 
               Estruct = "E0",
               whichX = c(T, T, F, T, F, F),
               Edist = "En")
  
  estimates <- lapply(c("HOM", "HC1"), estimate, model)
  names(estimates) <- c("HOM", "HC1")
  return (estimates)
})


testimate <- ldply(test_mod, estimate)
melt(testimate, id.vars = c(".id", "Adjustment", "Beta"))
melt(testimate)

sapply(c("HOM", "HC1"), performance, testrep, 1000)


runSim(iterations = 10, 
       n = 10, 
       B = "1 1 1 1 0 0", 
       Estruct = "E0", 
       whichX = "T T T T F F", 
       Edist = "En", 
       adjust = c("HOM", "HC1"), 
       seed = 391741793)


#run Sim
source(file = "Long Ervin Replication.R")

set.seed(20150707)

design <- list(n = c(25, 50, 100, 250, 500, 1000),
               B = "1 1 1 1 0 0",
               Estruct = c("E0", "E1", "E2", "E3", "E4", "E5", "E6"),
               whichX = "T T T T T F",
               Edist = c("En", "Ech", "Et"),
               adjust = "HOM HC0 HC1 HC2 HC3")

#adjust = "HOM HC0 HC1 HC2 HC3 HC4 HC4m HC5 HC2_fp HC3_fp HC4_fp HC4m_fp HC5_fp")

params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 1000
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/20150707.csv")

rm(list = ls())

results <- read.csv("Results/20150629.csv")
head(results)

tableData <- select(results, n, Estruct, Edist, Parameter, Measure, HOM, HC0, HC1, HC2, HC3)
tableData <- melt(tableData, id.vars = c("n", "Estruct", "Parameter", "Measure", "Edist"), variable.name = "Adjustment")
tableData$n <- factor(tableData$n)
head(tableData)
     
# Figure 1 Size
Fig1 <- filter(tableData, 
               Parameter == "B3",
               Edist == "En", 
               Measure == "Size", 
               Estruct == "E0") %>%
  select(n, Adjustment, value, Estruct)

ggplot(Fig1, aes(x = n,
                 y = value,
                 group = Adjustment,
                 color = Adjustment)) +
  geom_line() +
  geom_line(aes(y = .05,
                color = "Nominal"))

# Figure 1 Power
Fig1p <- filter(tableData, 
                Parameter == "B3", 
                Edist == "Ech", 
                Measure == "Power", 
                Estruct == "E0") %>%
  select(n, Adjustment, value, Estruct)

ggplot(Fig1p, aes(x = n,
                  y = value,
                  group = Adjustment,
                  color = Adjustment)) +
  geom_line()

# Figure 2
Fig2 <- filter(tableData, 
               Edist == "Ech", 
               Measure == "Size", 
               Estruct == "E2", 
               Parameter != "B0" & Parameter != "B3") %>%
  select(n, Adjustment, value, Estruct, Parameter)

ggplot(Fig2, aes(x = n,
                 y = value,
                 group = Adjustment,
                 color = Adjustment)) +
  geom_line() +
  facet_wrap(~Parameter) +
  geom_line(aes(y = .05,
                color = "Nominal"))

# Figure 3
Fig3 <- filter(tableData, Edist == "Ech", 
               Measure == "Power", 
               Estruct == "E3", 
               Parameter == "B1" | Parameter == "B3") %>%
  select(n, Adjustment, value, Estruct, Parameter)

ggplot(Fig3, aes(x = n,
                 y = value,
                 group = Adjustment,
                 color = Adjustment)) +
  geom_line() +
  facet_wrap(~Parameter)



check <- filter(tableData,
                Measure == "Size") %>%
  select(n, Adjustment, value, Estruct, Edist, Parameter)

ggplot(check, aes(x = n,
                  y = value,
                  group = Edist,
                  color = Edist)) +
  geom_line() + 
  facet_wrap(~Parameter*Estruct*Adjustment)


check <- filter(tableData,
                Measure == "Size",
                Edist == "Ech") %>%
  select(n, Adjustment, value, Estruct, Edist, Parameter)

ggplot(check, aes(x = n,
                  y = value,
                  group = Adjustment,
                  color = Adjustment)) +
  geom_line() + 
  facet_wrap(~Parameter*Estruct)



check <- filter(tableData,
                Measure == "Size",
                Edist == "Ech") %>%
  select(n, Adjustment, value, Estruct, Parameter)

ggplot(check, aes(x = n,
                  y = value,
                  group = Parameter,
                  color = Parameter)) +
  geom_line() + 
  facet_wrap(~Estruct)

###
source(file = "Long Ervin Replication.R")

testmod <- gdm(n = 1000, B = c(1, 1, 1, 1, 0, 0), Estruct = "E6", whichX = c(T, T, T, T, T, F), Edist = "Ech")
testmod$coefs
summary(lm(testmod$Y ~ testmod$X[,-1]))

