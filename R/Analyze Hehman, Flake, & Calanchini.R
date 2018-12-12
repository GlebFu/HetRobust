library(tidyverse)
source("New simulations/t-test-and-simulation-functions.R")



HFC_data <- 
  read_csv("data/HFC1.csv")

HFC_fit <- lm(, HFC_data)

summary(HFC_fit)

Y <- HFC_fit$Ought_Score
X <- model.matrix(HFC_fit)
whichX <- c(FALSE, rep(TRUE, 5))

new_fit <- estimate_model(Y = Y, X = X, whichX = whichX)
new_fit$coefs
coef(TC_fit)[-1]

all_tests <- c("naive",
               "Satt_E","KCp_E","KCCI_E","Rp_E","RCI_E","saddle_E",
               "Satt_H","KCp_H","KCCI_H","Rp_H","RCI_H","saddle_H")

HCs <- c("HC0","HC1","HC2","HC3","HC4","HC4m","HC5")
names(HCs) <- HCs

p_vals <- map_dfr(HCs, .f = run_tests, 
                  model = new_fit, 
                  tests = all_tests, 
                  alphas = .05, 
                  .id = "HC")
