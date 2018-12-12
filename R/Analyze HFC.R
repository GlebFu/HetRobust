library(tidyverse)
library(sandwich)
library(lmtest)
source("New simulations/t-test-and-simulation-functions.R")

HFC1 <- read_csv("data/HFC1.csv")

HFC_fit <- lm(DisproportionateBlackDeath ~ WhiteImplicit + WhiteExplicit + 
                BlackMedianIncome + WhiteMedianIncome + 
                PcentHSdegreeBlack + PcentHSdegreeWhite + 
                PcentBAdegreeBlack + PcentBAdegreeWhite + 
                Segregation + 
                BlackImplicit + BlackExplicit + 
                VioCrime + Unemployment + 
                PopulationDensity + TotalLethalForceRate, data = HFC1)

summary(HFC_fit)
coeftest(HFC_fit, vcov. = vcovHC, type = "HC2")



# Re-fit model using our functions

HFC_mod_frame <- model.frame(HFC_fit) 

Y <- HFC_mod_frame$DisproportionateBlackDeath
X <- model.matrix(HFC_fit)
whichX <- c(FALSE, rep(TRUE, ncol(X) - 1))

HFC_mod <- estimate_model(Y = Y, X = X, whichX = whichX)
all.equal(HFC_mod$coefs, coef(HFC_fit)[-1])

all.equal(test_hom(HFC_mod)$naive,
          as.vector(summary(HFC_fit)$coefficients[-1,4]))

# Calculate p-values for all HCs and tests

HCs <- c("HC0","HC1","HC2","HC3","HC4","HC4m","HC5")
names(HCs) <- HCs

all_tests <- c("naive", 
               "Satt_E","KCp_E","KCCI_E","Rp_E","RCI_E", "saddle_E",
               "Satt_H","KCp_H","KCCI_H","Rp_H","RCI_H", "saddle_H")

run_tests(HC = "HC0", model = HFC_mod, tests = all_tests, alphas = .05)

p_vals <- map_dfr(HCs, run_tests, 
                  model = HFC_mod, tests = all_tests, alphas = .05, 
                  .id = "HC")

p_vals %>%
  filter(coef == "WhiteImplicit")
