library(tidyverse)
source("New simulations/t-test-and-simulation-functions.R")

TC_data <- 
  read_csv("data/Tworek and Cimpian 2016 Study 1.csv") %>%
  filter(excluded==0) %>%
  select(
    Ought_Score, Inherence_Bias,
    educ,
    ravens = RavensProgressiveMatrix_sum,
    conserv,
    just_world = Belief_in_Just_World
  ) %>%
  mutate_all(funs((. - mean(.)) / sd(.)))

TC_fit <- lm(Ought_Score ~ Inherence_Bias + educ + ravens + conserv + just_world, data = TC_data)

summary(TC_fit)

Y <- TC_data$Ought_Score
X <- model.matrix(TC_fit)
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

p_vals %>%
  filter(coef == "Inherence_Bias")
