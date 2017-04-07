library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())
load("New simulations/one-dim-sim-power-results.Rdata")
ls()
power_results <- 
  power_results %>%
  select(-alphas) %>%
  unnest()
summary(power_results$percent_NA)
table(power_results$test)
table(power_results$iterations)
power_results %>%
  select(starts_with("p0."), starts_with("n0.")) %>%
  summary()

power_results %>%
  filter(n == 25, e_dist == "t5", -1 <= beta, beta <= 1) %>%
  ggplot(aes(beta, p0.01, color = test)) + 
  geom_point() + geom_line() + 
  facet_grid(z ~ x_skew, scales = "free_y", labeller = "label_both") + 
  expand_limits(y = 0) + 
  theme_light() + 
  theme(legend.position = "bottom", 
        strip.text.x = element_text(color = "black"), 
        strip.text.y = element_text(color = 'black')) + 
  labs(x = quote(beta), y = "Rejection rate", 
       color = "", linetype = "", shape = "") + 
  guides(color = guide_legend(nrow = 1))


