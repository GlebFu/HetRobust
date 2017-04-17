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
  unnest() %>%
  mutate(test = ifelse(test == "naive", paste(test, HC), test))


summary(power_results$percent_NA)
table(power_results$test)
table(power_results$iterations)
power_results %>%
  select(starts_with("p0."), starts_with("n0.")) %>%
  summary()

rel_power_results <-
  power_results %>%
  select(-HC, -seed, -iterations, -span, -coef, -percent_NA) %>%
  gather("level","rate", starts_with("p0"), starts_with("n0"))

rel_power_results <- 
  rel_power_results %>%
  filter(test=="naive HC5") %>%
  select(-test) %>%
  rename(HC5_rate = rate) %>%
  inner_join(rel_power_results) %>%
  filter(test != "naive HC5") %>%
  mutate(rel_power = rate / HC5_rate)


rel_power_plot <- function(dat) {
  ggplot(dat, aes(beta, rel_power, color = test)) + 
    geom_line(size = 1) + 
    # geom_smooth(method = "loess", se = FALSE, span = 0.2) + 
    facet_grid(z ~ x_skew, scales = "free_y", labeller = "label_both") + 
    theme_light() + 
    theme(legend.position = "bottom", 
          strip.text.x = element_text(color = "black"), 
          strip.text.y = element_text(color = 'black')) + 
    labs(x = quote(beta), y = "Relative power", 
         color = "", linetype = "", shape = "") + 
    guides(color = guide_legend(nrow = 1))
}

#-----------------------------------
# alpha = .05
#-----------------------------------

# normal errors

rel_power_results %>%
  filter(n==25, e_dist=="norm", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="norm", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="norm", level=="p0.05") %>%
  rel_power_plot()

# chi-squared errors

rel_power_results %>%
  filter(n==25, e_dist=="chisq", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="chisq", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="chisq", level=="p0.05") %>%
  rel_power_plot()

# t(5) errors

rel_power_results %>%
  filter(n==25, e_dist=="t5", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="t5", level=="p0.05") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="t5", level=="p0.05") %>%
  rel_power_plot()

#-----------------------------------
# alpha = .01
#-----------------------------------

# normal errors

rel_power_results %>%
  filter(n==25, e_dist=="norm", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="norm", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="norm", level=="p0.01") %>%
  rel_power_plot()

# chi-squared errors

rel_power_results %>%
  filter(n==25, e_dist=="chisq", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="chisq", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="chisq", level=="p0.01") %>%
  rel_power_plot()

# t(5) errors

rel_power_results %>%
  filter(n==25, e_dist=="t5", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="t5", level=="p0.01") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="t5", level=="p0.01") %>%
  rel_power_plot()


#-----------------------------------
# alpha = .005
#-----------------------------------

# normal errors

rel_power_results %>%
  filter(n==25, e_dist=="norm", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="norm", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="norm", level=="p0.005") %>%
  rel_power_plot()

# chi-squared errors

rel_power_results %>%
  filter(n==25, e_dist=="chisq", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="chisq", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="chisq", level=="p0.005") %>%
  rel_power_plot()

# t(5) errors

rel_power_results %>%
  filter(n==25, e_dist=="t5", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==50, e_dist=="t5", level=="p0.005") %>%
  rel_power_plot()

rel_power_results %>%
  filter(n==100, e_dist=="t5", level=="p0.005") %>%
  rel_power_plot()
