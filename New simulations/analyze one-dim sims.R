library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())

load("New simulations/one-dim-sim-20170327.Rdata")

summary(size_results$percent_NA)

size_results <- 
  size_results %>%
  select(-subset, -seed, -percent_NA) %>%
  gather("level","rejection", starts_with("p")) %>%
  mutate(
    level = as.numeric(str_sub(level, 2,-1)),
    by_val = ifelse(test=="naive", HC, test),
    reject_rel = rejection / level
  )


size_plot <- function(dat) {
  iterations <- unique(dat$iterations)
  level_select <- unique(dat$level)
  MC_CI <- qbinom(0.95, size = iterations, prob = level_select) / iterations
  p <- 
    ggplot(dat, aes(z, rejection, color = by_val)) +
      coord_cartesian(ylim = c(0, 3 * level_select)) + 
      geom_hline(yintercept = level_select) + 
      geom_hline(yintercept = MC_CI, linetype = "dashed") + 
      geom_point() + geom_line() +
      facet_grid(x_skew ~ n, scales = "free_y", labeller = label_both) + 
      theme_bw() + 
      theme(legend.position = "bottom") + 
      labs(x = "zeta (heteroskedasticity)", y = "Rejection rate", 
         color = "")
  p
}

e_dist_select <- "norm"
level_select <- .01

# naive tests

size_results %>%
  filter(test=="naive" & HC!="hom" & level==level_select & e_dist==e_dist_select) %>%
  size_plot()

# distributional tests

size_results %>%
  filter(test!="naive" & level==level_select & e_dist==e_dist_select) %>%
  size_plot() 

size_results %>%
  filter(test %in% c("KCCI_E","KCCI_H","KCp_E","KCp_H") & level==level_select & e_dist==e_dist_select) %>%
  size_plot() 

size_results %>%
  filter(test %in% c("RCI_E","RCI_H","Rp_E","Rp_H") & level==level_select & e_dist==e_dist_select) %>%
  size_plot() 

size_results %>%
  filter(test %in% c("saddle_H","saddle_E","saddle_S","Satt_H","Satt_E") & level==level_select & e_dist==e_dist_select) %>%
  size_plot() 

# selected tests
 
size_results %>%
  filter((test=="naive" & HC %in% c("HC3", "HC4")) | test %in% c("saddle_E","saddle_S","Satt_H","KCCI_E", "KCCI_H"), 
         level==level_select, e_dist==e_dist_select) %>%
  size_plot()

size_results %>%
  filter(test=="saddle_S", HC == "HC2") %>%
  ggplot(aes(z, rejection, color = e_dist, linetype = factor(x_skew))) +
  geom_point() + geom_line() +
  facet_grid(level ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "zeta (heteroskedasticity)", y = "Rejection rate", 
       color = "")


# head-to-head comparisons

size_results_wide <-
  size_results %>%
  filter((test=="naive" & HC %in% c("HC3", "HC4")) | test %in% c("saddle_E","saddle_S","Satt_H","KCCI_E", "KCCI_H")) %>%
  mutate(
    test = ifelse(test=="naive", paste(test, HC, sep = "_"), test),
    n_level = paste("n =", n, ", alpha =", level)
  ) %>%
  select(n, z, x_skew, e_dist, test, level, n_level, rejection) %>%
  spread(test, rejection)

alpha_levels <- 
  size_results_wide %>% 
  group_by(n, level) %>%
  filter(row_number()==1) %>%
  select(-(KCCI_E:Satt_H)) %>%
  mutate(x = 0, y = 0)

head_to_head <- function(x, y, data = size_results_wide) {
  p <- 
    ggplot(data, aes_(x = as.name(x), y = as.name(y), color = ~ e_dist)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      geom_blank(data = alpha_levels, aes(x, y)) +
      geom_hline(data = alpha_levels, aes(yintercept = level)) +
      geom_vline(data = alpha_levels, aes(xintercept = level)) +
      facet_wrap(~ level + n, scales = "free", labeller = "label_both", ncol = 3) + 
      theme_bw() + 
      theme(legend.position = "bottom") + 
      labs(color = "")  
  p
}

head_to_head("naive_HC3", "naive_HC4")
head_to_head("naive_HC3", "KCCI_E")
head_to_head("naive_HC3", "Satt_H")
head_to_head("naive_HC3", "saddle_E")

head_to_head("naive_HC4", "KCCI_E")
head_to_head("naive_HC4", "Satt_H")
head_to_head("naive_HC4", "saddle_E")

head_to_head("KCCI_E", "Satt_H")
head_to_head("KCCI_E", "saddle_E")
head_to_head("Satt_H", "saddle_E")
