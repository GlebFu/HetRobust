library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())

load("New simulations/one-dim-sim-20170317.Rdata")

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
level_select <- .005

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
  filter(test %in% c("saddle_H","saddle_E","Satt_H","Satt_E") & level==level_select & e_dist==e_dist_select) %>%
  size_plot() 

# head-to-head
 
size_results %>%
  filter((test=="naive" & HC %in% c("HC3", "HC4")) | test %in% c("saddle_H","saddle_E","Satt_H","Satt_E", "KCCI_E", "KCCI_H"), 
         level==level_select, e_dist==e_dist_select) %>%
  size_plot()
