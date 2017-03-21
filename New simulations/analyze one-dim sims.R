library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
rm(list=ls())
load("New simulations/one-dim-sim-20170317.Rdata")

size_results <- 
  size_results %>%
  select(-subset, -seed, -percent_NA) %>%
  gather("level","rejection", starts_with("p")) %>%
  mutate(
    level = as.numeric(str_sub(level, 2,-1)),
    by_val = ifelse(test=="naive", HC, test),
    reject_rel = rejection / level
  )

alpha_dat <- 
  size_results %>%
  group_by(z, level) %>%
  summarise(iterations = mean(iterations)) %>%
  mutate(
    alpha = level, 
    MC_CI = qbinom(0.95, size = iterations, prob = level) / iterations
  ) %>%
  ungroup()

size_plot <- function(dat) {
  title_txt <- paste0("n = ", unique(dat$n), ", error dist = ", unique(dat$e_dist))
  p <- ggplot(dat, aes(x_skew, reject_rel, color = by_val)) +
          coord_cartesian(ylim = c(0, 3)) + 
          geom_hline(yintercept = 1) + 
          geom_point() + geom_line() +
          facet_grid(level ~ z, scales = "free_y", labeller = label_both) + 
          theme_bw() + 
          theme(legend.position = "bottom") + 
          labs(x = "Covariate skewnesss", y = "Relative rejection rate", 
               color = "", title = title_txt)
  p
}

n_select <- 50

# naive tests

size_results %>%
  filter(test=="naive" & HC!="hom" & n==n_select & e_dist=="norm") %>%
  size_plot() 

size_results %>%
  filter(test=="naive" & HC!="hom" & n==n_select & e_dist=="chisq") %>%
  size_plot() 

size_results %>%
  filter(test=="naive" & HC!="hom" & n==n_select & e_dist=="t5") %>%
  size_plot() 

# distributional tests

size_results %>%
  filter(test!="naive" & n==n_select & e_dist=="norm") %>%
  size_plot() 

size_results %>%
  filter(test!="naive" & n==n_select & e_dist=="chisq") %>%
  size_plot() 

size_results %>%
  filter(test!="naive" & n==n_select & e_dist=="t5") %>%
  size_plot() 

# head-to-head
 
size_results %>%
  filter((test=="naive" & HC=="HC4") | test %in% c("saddle_H","saddle_E","Satt_H"), 
         n==n_select, e_dist=="norm") %>%
  size_plot()

size_results %>%
  filter((test=="naive" & HC=="HC4") | test %in% c("saddle_H","saddle_E","Satt_H"), 
         n==n_select, e_dist=="chisq") %>%
  size_plot()

size_results %>%
  filter((test=="naive" & HC=="HC4") | test %in% c("saddle_H","saddle_E","Satt_H"), 
         n==n_select, e_dist=="t5") %>%
  size_plot()
