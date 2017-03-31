library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())

load("New simulations/one-dim-sim-20170327.Rdata")

summary(size_results$percent_NA)
table(size_results$test)
table(size_results$iterations)

size_results <- 
  size_results %>%
  select(-subset, -seed, -percent_NA) %>%
  gather("level","rejection", starts_with("p")) %>%
  mutate(
    level = as.numeric(str_sub(level, 2,-1)),
    alpha = paste("\alpha =", str_sub(level, 2, -1)),
    by_val = ifelse(test=="naive", HC, test),
    reject_rel = rejection / level,
    error_dist = factor(e_dist, 
                        levels = c("norm","t5","chisq"), 
                        labels = paste(c("Normal","t(5)","Chi-squared"), "errors")),
    skew = paste("Skewness =", x_skew)
  )

#----------------------------
# summary plots 
#----------------------------

summary_measures <- 
  size_results %>%
  group_by(level, n, HC, test, by_val) %>%
  summarize(
    pct_below = mean(rejection <= level),
    MAE = median(abs(rejection - level)),
    RMSE = sqrt(mean((rejection - level)^2))
  ) %>%
  mutate(
    MARE = MAE / level,
    RRMSE = RMSE / level
  )

summary_measures %>%
  # filter(pct_below > 0.0, RRMSE < 0.5) %>%
  filter(test %in% c("saddle_H","Satt_H","KCCI_H")) %>%
  ggplot(aes(pct_below, RMSE, color = by_val)) + 
    geom_point() + 
    geom_text(aes(label = by_val), size = 3) + 
    expand_limits(y = 0) + 
    facet_grid(level ~ n, scales = "free_y") + 
    theme_light() +
    theme(legend.position = "none", 
        strip.text.x = element_text(color = "black"), 
        strip.text.y = element_text(color = 'black')) 

summary_measures %>%
  filter(pct_below > 0.0, MARE < 0.5) %>%
  ggplot(aes(pct_below, MAE, color = by_val)) + 
  geom_point() + 
  geom_text(aes(label = by_val), size = 3) + 
  expand_limits(y = 0) + 
  facet_grid(level ~ n, scales = "free_y") + 
  theme_light() +
  theme(legend.position = "none", 
        strip.text.x = element_text(color = "black"), 
        strip.text.y = element_text(color = 'black')) 

#----------------------------
# Detailed plots
#----------------------------

size_results_select <- 
  size_results %>%
  filter(
    e_dist %in% c("norm","chisq"), 
    level %in% c(.005, .01, .05)
  )


size_plot <- function(dat) {
  iterations <- unique(dat$iterations)
  alphas <- unique(dat$alpha)
  alpha_levels <- unique(dat$level)
  MC <- qbinom(p = 0.95, size = iterations, prob = alpha_levels) / iterations
  MC_bound <- data.frame(alpha = alphas, 
                         level = alpha_levels, 
                         MC = MC)
  
  p <- 
    ggplot(dat, aes(z, rejection, color = by_val, shape = by_val)) +
      geom_point() + geom_line() +
      geom_hline(data = MC_bound, aes(yintercept = level), linetype = "solid") + 
      geom_hline(data = MC_bound, aes(yintercept = MC), linetype = "dashed") + 
      facet_grid(alpha + error_dist ~ skew, scales = "free_y") + 
      expand_limits(y = 0) + 
      theme_light() + 
      theme(legend.position = "bottom", 
            strip.text.x = element_text(color = "black"), 
            strip.text.y = element_text(color = 'black')) + 
      labs(x = quote(Heteroskedasticity (zeta)), y = "Rejection rate", 
           color = "", linetype = "", shape = "") + 
      guides(color = guide_legend(nrow = 1))
  p
}

# homoskedasticity results
size_results_select %>%
  filter(HC=="hom") %>%
  mutate(by_val = factor(n)) %>%
  size_plot()


# all naive tests
size_results_select %>%
  filter(test=="naive", HC %in% c("hom","HC2","HC3","HC4","HC5"), n == 25) %>%
  size_plot()
size_results_select %>%
  filter(test=="naive", HC %in% c("hom","HC2","HC3","HC4","HC5"), n == 50) %>%
  size_plot()
size_results_select %>%
  filter(test=="naive", HC %in% c("hom","HC2","HC3","HC4","HC5"), n == 100) %>%
  size_plot()


# naive tests
size_results_select %>%
  filter(test=="naive" & HC %in% c("HC2","HC3","HC4","HC4m","HC5"), n == 25) %>%
  size_plot()
size_results_select %>%
  filter(test=="naive" & HC %in% c("HC2","HC3","HC4","HC4m","HC5"), n == 50) %>%
  size_plot()
size_results_select %>%
  filter(test=="naive" & HC %in% c("HC2","HC3","HC4","HC4m","HC5"), n == 100) %>%
  size_plot()

# Edgeworth approximations
size_results_select %>%
  filter(test == "RCI_E", n == 100) %>%
  size_plot() 

size_results_select %>%
  filter(test %in% c("KCCI_E","KCCI_H","KCp_E","KCp_H","RCI_H"), n == 25) %>%
  size_plot() 
size_results_select %>%
  filter(test %in% c("KCCI_E","KCCI_H","KCp_E","KCp_H","RCI_H"), n == 50) %>%
  size_plot() 
size_results_select %>%
  filter(test %in% c("KCCI_E","KCCI_H","KCp_E","KCp_H","RCI_H"), n == 100) %>%
  size_plot() 

# Satterthwaite and saddlepoint approximations
size_results_select %>%
  filter(test %in% c("saddle_H","saddle_E","saddle_S","Satt_H","Satt_E") & n == 25) %>%
  size_plot() 
size_results_select %>%
  filter(test %in% c("saddle_H","saddle_E","saddle_S","Satt_H","Satt_E") & n == 50) %>%
  size_plot() 
size_results_select %>%
  filter(test %in% c("saddle_H","saddle_E","saddle_S","Satt_H","Satt_E") & n == 100) %>%
  size_plot() 

# selected tests
 
selected_tests <- c("saddle_E","saddle_H","saddle_S")

size_results_select %>%
  filter((test=="naive" & HC %in% c("HC4")) | test %in% selected_tests, n == 25) %>%
  size_plot()
size_results_select %>%
  filter((test=="naive" & HC %in% c("HC4")) | test %in% selected_tests, n == 50) %>%
  size_plot()
size_results_select %>%
  filter((test=="naive" & HC %in% c("HC4")) | test %in% selected_tests, n == 100) %>%
  size_plot()


size_results_select %>%
  filter(test=="saddle_S") %>%
  ggplot(aes(z, rejection, color = skew, shape = skew)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept = level)) + 
  expand_limits(y = 0) + 
  facet_grid(level + e_dist ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = quote(Heteroskedasticity (zeta)), y = "Rejection rate", 
       color = "", shape = "")


size_results_select %>%
  filter(test=="saddle_T") %>%
  ggplot(aes(z, rejection, color = skew, shape = skew)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept = level)) + 
  expand_limits(y = 0) + 
  facet_grid(level + e_dist ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = quote(Heteroskedasticity (zeta)), y = "Rejection rate", 
       color = "", shape = "")


# head-to-head comparisons

size_results_wide <-
  size_results %>%
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
    ggplot(data, aes_(x = as.name(x), y = as.name(y), color = ~ e_dist, shape = ~ as.factor(x_skew))) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0) +
      geom_blank(data = alpha_levels, aes(x, y)) +
      geom_hline(data = alpha_levels, aes(yintercept = level)) +
      geom_vline(data = alpha_levels, aes(xintercept = level)) +
      facet_wrap(~ level + n, scales = "free", labeller = "label_both", ncol = 3) + 
      theme_bw() + 
      theme(legend.position = "bottom") + 
      labs(color = "", shape = "skew")  
  p
}

head_to_head("naive_HC3", "naive_HC4")
head_to_head("naive_HC3", "KCCI_E")
head_to_head("naive_HC3", "Satt_H")
head_to_head("naive_HC3", "saddle_E")

head_to_head("naive_HC4", "KCCI_E")
head_to_head("naive_HC4", "Satt_H")
head_to_head("naive_HC4", "saddle_E")
head_to_head("naive_HC4", "saddle_S")
head_to_head("naive_HC4", "saddle_T")

head_to_head("KCCI_E", "Satt_H")
head_to_head("KCCI_E", "saddle_E")
head_to_head("Satt_H", "saddle_E")
head_to_head("Satt_H", "saddle_S")
head_to_head("Satt_H", "saddle_T")
