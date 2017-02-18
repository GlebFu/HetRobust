rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

df <- read.csv("Results/MacKinnon/20161016.csv")

df <- filter(df, !(HC == "OLS" & test %in% levels(df$test)[-5])) %>% 
  select(-X, -(B:power), -(iterations:seed), -(coef:criterion), -percent_NA) %>%
  gather(key = alpha, value = p, p.005:p.100) %>%
  mutate(alpha = as.numeric(sub(".","", alpha)),
         ULa = alpha * 1.2,
         comb = paste(HC, test),
         approx = str_split_fixed(test, "_", n = 2)[,1],
         approx = factor(approx, 
                         levels = c("naive","KCCI","KCp","RCI","Rp","saddle","Satt"),
                         labels = c("naive","Kauermann-Carroll","KCp","Rothenberg","Rp",
                                    "saddlepoint","Satterthwaite")),
         residuals = str_split_fixed(test, "_", n = 2)[,2],
         residuals = ifelse(residuals=="E", "Empirical","Working model"),
         combo = paste(HC, approx)) %>%
  group_by(HC, test, n, alpha) %>%
  mutate(under = prod(p <= ULa)) %>%
  ungroup()

width <- 15
height <- 10
#-----------------------------
# Naive tests only
#-----------------------------

df %>%
  filter(HC != "OLS", test == "naive") %>%
  ggplot(aes(x = g,
             y = p,
             color = HC)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_hline(aes(yintercept = ULa),
             color = "grey") +
  geom_hline(aes(yintercept = alpha)) +
  facet_grid(alpha ~ n, labeller = "label_both", scales = "free_y") +
  labs(x = "Degree of heteroskedasticity (gamma)", y = "Rejection rate", color = "") +
  theme_minimal()
ggsave("AERA figures/Figure-1.wmf", width = width, height = height)


#------------------------------------------------
# Reference distribution approximations
#------------------------------------------------

select_combos <- c("HC2 Kauermann-Carroll","HC1 Rothenberg","HC2 Rothenberg", 
                   "HC2 saddlepoint","HC2 Satterthwaite")

df %>%
  filter(under & combo %in% select_combos) %>%
  ggplot(aes(x = g,
             y = p,
             linetype = residuals,
             color = combo)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_hline(aes(yintercept = ULa),
             color = "grey") +
  geom_hline(aes(yintercept = alpha)) +
  facet_grid(alpha ~ n, labeller = "label_both", scales = "free_y") +
  labs(x = "Degree of heteroskedasticity (gamma)", y = "Rejection rate", 
       color = "Approximation", linetype = "Auxilliary estimates") +
  theme_minimal()
ggsave("AERA figures/Figure-2.wmf", width = width, height = height)

#------------------------------------------------
# Comparing naive and reference distribution
#------------------------------------------------

winners <- c("HC3 naive","HC4 naive","HC1 RCI_H","HC2 saddle_H")

df %>%
  filter(comb %in% winners) %>%
  ggplot(aes(x = g,
             y = p,
             color = combo)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_hline(aes(yintercept = ULa),
             color = "grey") +
  geom_hline(aes(yintercept = alpha)) +
  facet_grid(alpha ~ n, labeller = "label_both", scales = "free_y") +
  labs(x = "Degree of heteroskedasticity (gamma)", y = "Rejection rate", 
       color = "Approximation") +
  theme_minimal()

ggsave("AERA figures/Figure-3.wmf", width = width, height = height)
