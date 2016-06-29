rm(list = ls())

library(dplyr)
library(ggplot2)

df <- read.csv("Results/MacKinnon/20160629.csv")


filter(df, n == 40, test == "naive", criterion == "size", coef == "x4", HC %in% c("HC1", "HC2", "HC3", "HC4")) %>%
  ggplot(aes(x = g,
             y = p05,
             color = HC)) +
  geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05)


filter(df, test == "naive", criterion == "size", coef == "x4", HC == "HC3") %>%
  ggplot(aes(x = g,
             y = p05,
             color = as.factor(n))) +
  geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05)

filter(df, n == 40, test == "naive", criterion == "size") %>%
  ggplot(aes(x = g,
             y = p05,
             color = HC)) +
  geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05) +
  facet_wrap(~coef)

filter(df, n == 40, criterion == "size", coef == "x4") %>%
  ggplot(aes(x = g,
             y = p05,
             color = HC)) +
  geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05) +
  facet_wrap(~test)