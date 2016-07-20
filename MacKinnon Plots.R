rm(list = ls())

library(dplyr)
library(ggplot2)

df <- read.csv("Results/MacKinnon/20161016.csv")

df <- filter(df, !(HC == "OLS" & test %in% levels(df$test)[-5]))

MCSE <- function(a, k = 20000) sqrt((a * (1-a))/k)

#-------------------------------
# MacKinnon Plots
#-------------------------------

filter(df, n == 40, test == "naive", criterion == "size", coef == "x4", HC %in% c("HC1", "HC2", "HC3", "HC4", "OLS")) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = HC)) +
  geom_point() + 
  geom_smooth(method = "loess", size = .2, se = F) +
  geom_hline(yintercept = .05)


filter(df, test == "naive", criterion == "size", coef == "x4", HC == "HC3") %>%
  ggplot(aes(x = g,
             y = p.050,
             color = as.factor(n))) +
  geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05)


#-------------------------------
# g by .005, HC ~ n, color = test
#-------------------------------

pv <- .050
filter(df, HC != "OLS", test != "naive") %>%
  ggplot(aes(x = g,
             y = p.050,
             color = test)) +
  #geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  # geom_hline(yintercept = pv) +
  facet_grid(n ~ HC) +
#   geom_smooth(data = filter(df, HC == "OLS") %>% select(-HC),
#               color = "black",
#               linetype = "dashed",
#               size = .2, 
#               se = F) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, 2*pv))

filter(df, HC != "OLS", test != "naive") %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = test)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, 2*pv))

incl <- c("HC1 RCI_H", "HC2 saddle_E", "HC2 saddle_H", "HC2 KCp_H", "HC2 KCCI_E", "HC2 Satt_E",
          "HC4 KCp_H", "HC5 KCCI_H",
          "HC5 KCp_E", "HC5 RCI_E", "HC5 Rp_E")

filter(df, HC != "OLS", test != "naive") %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = test)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, pv + MCSE(pv)))

##############
# n = 20
##############
incl <- c("HC1 RCI_H", "HC2 saddle_E", "HC2 saddle_H", "HC2 KCp_H",
          "HC5 KCp_E", "HC5 RCI_E", "HC5 Rp_E")

filter(df, HC != "OLS", test != "naive", n == 20) %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = comb)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1


##############
# n = 40
##############
incl <- c("HC1 RCI_H", "HC2 saddle_E", "HC2 KCp_H")

filter(df, HC != "OLS", test != "naive", n == 40) %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = comb)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, pv + MCSE(pv)))


##############
# n = 60
##############
incl <- c("HC1 RCI_H", "HC2 saddle_E")


filter(df, HC != "OLS", test != "naive", n == 60) %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = comb)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, pv + MCSE(pv)))

##############
# n = 80
##############
incl <- c("HC1 RCI_H", "HC2 saddle_E", "HC2 KCCI_E")


filter(df, HC != "OLS", test != "naive", n == 80) %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = comb)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, pv + MCSE(pv)))

##############
# n = 100
##############
incl <- c("HC1 RCI_H", "HC2 KCCI_E")


filter(df, HC != "OLS", test != "naive", n == 100) %>%
  mutate(comb = paste(HC, test)) %>%
  group_by(HC, test, n) %>%
  mutate(excl = prod(p.050 < pv + MCSE(pv))) %>%
  filter(excl == 1, (comb %in% incl)) %>%
  ggplot(aes(x = g,
             y = p.050,
             color = comb)) +
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = pv + MCSE(pv),
             linetype = "dashed") +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> fig1

fig1 + coord_cartesian(ylim = c(0, pv + MCSE(pv)))