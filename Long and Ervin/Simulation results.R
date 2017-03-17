library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

sim_dat <- 
  read_csv("Long and Ervin/TACC Runs/LESIM2/20160808.csv") %>%
  select(n, Estruct, Edist, iterations, HC, coef, criterion, test, starts_with("p."))

table(sim_dat$iterations)

#------------------------
# size
#------------------------

size <- 
  sim_dat %>%
  filter(criterion=="size" & HC != "OLS") %>%
  group_by(n, Estruct, Edist, HC, coef, test) %>%
  summarise_at(vars(starts_with("p")), mean) %>%
  gather("level","rate", p.005:p.100) %>%
  mutate(level = as.numeric(str_sub(level, 2, -1)))

naive_size <- 
  size %>% 
  filter(test == "naive")

ggplot(naive_size, aes(HC, rate, color = HC, fill = HC)) + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(level ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom")

dist_size <- 
  size %>%
  filter(test != "naive") %>%
  filter((HC == "HC0" & test %in% c("RCI_H","Rp_H")) | 
            (HC == "HC2" & test %in% c("KCp_E","KCp_H","saddle_E","saddle_H","Satt_E","Satt_H")))

ggplot(dist_size, aes(test, rate, color = test, fill = test)) + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(level ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

select_size <-
  size %>%
  filter((test == "naive" & HC %in% c("HC3","HC4","HC4m")) | 
           (HC == "HC2" & test %in% c("saddle_E","saddle_H","Satt_E","Satt_H"))) %>%
  mutate(test = paste0(test, HC))

ggplot(select_size, aes(test, rate, color = test, fill = test)) + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(level ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))

#------------------
# power
#------------------

power <- 
  sim_dat %>%
  filter(criterion=="power" & HC != "OLS" & coef != "x4") %>%
  group_by(n, Estruct, Edist, HC, coef, test) %>%
  summarise_at(vars(starts_with("p")), mean) %>%
  gather("level","rate", p.005:p.100) %>%
  mutate(level = as.numeric(str_sub(level, 2, -1)))

select_power <-
  power %>%
  filter((test == "naive" & HC %in% c("HC3","HC4","HC4m")) | 
           (HC == "HC2" & test %in% c("saddle_E","saddle_H","Satt_E","Satt_H"))) %>%
  mutate(test = paste0(test, HC))

ggplot(select_power, aes(test, rate, color = test, fill = test)) + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(level ~ n, scales = "free_y", labeller = label_both) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
