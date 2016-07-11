rm(list = ls())

library(dplyr)
library(ggplot2)

df <- read.csv("Results/MacKinnon/20161016.csv")

df <- filter(df, !(HC == "OLS" & test %in% levels(df$test)[-5]))

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


filter(df, HC != "OLS", test != "naive") %>%
  ggplot(aes(x = g,
             y = p.005,
             color = test)) +
  #geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .005,
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC == "OLS") %>% select(-HC),
              color = "black",
              linetype = "dashed",
              size = .2, 
              se = F) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> tbl1

tbl1 + coord_cartesian(ylim = c(0, .01))

#-------------------------------
# g by .010, HC ~ n, color = test
#-------------------------------


filter(df, HC != "OLS", test != "naive") %>%
  ggplot(aes(x = g,
             y = p.010,
             color = test)) +
  #geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .01,
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC == "OLS") %>% select(-HC),
              color = "black",
              linetype = "dashed",
              size = .2, 
              se = F) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> tbl1

tbl1 + coord_cartesian(ylim = c(0, .02))

#-------------------------------
# g by .050, HC ~ n, color = test
#-------------------------------


filter(df, HC != "OLS", test != "naive") %>%
  ggplot(aes(x = g,
             y = p.050,
             color = test)) +
  #geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .05,
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC == "OLS") %>% select(-HC),
              color = "black",
              linetype = "dashed",
              size = .2, 
              se = F) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> tbl1

tbl1 + coord_cartesian(ylim = c(0, .1))

#-------------------------------
# g by .100, HC ~ n, color = test
#-------------------------------


filter(df, HC != "OLS", test != "naive") %>%
  ggplot(aes(x = g,
             y = p.100,
             color = test)) +
  #geom_point() + 
  geom_smooth(method = "loess", size = .5, se = F) +
  geom_hline(yintercept = .10,
             linetype = "dashed") +
  facet_grid(n ~ HC) +
  geom_smooth(data = filter(df, HC == "OLS") %>% select(-HC),
              color = "black",
              linetype = "dashed",
              size = .2, 
              se = F) +
  geom_smooth(data = filter(df, HC != "OLS" & test == "naive"),
              color = "black",
              size = .2, 
              se = F) -> tbl1

tbl1 + coord_cartesian(ylim = c(0, .2))
