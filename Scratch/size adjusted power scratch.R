
library(tibble)
require(purrr)
require(tidyr)
require(dplyr)

rm(list=ls())

source("New simulations/t-test-and-simulation-functions.R")

one_dim_dgm <- function(n = 25, B = c(0, 0), whichX = c(F, T), 
                        x_skew = 0.1, z = 0, e_dist = "norm", ...) {
  
  v <- 8 / x_skew^2
  x <- (rchisq(n, df = v) - v ) / sqrt(2 * v)
  
  e <- switch(e_dist,
              norm = rnorm(n, 0, 1),
              chisq = (rchisq(n, 5) - 5) / sqrt(10),
              t5 = rt(n, 5) * sqrt(3 / 5)
  )
  
  sigma <- exp(z * x)
  
  Y <- B[1] + B[2] * x + sigma * e
  
  fitted_model <- estimate_model(Y = Y, X = cbind(x0 = 1, x1 = x), 
                                 trueB = B, whichX = whichX)
  
  fitted_model$sigma <- sigma
  
  fitted_model
}

load("New simulations/one-dim-sim-adjusted_alphas.Rdata")

dgm <- one_dim_dgm
iterations <- 2000

j <- 1
n <- adjusted_alphas$n[[j]]
span <- adjusted_alphas$span[[j]]
z <- adjusted_alphas$z[[j]]
x_skew <- adjusted_alphas$x_skew[[j]]
alphas <- adjusted_alphas$alphas[[1]]

alphas <- c(.005, .010, .050, .100)
names(alphas) <- paste0("p", alphas)
adjusted_alpha <- TRUE

HCtests <-
  tribble(~ HC, ~ tests,
          "HC2", list("Satt_H", "saddle_E","saddle_H", "KCCI_H"),
          "HC4", list("naive")
  )

seed <- NULL


run_sim(dgm = dgm, iterations = iterations, n = n, 
        z = z, x_skew = x_skew, 
        alphas = alphas, HCtests = HCtests, span = span, 
        adjusted_alpha = adjusted_alpha, seed = seed)



if (is.vector(alphas)) {
  test_alphas <- HCtests
  test_alphas$alphas <- rep(list(alphas), nrow(test_alphas))
} else {
  test_alphas <- 
    alphas %>%
    group_by_(.dots = "HC") %>%
    nest(.key = "alphas") %>%
    right_join(HCtests, by = "HC")
}

# simulate

res <- 
  rerun(.n = iterations, {
    model <- dgm(n = n)
    invoke_rows(.f = run_tests, .d = test_alphas, 
                model = model, span = span,
                .to = "res")
  }) %>%
  bind_rows() %>%
  select_(.dots = c("HC", "res"))

x <- 
  res %>%
  group_by_(.dots = "HC") %>%
  nest(.key = "x") %>%
  left_join(test_alphas, by = "HC") %>%
  select_(.dots = c("HC", "x", "alphas"))

x %>%
  invoke_rows(.f = calculate_rejections, adjusted_alpha = adjusted_alpha, .to = "reject_rates") %>%
  select_(.dots = c("HC", "reject_rates")) %>%
  unnest()

alphas <- x$alphas[[1]]
x <- x$x[[1]]
calculate_rejections(x = x, alphas = alphas, adjusted_alpha = adjusted_alpha)


y <- 
  unnest(x) %>%
  gather("test", "p_val", -1) %>%
  group_by_(.dots = c("coef", "test")) %>%
  nest(.key = "p_vals") 

if (is.vector(alphas)) {
  y$alphas <- rep(list(alphas), nrow(y))
} else {
  y <- left_join(y, alphas, by = c("coef","test"))
}

p_vals <- y$p_vals[[1]]
alphas <- y$alphas[[1]]
