library(ggplot2)

normdist <- function(x, mu, sigma) {
  exp((-(x - mu)^2)/2*sigma^2)/(sigma * sqrt(2*pi))
}

spread <- seq(-3, 3, by = .01)

test <- data.frame(x = spread, y = normdist(spread, 0, 1))

ggplot(test, aes(x = x,
                 y = y)) +
         geom_point()
