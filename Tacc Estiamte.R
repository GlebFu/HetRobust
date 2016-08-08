df <- data.frame(i = c(10, 32, 50, 64, 100, 96), minutes = c(2.383333, 6.4, 13.016666, 11.7833333, 17.98333,  17.55))
plot(df)
lm(minutes ~ i, df)
summary(lm(minutes ~ i, df))
data.frame(I = c(10000, 20000, 50000, 100000), hours = c(10000, 20000, 50000, 100000) * 0.1689 / 120)

