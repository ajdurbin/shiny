library(ggplot2)
library(gridExtra)

num <- 1000
input <- matrix(runif(n = num, min = 0, max = 1), ncol = 2)
r <- sqrt(-2 * log(input[, 1]))
theta <- 2 * pi * input[, 2]
x <- r * cos(theta)
y <- r * sin(theta)

summary(x)
var(x)
summary(y)
var(y)
cov(x, y)
shapiro.test(x)
shapiro.test(y)

qx <- ggplot() +
  geom_qq(mapping = aes(sample = x), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("X QQ Plot")
qy <- ggplot() +
  geom_qq(mapping = aes(sample = y), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Y QQ Plot")
hx <- ggplot() +
  geom_histogram(mapping = aes(x = x, y = ..density..), binwidth = 0.5) +
  geom_density(mapping = aes(x = x), alpha = 0.2, color = "red") +
  ggtitle("Y Histogram With Density")
hy <- ggplot() +
  geom_histogram(mapping = aes(x = y, y = ..density..), binwidth = 0.5) +
  geom_density(mapping = aes(x = y), alpha = 0.2, color = "red") +
  ggtitle("Y Histogram With Density")

grid.arrange(qx, hx, qy, hy, ncol = 2)
