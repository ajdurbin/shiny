library(tidyverse)
library(gridExtra)


# from class --------------------------------------------------------------


# ## bootstrap mean
# ## set up a Gamma(alpha,beta) distribution
# alpha <- 4; ibeta <- 1/4
# n <- 30
# x <- rgamma(n, alpha, ibeta)
# 
# ## for overlaying the truth
# xt <- seq(0,qgamma(0.999, alpha, ibeta), length=1000)
# f <- dgamma(xt, alpha, ibeta)
# ## plot a histogram of the samples
# hist(x, freq=FALSE, xlim=range(xt),
#      ylim=c(0,1.5*max(f)), main="")
# lines(xt, f, col=2, lwd=2, lty=2)
# legend("topright", "true pdf", lty=2, col=2,
#        lwd=2, cex=1.5, bty="n")
# 
# ## normal approximation
# xbar <- mean(x)
# s2.norm <- var(x)
# za <- qnorm(0.975, 0, sqrt(s2.norm))
# q.norm <- xbar + c(-1,1)*za/sqrt(n)
# points(xbar, 0, col=3, cex=1.5)
# text(q.norm, c(0,0), c("[", "]"), 
#      col=3, cex=1.5)
# 
# ## now via bootstrap
# B <- 199
# X.star <- matrix(NA, nrow=B, ncol=n)
# for(b in 1:B) {
#   X.star[b,] <- sample(x, n, replace=TRUE)
# }
# 
# ## calculate the bootstrap mean-interval
# xbar.boot <- apply(X.star, 1, mean)
# q.boot <- quantile(xbar.boot, c(0.025, 0.975))
# c(mean(xbar.boot), q.boot)
# 
# ## adding bootstrap interval to plot
# text(q.boot, c(0,0), c("[", "]"), col=4, cex=1.5)
# ## we can also add information about the entire
# ## bootstrap distribution for the mean to the plot
# lines(density(xbar.boot), col=4, lty=3, lwd=2)
# legend("right", "boot mean distn", cex=1.5,
#        col=4, lty=3, lwd=2, bty="n")


# ggplot ------------------------------------------------------------------

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=0), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=2), color="red", linetype="dashed", size=1)
                          
# now plot confidence interval, mean, density with legend 
ggplot(dat, aes(x=rating)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(rating, na.rm=T), color = "mean"),   # Ignore NA values for mean
             linetype="dashed", size=1) +
  geom_vline(aes(xintercept=0, color = '95% Confidence Interval'), linetype="dashed", size=1) +
  geom_vline(aes(xintercept=2), color="red", linetype="dashed", size=1) +
  scale_color_manual("statistics", values = c("red", "blue")) +
  ggtitle("Normal Approximation")
