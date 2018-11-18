hist(rbeta(10000,5,2), col="orange", main = "", xlab = "values")
hist(rbeta(10000,2,5), col="orange", main = "", xlab = "count", xaxt = "n")
hist(rbeta(10000,5,5), col="lightblue", main = "", xlab = "count", xaxt = "n")

skew <- rbeta(10000,2,5)
normal1 <- rbeta(100,5,5)
normal2 <- rbeta(100,5,5)

df1 <- data.frame(skew, normal1)

plot(df1)

df2 <- data.frame(normal1, normal2)

plot(df2)

logtrans <- log10(skew)
hist(logtrans)

# Bootstrapping
scores <- c(45, 65, 70, 35, 27, 75, 80, 66, 41, 81)
originalMean <- mean(scores)
sample1 <- c(45, 70, 27, 41, 27, 75, 81, 45, 70, 35)
Mean1 <- mean(sample1)
sample2 <- c(35, 75, 41, 41, 65, 70, 35, 45, 65, 66)
Mean2 <- mean(sample2)
sample3 <- c(81, 66, 81, 80, 35, 41, 65, 70, 45, 81)
Mean3 <- mean(sample3)

# Using sample function
sampleX <- sample(scores, length(scores), replace = T)

library("boot")
boot.mean <- function(x,i){boot.mean <- mean(x[i])}
sampleBoot <- boot(scores, statistic = boot.mean, R=10000)
hist(sampleBoot$t, col="orange", main="bootstrap distribution \nreps = 10,000", xlab = "mean scores")

# Multiple hypothesis testing
p = c(0.04, 0.01, 0.001, 0.06, 0.4)
p.adjust(p, method = p.adjust.methods, n = length(p))
