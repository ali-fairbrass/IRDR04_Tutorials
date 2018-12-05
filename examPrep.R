# Q1.

# The following data:

df <- c(12, 15, 18, 9, 20, 17, 13, 14)

mean(df)
sd(df)
median(df)

set.seed(10)
round(rnorm(5, 60, 20))

city1 <- c(100000, 950000, 85000, 123000, 870000)
city2 <- c(51000, 97000, 145000, 1250000, 133000)

wilcox.test(city1, city2)
