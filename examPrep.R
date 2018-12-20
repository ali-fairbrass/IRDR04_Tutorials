# Q1.

# The following data:

df <- c(10, 9, 31, 10, 20, 3, 10, 11)

mean(df)
sd(df)
median(df)

df <- c(7, 14, 18, 25, 29, 24, 33, 37, 43)
hist(df)

pnorm(36, mean = 36, sd = 5)
pnorm(40, mean = 36, sd = 5) - pnorm(30, mean = 36, sd = 5) 

set.seed(10)
round(rnorm(5, 60, 20))

city1 <- c(100000, 950000, 85000, 123000, 870000)
city2 <- c(51000, 97000, 145000, 1250000, 133000)

wilcox.test(city1, city2)

set.seed(10)
before <- round(rnorm(n=10, mean=100, sd=10))
set.seed(10)
after <- round(rnorm(n=10, mean=95, sd=10))

t.test(before, after, paired = T)

# Create correlated data
require(MASS)
out <- mvrnorm(50, mu = c(0,0), Sigma = matrix(c(1,-0.56,-0.56,1), ncol = 2),
               empirical = TRUE)


png(file="exam\\scatterNegative.png", width=8,height=6, units = 'in', res=100)
plot(out[,1], out[,2], xlab = "GDP ($)", ylab = "Child mortality (n)", xaxt='n', yaxt='n')
dev.off()

20000 + -1.2*12000
20000 + (-1.2*12000) 

0.30*240

(100/65)*58

pnorm(60, mean=50, sd=15) - pnorm(30, mean=50, sd=15)
prob<-1-(pnorm(30,mean=50,sd=15,lower.tail=T)+pnorm(60,mean=50,sd=15,lower.tail = F))
prob

pnorm(40, mean = 36, sd = 5) - pnorm(30, mean = 36, sd = 5)
1-(pnorm(30, mean = 36, sd = 5,lower.tail = T) + pnorm(40, mean = 36, sd = 5, lower.tail = F))

x = c(1,3,6,3,2,5,6,7,5,4)
y = c(4,7,5,2,1,4,5,6,7,8)
z <- data.frame(x,y)
lm.out <- lm(z$x~z$y)
