# Group 1: Evacuees
set.seed(10)
x <- rnorm(n=235, mean=60, sd=11)
x <- round(x)

# Group 2: Non-evacuees
set.seed(10)
y <- rnorm(n=230, mean=30, sd=12)
y <- round(y)

# Checking distribution of data
hist(x, main = "", xlab = "Age")
hist(y)

# Checking if vairance of groups is equal
11/12

# now use t.test
t.test(x,y)
