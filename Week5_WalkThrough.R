### Unpaired t-test

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
hist(y, main = "", xlab = "Age")

# Checking if variance of groups is equal
11/12

# now use t.test
t.test(x,y)

# Paired t-test

# Group 1: Children before event
set.seed(10)
x <- rnorm(n=130, mean=100, sd=10)
x <- round(x)

# Group 2: Children after event
set.seed(10)
y <- rnorm(n=130, mean=85, sd=20)
y <- round(y)

# Checking distribution of data
hist(x, main = "", xlab = "Academic score")
hist(y, main = "", xlab = "Academic score")

# Checking if variance of groups is equal
10/20

# now use t.test specifying the paired arguement
t.test(x,y, paired = T)


# Chi-squared test
# Create 2x2 table
results <- matrix(c(24,14,12,27),ncol=2,byrow=TRUE)
colnames(results) <- c("No disease", "Disease")
rownames(results) <- c("Group 1: Non-Evacuees", "Group 2: Evacuees")
results <- as.table(results)
results

# Run chi-sq test
chisq.test(results)

# ANOVA with 3 groups
# Create dataframe
DrugA <- c(4, 5, 4, 3, 2, 4,3, 4, 4)
DrugB <- c(6,8,4,5,4,6,5,8,6)
DrugC <- c(6,7,6,6,7,5,6,5,5)
results <- c(DrugA, DrugB, DrugC)

drug <- c(rep("DrugA", length(DrugA)), rep("DrugB", length(DrugB)), rep("DrugC", length(DrugC)))

df <- as.data.frame(cbind(results,drug))
df$results <- as.numeric(df$results)

# Look at summary statistics if you like
install.packages('fBasics')
library('fBasics')
basicStats(results)[c("Mean", "Stdev"),]

# Now run ANOVA test
res.aov <- aov(results ~ drug, data = df)
# Summary of the analysis
summary(res.aov)

# Simulating Random Multivariate Correlated Data (Continuous Variables)
# Code from https://www.r-bloggers.com/simulating-random-multivariate-correlated-data-continuous-variables/
R = matrix(cbind(1,.80,.2,  .80,1,.7,  .2,.7,1),nrow=3)
U = t(chol(R))
nvars = dim(U)[1]
numobs = 100000
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
names(raw) = c("response","predictor1","predictor2")
cor(raw)
plot(head(raw, 100))
plot(head(orig.raw,100))

# Plotting scatter plots for lecture
plot(head(raw$response,100), head(raw$predictor2,100), xlab="drownings", ylab = "air con units sold",
     cex = 2, cex.axis = 2, cex.lab=2, col = "red")
plot(head(raw$predictor1,100), head(raw$predictor2,100), xlab="drownings", ylab = "ice creams sold",
     cex = 2, cex.axis = 2, cex.lab=2, col = "blue")
plot(head(raw$predictor1,100), head(raw$predictor2,100), xlab="", ylab = "", xaxt='n', yaxt = 'n',
     cex = 2, cex.axis = 2, cex.lab=2, col = "blue")
plot(head(raw$response,100), head(raw$predictor1,100), xlab="", ylab = "", xaxt='n', yaxt = 'n',
     cex = 2, cex.axis = 2, cex.lab=2, col = "red")
plot(head(raw$response,100), head(raw$predictor2,100), xlab="", ylab = "", xaxt='n', yaxt = 'n',
     cex = 2, cex.axis = 2, cex.lab=2, col = "orange")

# Mann-Whitney test
group1 <- c(1,2,4,6,3)
group2 <- c(7,5,6,4,12)
hist(group2, col = "orange", main="Group 2")
wilcox.test(group1,group2)
