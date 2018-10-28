### Unpaired t-test

# Group 1: Evacuees
set.seed(10)
age <- rnorm(n=235, mean=30, sd=11)
age <- round(age)
group <- 1:length(age)
group <- rep("Group 1",length(group))
df1 <- data.frame(age, group)

# Group 2: Non-evacuees
set.seed(10)
age <- rnorm(n=230, mean=60, sd=12)
age <- round(age)
group <- 1:length(age)
group <- rep("Group 2",length(group))
df2 <- data.frame(age, group)

# Combine data into dataframe
df <- rbind(df1, df2)

# Checking distribution of data
hist(df$age[df$group=="Group 1"], main = "Group 1", xlab = "Age")
hist(df$age[df$group=="Group 2"], main = "Group 2", xlab = "Age")

# Checking if variance of groups is equal
# Using the Levene test
# For this test, the null hypothesis is that all populations variances are equal;
# and the alternative hypothesis is that they differ.
library(car)
leveneTest(age ~ group, data = df)

# From the output, it can be seen that the p-value of 0.1147 is not less than 
# the significance level of 0.05. This means that there is no evidence to suggest 
# that the variance in plant growth is statistically significantly different for 
# the three treatment groups.

# now use t.test
t.test(df$age[df$group=="Group 2"], df$age[df$group=="Group 1"])


# Mann-Whitney test
# Investigating differences in men and womens weights
# We want to know, if the median women’s weight differs from the median men’s weight?
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
df <- data.frame( 
  group = rep(c("Woman", "Man"), each = length(women_weight)),
  weight = c(women_weight,  men_weight)
)

# Checking distribution of data
hist(df$weight[df$group=="Woman"], main = "Woman", xlab = "Weight")
hist(df$weight[df$group=="Man"], main = "Men", xlab = "Weight")

# Use a boxplot to visualise groups
boxplot(weight~group, data = df)

# Compute Wilcoxon test
wilcox.test(df$weight[df$group=="Woman"], df$weight[df$group=="Man"])
wilcox.test(women_weight, men_weight, exact=F)

# It will give a warning message, saying that “cannot compute exact p-value with tie”. 
# It comes from the assumption of a Wilcoxon test that the responses are continuous. 
# You can suppress this message by adding another argument exact = FALSE, but the result 
# will be the same.

# For reporting calculate the median and confidence intervals for the data
n <- length(women_weight)
sort(women_weight)
women_median <- median(women_weight)

# Lower CI
rank <- (n/2) - ((1.96*(sqrt(n)))/2)
rank <- round(rank)
LowerCI <- sort(women_weight)[rank]
# Upper CI
rank <- 1 + (n/2) + ((1.96*(sqrt(n)))/2)
rank <- round(rank)
UpperCI <- sort(women_weight)[rank]

paste("Median womens weight: ", women_median)
paste("Lower Confidence Interval: ", LowerCI)
paste("Upper Confidence Interval: ", UpperCI)



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
wilcox.test(group1,group2, conf.int = T)

# Wilcoxon Signed-rank Test
group1 <- c(85,70,40,65,80,75,55,20)
group2 <- c(75,50,50,40,20,65,40,25)
wilcox.test(group1,group2, paired = T, exact = T) 

# Calculate median and confidence intervals
n <- length(group1)
sort(group1)
median(group1)

# Lower CI
(n/2) - ((1.96*(sqrt(n)))/2)
# Upper CI
1 + (n/2) + ((1.96*(sqrt(n)))/2)

# Kruskall wallace test
a = c(1, 5, 8, 17, 16)
b = c(2, 16, 5, 7, 4)
c = c(1, 1, 3, 7, 9)
d = c(2, 15, 2, 9, 7)
dati = list(g1=a, g2=b, g3=c, g4=d)
