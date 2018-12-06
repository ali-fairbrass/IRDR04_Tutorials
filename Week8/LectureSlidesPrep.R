# Week 8 Lecture Prep Script 

# Multiple hypothesis testing

list_of_p <- c(0.2, 0.001, 0.049, 0.34, 0.0035)
p.adjust(list_of_p, method = "bonferroni")

# Making plots with regression line
plot(1, type = "n", xlab="x", ylab="y", xlim=c(0, 10,1), ylim=c(0, 10,1))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
abline(0, 1, col="red", lwd=2)
abline(4, 1, col="red", lwd=2, lty="dotted")
abline(1, 2, col="blue", lwd=2)
abline(8, -1, col="blue", lwd=2, lty="dotted")
