# load data from csv file
butterflies <- read.csv('data/butterflies.csv')

# Plot subset of data
butterflies_sub <- butterflies[which(butterflies$Species=="maniola jurtina" |
                                       butterflies$Species=="pyronia tithonus"),]
plot(butterflies_sub$Width, butterflies_sub$Height, col=butterflies_sub$Species, pch=16, asp=1,
     xlab = "Width", ylab = "Height")

legend(1,2, legend=c("Maniola jurtina", "Pyronia tithonus"), col=c("red", "black"), pch=16,cex=0.8)

# Create 75/25 train/test split

# Test
validation_indices <- sample(1:nrow(butterflies), 25, replace=FALSE)
#Train
train_indices <- setdiff(1:nrow(butterflies), validation_indices)
# create new dataset
train_data <- butterflies[train_indices,]
validation_data <- butterflies[validation_indices,]

#plot
#all data
plot(butterflies$Width, butterflies$Height, col=butterflies$Species, pch=16, xlab="Width (cm)", ylab = "Height (cm)")
#Train data
plot(train_data$Width, train_data$Height, col = train_data$Species, pch=16,  xlab="Width (cm)", ylab = "Height (cm)")
#Test data
plot(validation_data$Width, validation_data$Height, col = validation_data$Species, pch=16,  xlab="Width (cm)", ylab = "Height (cm)")

library('randomForest')
# If there's a problem installing follow guidance here to use an alternative mirror:
# https://stackoverflow.com/questions/31249980/error-in-r-package-which-is-only-available-in-source-form-and-may-need-compil
