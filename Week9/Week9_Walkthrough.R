# IRDR04 Research Tools Module
# Week 9 Tutorial 
# 4/12/18 9-11am @ Birkbeck, Malet Street 414/415 Public Cluster
# Tutor: Dr Alison Fairbrass, UCL Centre for Biodiversity & Environment Research, alison.fairbrass@gmail.com



# # #  Walk-through # # #

# install the random forest package
# If there's a problem installing follow guidance here to use an alternative mirror:
# https://stackoverflow.com/questions/31249980/error-in-r-package-which-is-only-available-in-source-form-and-may-need-compil
install.packages('randomForest')

# load the random forest package
library(randomForest)

# load data from csv file
butterflies <- read.csv('data/butterflies.csv')

# plot the dataset
plot(butterflies$Width, butterflies$Height, col=butterflies$Species, pch=16)

# split the data into validation and test
# randomly choose take 50 datapoints for validation
validation_indices <- sample(1:nrow(butterflies), 50, replace=FALSE)
# use the rest for training
# train_indices <- setdiff(seq(1, nrow(butterflies)), validation_indices)
train_indices <- setdiff(1:nrow(butterflies), validation_indices)

# create the new dataframes
train_data <- butterflies[train_indices,]
validation_data <- butterflies[validation_indices,]
print(paste("Number of training samples: ", nrow(train_data)))
print(paste("Number of validation samples: ", nrow(validation_data)))

# draw circles around the training set
points(train_data$Width, train_data$Height, pch=1, cex=1.5)

# train the classifier
rf <- randomForest(Species ~ ., train_data, ntree=20)

# make predictions for the validation set
species_predictions <- predict(rf, validation_data)

# compute the classification accuracy on the validation set
correct_classifications <- validation_data$Species == species_predictions
percent_correct_predictions <- mean(correct_classifications)*100
percent_incorrect_predictions <- (1 - mean(correct_classifications))*100
print(paste("Classification accuracy (%): ", percent_correct_predictions))

# extract the incorrect predictions so we can plot them
incorrect_predictions <- validation_data[correct_classifications==FALSE, ]

# draw an 'x' where we predict the wrong answer
points(incorrect_predictions$Width, incorrect_predictions$Height, pch=4, cex=2, lwd=2, col=4)

# insert legend
legend_text <- c(levels(butterflies$Species), 'train data', 'incorrect predictions')
legend(x='topleft', legend=legend_text, col=c(1, 2, 3, 1, 4), pch=c(16, 16, 16, 1, 4))
