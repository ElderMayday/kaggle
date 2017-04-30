library(FSelector)   #load the feature-selection library

#load the train data
train = read.csv("D:\\kaggle\\train.csv", header = TRUE)

#calculate the information gain of each feature
weights <- information.gain(SalePrice~., train)

print(weights)

#select top features
subset <- cutoff.k(weights, 5)

#print the results
f <- as.simple.formula(subset, "Features")

print(f)