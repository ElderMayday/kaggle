#performs model combination, outputs the result on test data, estimates mse on train data

library(lazy)
library(tree)
library(e1071)

setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!

source("parameters.R")
source("feature-filter.R")
source("teach-model.R")

train_raw = read.csv("./train.csv", header = TRUE)
train = feature_filter(train_raw)
train = reassign_factors(train, train)
train = replace_na(train)

#assign hardcoded precomputated indexes of the best model configurations
tree_conf_id = 54
lazy_conf_id = 18
svm_conf_id = 1

#retrieve best configurations
tree_parameters = get_parameters_tree()[tree_conf_id,]
lazy_parameters = get_parameters_lazy()[lazy_conf_id,]
svm_parameters = get_parameters_svm()[svm_conf_id,]

#hardcoded rmse of the best model configurations in order to bias the result solution for the test set
rmse_tree = 71361
rmse_lazy = 30307
rmse_svm = 116588

p1 = 1 / rmse_tree
p2 = 1/ rmse_lazy
p3 = 1/ rmse_svm

s = p1 + p2 + p3
w1 = p1 / s
w2 = p2 / s
w3 = p3 / s

tree_model = teach_model(train, 1, tree_parameters)
lazy_model = teach_model(train, 2, lazy_parameters)
svm_model = teach_model(train, 3, svm_parameters)

prediction = train[,1:(ncol(train)-1)]
predicted_tree = predict(tree_model, prediction)

prediction = train[,1:(ncol(train)-1)]
predicted_lazy = predict(lazy_model, prediction)[[1]]  #[[1]] since lazy package implementation returns the result as a list containing a vector as the first element

prediction = train[,1:(ncol(train)-1)]
predicted_svm = predict(svm_model, prediction)

predicted = predicted_tree * w1 + predicted_lazy * w2 + predicted_svm * w3


mse = mean((predicted - train[,'SalePrice'])^2)
rmse = sqrt(mse)



#-----------------------------------------------------------------

test_raw = read.csv("./test.csv", header = TRUE)
train_raw = read.csv("./train.csv", header = TRUE)  
train = feature_filter(train_raw)
test = feature_filter_without_saleprice(test_raw)
test = reassign_factors(train, test)
test = replace_na(test)

prediction = test
predicted_tree = predict(tree_model, prediction)

prediction = test
predicted_lazy = predict(lazy_model, prediction)[[1]]  #[[1]] since lazy package implementation returns the result as a list containing a vector as the first element

prediction = test
predicted_svm = predict(svm_model, prediction)

predicted = predicted_tree * w1 + predicted_lazy * w2 + predicted_svm * w3

write.table(predicted, row.names = c(1461:2919), file = "foo.csv", sep = ",", col.names = 'Id,SalePrice', qmethod = "double", quote = FALSE)
