#performs model combination, outputs the result on test data, estimates mse on train data

library(lazy)
library(tree)
library(e1071)

setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!

source("parameters.R")
source("feature-filter.R")
source("teach-model.R")

combine <- function(data, model_tree, model_lazy, model_svm, rmse_tree, rmse_lazy, rmse_svm)
{
  p1 = 1 / rmse_tree
  p2 = 1/ rmse_lazy
  p3 = 1/ rmse_svm
  
  s = p1 + p2 + p3
  w1 = p1 / s
  w2 = p2 / s
  w3 = p3 / s
  
  predicted_tree = predict(model_tree, data)
  predicted_lazy = predict(model_lazy, data)[[1]]  #[[1]] since lazy package implementation returns the result as a list containing a vector as the first element
  predicted_svm = predict(model_svm, data)
  
  predicted = predicted_tree * w1 + predicted_lazy * w2 + predicted_svm * w3
  
  return(predicted)
}

train_raw = read.csv("./train.csv", header = TRUE)
train = feature_filter(train_raw)
train = reassign_factors(train, train)
train = replace_na(train)


#assign hardcoded precomputated indexes of the best model configurations
tree_conf_id = 54
lazy_conf_id = 18
svm_conf_id = 1

tree_parameters = get_parameters_tree()[tree_conf_id,]
lazy_parameters = get_parameters_lazy()[lazy_conf_id,]
svm_parameters = get_parameters_svm()[svm_conf_id,]

rmse_tree = 71361
rmse_lazy = 30307
rmse_svm = 116588

model_tree = teach_model(train, 1, tree_parameters)
model_lazy = teach_model(train, 2, lazy_parameters)
model_svm = teach_model(train, 3, svm_parameters)

#apply to train set (to evaluate combined rmse)

data = replace_na(train)[,1:(ncol(train)-1)]   #train data without SalePrice column
predicted = combine(data, model_tree, model_lazy, model_svm, rmse_tree, rmse_lazy, rmse_svm)

mse = mean((predicted - train[,'SalePrice'])^2)
rmse = sqrt(mse)

#apply to test set

test_raw = read.csv("./test.csv", header = TRUE)
train_raw = read.csv("./train.csv", header = TRUE)  
train = feature_filter(train_raw)
test = feature_filter_without_saleprice(test_raw)
test = reassign_factors(train, test)
data = replace_na(test)

predicted = combine(data, model_tree, model_lazy, model_svm, rmse_tree, rmse_lazy, rmse_svm)

write.table(predicted, row.names = c(1461:2919), file = "out.csv", sep = ",", col.names = 'Id,SalePrice', qmethod = "double", quote = FALSE)
