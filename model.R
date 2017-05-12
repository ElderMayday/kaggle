library(lazy)
library(tree)
library(e1071)



setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!

source("parameters.R")
source("split-folds.R")
source("feature-filter.R")
source("teach-model.R")



#teaches and evaluates an abstract model using cross-validation technique over the given [folds] with current [param] configuration
cross_validation <- function(folds, model_flag, param)
{
  mse_all = c()
  
  iteration = 1
  
  while (iteration <= length(folds))
  {
    test = folds[[iteration]]
    
    train = data.frame()
    
    fold_num = 1
    
    while (fold_num <= length(folds))
    {
      if (fold_num != iteration)
        train <- rbind(train, folds[[fold_num]])
      
      fold_num = fold_num + 1
    }
    
    model = teach_model(train, model_flag, param)
    
    prediction = test[,1:(ncol(test)-1)]
    predicted = predict(model, prediction)
    mse = mean((predicted[[1]] - test[,"SalePrice"])^2)
    mse_all = c(mse_all, mse)
    
    iteration = iteration + 1
  }
  
  return(sqrt(mse))
}



#apply model selection to [train] dataframe with [model_flag] model over [param] configuration set
select_model <- function(train, model_flag, param)
{
  folds = split_folds(train)
  
  rmse_all = c()
  
  for (i in 1:nrow(param))
  {
    #print(i)          #uncomment if you want to track selection progress
    rmse = cross_validation(folds, model_flag, param[i, ])
    rmse_all = c(rmse_all, rmse)
  }
  
  min_index = which.min(rmse_all)
  min_value = min(rmse_all)
  
  param[,'rmse'] = rmse_all
  
  return(param)
}


train_unfiltered = read.csv("./train.csv", header = TRUE)
train = feature_filter(train_unfiltered)
train = reassign_factors(train, train)
train = replace_na(train)

parameters_tree = get_parameters_tree()
result_tree = select_model(train, 1, parameters_tree)
print('tree_index = ')
print(which.min(result_tree[,'rmse']))
print('tree_rmse = ')
print(min(result_tree[,'rmse']))

parameters_lazy = get_parameters_lazy()
result_lazy = select_model(train, 2, parameters_lazy)
print('lazy_index = ')
print(which.min(result_lazy[,'rmse']))
print('lazy_rmse = ')
print(min(result_lazy[,'rmse']))

parameters_svm = get_parameters_svm()
result_svm = select_model(train, 3, parameters_svm)
print('svm_index = ')
print(which.min(result_svm[,'rmse']))
print('svm_rmse = ')
print(min(result_svm[,'rmse']))
