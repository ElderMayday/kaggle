library(tree)
library(e1071)
library(stats)
library(lazy)
library(caret)
library(nnet)

setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!
source("parameters.R")
source("split-folds.R")
source("feature-filter.R")




#evaluates the MSE of the prediction
evaluate <- function(prediction)
{
  return(mean((prediction[,"PredictedPrice"] - prediction[,"RealPrice"])^2))
}


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



#teaches a model depending of its type and hyperparameter
#model_flag: 1 - tree, 2 - lm, 3 - svm
teach_model <- function(train, model_flag, param)
{
  if (model_flag == 1)
  {
    model = tree(SalePrice~., train, control = tree.control(nobs = param[1, 'nobs'], mincut = param[1, 'mincut'], minsize = param[1, 'minsize'], mindev = param[1, 'mindev']))
  }
  else if (model_flag == 2)
  {
    model = lm(SalePrice~., train) 
  }
  else if (model_flag == 3)
  {
    model = svm(SalePrice~., train, degree=10, nu = 0.8, cachesize = 100, tolerance = 0.1, epsilon = 0.5)
  }
  return(model)
}





select_model <- function(train, model_flag, param)
{
  folds = split_folds(train, 5) #TO-REVIEW
  
  rmse_all = c()
  
  for (i in 1:nrow(param))
  {
    rmse = cross_validation(folds, model_flag, param[i, ])
    rmse_all = c(rmse_all, rmse)
  }
  
  min_index = which.min(rmse_all)
  min_value = min(rmse_all)
  
  return(c(min_index, min_value))
}


train_raw = read.csv("./train.csv", header = TRUE, stringsAsFactors=FALSE)
train = feature_filter(train_raw)

tree_parameters = get_tree_parameters()
result_tree = select_model(train, 1, tree_parameters)

print('tree_index = ')
print(result_tree[1])
print('tree_rmse = ')
print(result_tree[2])

lm_parameters = get_lm_parameters()
result_lm = select_model(train, 2, lm_parameters)

print('lazy_index = ')
print(result_lm[1])
print('lazy_rmse = ')
print(result_lm[2])

svm_parameters = get_svm_parameters()[20,]
result_svm = select_model(train, 3, svm_parameters)

print('svm_index = ')
print(result_svm[1])
print('svm_rmse = ')
print(result_svm[2])

if (FALSE)
{
  con=lazy.control(conIdPar=NULL, linIdPar=1, quaIdPar=NULL, distance=c("manhattan","euclidean"), metric=NULL, cmbPar=1, lambda=1e+03)
  model = lazy(SalePrice~., train,control=con)
  
model = tree(SalePrice~., train, con=tree.control(nobs = 10000, mincut = 1, minsize = 2, mindev = 0))   #lazy

model = lm(SalePrice~., train)

model = svm(SalePrice~., train, degree=10, nu = 0.8, cachesize = 100, tolerance = 0.1, epsilon = 0.5)

model <- train(SalePrice~., train, method='nnet', linout=TRUE, trace=FALSE, size=5, decay=0.001) 

prediction = train[,1:(ncol(train)-1)]
prediction[,"PredictedPrice"] = predict(model, prediction)
prediction[,"RealPrice"] = train[,"SalePrice"]
vec = prediction[,"PredictedPrice"] - prediction[,"RealPrice"]
mse = mean((prediction[,"PredictedPrice"] - prediction[,"RealPrice"])^2)
rmse = sqrt(mse)
print(rmse)
}

