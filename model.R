library(lazy)
library(tree)
library(e1071)
library(stats)

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
#model_flag: 1 - tree, 2 - lazy, 3 - svm
teach_model <- function(train, model_flag, param)
{
  if (model_flag == 1)
  {
    model = tree(SalePrice~., train, control = tree.control(nobs = param[1, 'nobs'], mincut = param[1, 'mincut'], minsize = param[1, 'minsize'], mindev = param[1, 'mindev']))
  }
  else if (model_flag == 2)
  {
    model = lazy(SalePrice~., train, control = lazy.control(conIdPar=NULL, linIdPar=param[1, 'linIdPar'], quaIdPar=NULL, distance=c("manhattan","euclidean"), metric=NULL, cmbPar=param[1, 'cmbPar'], lambda=param[1, 'lambda']))
  }
  else if (model_flag == 3)
  {
    model = svm(SalePrice~., train, degree=param[1, 'degree'], nu=param[1, 'nu'], cachesize = 100, tolerance=param[1, 'tolerance'], epsilon=param[1, 'epsilon'])
  }
  return(model)
}





select_model <- function(train, model_flag, param)
{
  folds = split_folds(train)
  
  rmse_all = c()
  
  for (i in 1:nrow(param))
  {
    print(i)
    rmse = cross_validation(folds, model_flag, param[i, ])
    rmse_all = c(rmse_all, rmse)
  }
  
  min_index = which.min(rmse_all)
  min_value = min(rmse_all)
  
  param[,'rmse'] = rmse_all
  
  return(param)
}


train_raw = read.csv("./train.csv", header = TRUE)   #stringsAsFactors is necessary to remove NAs
train = feature_filter(train_raw)
train = reassign_factors(train, train)
train = replace_na(train)

tree_parameters = get_tree_parameters()
result_tree = select_model(train, 1, tree_parameters)

print('tree_index = ')
print(which.min(result_tree[,'rmse']))
print('tree_rmse = ')
print(min(result_tree[,'rmse']))

lazy_parameters = get_lazy_parameters()
result_lazy = select_model(train, 2, lazy_parameters)

print('lazy_index = ')
print(which.min(result_lazy[,'rmse']))
print('lazy_rmse = ')
print(min(result_lazy[,'rmse']))

svm_parameters = get_svm_parameters()
result_svm = select_model(train, 3, svm_parameters)

print('svm_index = ')
print(which.min(result_svm[,'rmse']))
print('svm_rmse = ')
print(min(result_svm[,'rmse']))


factor(train[,'Neighborhood'])


nvalues = unique(train[,'Neighborhood'])

where(nvalues == 'CollgCr')



if (FALSE)
{
con=lazy.control(conIdPar=NULL, linIdPar=1, quaIdPar=NULL, distance=c("manhattan","euclidean"), metric=NULL, cmbPar=1, lambda=1e+03)
model = lazy(SalePrice~., train,control=con)   #lazy

model = tree(SalePrice~., train, con=tree.control(nobs = 10000, mincut = 1, minsize = 2, mindev = 0))   #lazy


model = svm(SalePrice~., train, degree=10, nu = 0.9, cachesize = 100, tolerance = 0.1, epsilon = 0.5)


prediction = train[,1:(ncol(train)-1)]
prediction[,"PredictedPrice"] = predict(model, prediction)
prediction[,"RealPrice"] = train[,"SalePrice"]
vec = prediction[,"PredictedPrice"] - prediction[,"RealPrice"]
mse = mean((prediction[,"PredictedPrice"] - prediction[,"RealPrice"])^2)
rmse = sqrt(mse)
print(rmse)
}

