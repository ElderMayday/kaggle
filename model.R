library(lazy)
library(tree)
library(e1071)

source("parameters.R")
source("split-folds.R")

#Filters the train dataframe by features
#Only higher than 0.1 IG features were selected
#Non-numeric and sparse features are however excluded
#LotFrontage and GarageYrBlt and MasVnrArea excluded because of NaN values
feature_filter <- function(input) {
  result <- subset(input, select=c(OverallQual, GrLivArea, 
                                   GarageCars, GarageArea, YearBuilt, TotalBsmtSF, MSSubClass,
                                   X1stFlrSF, FullBath, YearRemodAdd,
                                   TotRmsAbvGrd, X2ndFlrSF, Fireplaces, LotArea, OpenPorchSF,
                                   BsmtFinSF1,
                                   OverallCond, WoodDeckSF, HalfBath, BsmtUnfSF, SalePrice))
  return(result)
}



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
    
    prediction = test[,1:(ncol(train)-1)]
    predicted = predict(model, prediction)
    #p1 = predicted[[1]]
    #p2 = test[,"SalePrice"]
    #p = p1 - p2
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
    cont = lazy.control(conIdPar=param[1, 'conIdPar'], linIdPar=param[1, 'linIdPar'], quaIdPar=param[1, 'quaIdPar'], distance=c("manhattan","euclidean"), metric=param[1, 'metric'], cmbPar=param[1,'cmbPar'], lambda=param[1, 'lambda'])
    model = lazy(SalePrice~., train, control = con)
  }
  else if (model_flag == 3)
  {
    
  }
  return(model)
}





select_model <- function(train, model_flag, param)
{
  folds = split_folds(train)
  
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

setwd('D:/kaggle')  #TO-MODIFY depending on the directory path!!!
train_raw = read.csv("./train.csv", header = TRUE)
train = feature_filter(train_raw)


tree_parameters = get_tree_parameters()
result = select_model(train, 1, tree_parameters)

print('tree_index = ')
print(result[1])
print('tree_rmse = ')
print(result[2])


lazy_parameters = get_lazy_parameters()
result = select_model(train, 2, lazy_parameters)

print('lazy_index = ')
print(result[1])
print('lazy_rmse = ')
print(result[2])

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

