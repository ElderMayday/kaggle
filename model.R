library(lazy)
library(tree)
library(e1071)

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

#splits the [data] dataframe into [fold_num] folds
split_folds <- function(data_ordered, fold_num=5)
{
  #permutate the datarows in a random way
  data = data_ordered
  #data <- data_ordered[sample(nrow(data_ordered)),]    TO-RETURN!!!
  
  #the size of a small fold
  size = nrow(data) %/% fold_num
  
  #number of (size+1) folds
  bigfold_num = nrow(data) %% fold_num
  
  #number of (size) folds
  smallfold_num = fold_num - bigfold_num
  
  #current datarow
  current_row = 1
  
  #current fold number
  current_fold = 1
  
  #the vector of all folds
  folds = list()
  
  #bigfold partition loop
  while (current_fold <= bigfold_num)
  {
    folds[[length(folds) + 1]] = data[current_row:(current_row + size),]
    current_fold = current_fold + 1
    current_row = current_row + size + 1
  }
  
  #reset the fold counter for the remaining small folds
  current_fold = 1
  
  #smallfold partition loop
  while (current_fold <= smallfold_num)
  {
    folds[[length(folds) + 1]] = data[current_row:(current_row + size - 1),]
    current_fold = current_fold + 1
    current_row = current_row + size
  }
  
  return(folds)
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
    mse = mean((predicted - test[,"SalePrice"])^2)
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
    model = tree(SalePrice~., train, con=tree.control(nobs = param[1, 'nobs'], mincut = param[1, 'mincut'], minsize = param[1, 'minsize'], mindev = param[1, 'mindev']))
  }
  else if (model_flag == 2)
  {
    
  }
  else if (model_flag == 3)
  {
    
  }
  return(model)
}


#creates a predefined set of hyperparameter configurations
get_tree_parameters <- function()
{
  param_tree_1 = data.frame(nobs = 10000)
  
  mincut_vector = c(1, 2, 3, 4, 5, 10, 20, 50, 100)
  minsize_vector = c(2:10)
  
  param_tree_2a = data.frame(mincut = mincut_vector)
  param_tree_2b = data.frame(minsize = minsize_vector)
  param_tree_2 = merge(param_tree_2a, param_tree_2b)
  param_tree_2[,'minsize'] = param_tree_2[,'minsize'] * param_tree_2[,'mincut']
  
  param_tree_3 = data.frame(mindev = c(0, 0.01, 0.05, 0.10, 0.50, 1.0, 10.0, 100.0))
  
  param_tree = merge(param_tree_1, param_tree_2)
  param_tree = merge(param_tree, param_tree_3)
  return(param_tree)
}

train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)
train = feature_filter(train_raw)

folds = split_folds(train)

tree_parameters = get_tree_parameters()

rmse_all = c()



for (i in 1:nrow(tree_parameters))
{
  rmse = cross_validation(folds, 1, tree_parameters[i, ])
  rmse_all = c(rmse_all, rmse)
}

min_index = which.min(rmse_all)
min_value = min(rmse_all)

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
