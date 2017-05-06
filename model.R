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
  data <- data_ordered[sample(nrow(data_ordered)),]
  
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

#
cross_validation <- function(folds, model)
{
  
}

train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)
train = feature_filter(train_raw)

folds = split_folds(train)


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

