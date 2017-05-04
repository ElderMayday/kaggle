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

train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)
train = feature_filter(train_raw)
con=lazy.control(conIdPar=NULL, linIdPar=1, quaIdPar=NULL, distance=c("manhattan","euclidean"), metric=NULL, cmbPar=1, lambda=1e+03)
model = lazy(SalePrice~., train,control=con)   #lazy

model = tree(SalePrice~., train, con=tree.control(nobs = 10000, mincut = 1, minsize = 2, mindev = 0))   #lazy

model = nnet(SalePrice~., train, size = 2, rang = 0.1,
             decay = 5e-4, maxit = 200)

model = svm(SalePrice~., train, degree=10, nu = 0.9, cachesize = 100, tolerance = 0.1, epsilon = 0.5)


prediction = train[,1:(ncol(train)-1)]
prediction[,"PredictedPrice"] = predict(model, prediction)
prediction[,"RealPrice"] = train[,"SalePrice"]
vec = prediction[,"PredictedPrice"] - prediction[,"RealPrice"]
mse = mean((prediction[,"PredictedPrice"] - prediction[,"RealPrice"])^2)
rmse = sqrt(mse)
print(rmse)

