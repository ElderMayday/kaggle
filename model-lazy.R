library(lazy)

#Filters the train dataframe
#Only higher than 0.1 IG features were selected
#Non-numeric and sparse features are however excluded since in is uneffective
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
con=lazy.control(conIdPar=NULL, linIdPar=1, quaIdPar=NULL,
             distance=c("manhattan","euclidean"), metric=NULL,
             cmbPar=1, lambda=1e+03)
model = lazy(SalePrice~., train,control=con) 

prediction = train[,1:(ncol(train)-1)]
prediction[,"PredictedPrice"] = predict(model, prediction)
prediction[,"RealPrice"] = train[,"SalePrice"]
rmse = sqrt(mean(prediction[,"PredictedPrice"] - prediction[,"RealPrice"])^2)
print(rmse)


