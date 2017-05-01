library(lazy)

#Filters the train dataframe
#Only higher than 0.1 IG features were selected
#PoolQC, Fence and MiscFeature are however excluded since in is uneffective (sparse actual values - the information gain could be random)


#LotFrontage excluded because of NaN values
feature_filter <- function(input) {
  result <- subset(input, select=c(OverallQual, GrLivArea, 
                                   GarageCars, GarageArea, YearBuilt, TotalBsmtSF, MSSubClass,
                                   X1stFlrSF, FullBath, GarageYrBlt, YearRemodAdd,
                                   TotRmsAbvGrd, X2ndFlrSF, Fireplaces, LotArea, OpenPorchSF,
                                   BsmtFinSF1, MasVnrArea,
                                   OverallCond, WoodDeckSF, HalfBath, BsmtUnfSF, SalePrice))
  return(result)
}


train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)
train = feature_filter(train_raw)
regtree = lazy(SalePrice~., train) 
plot(regtree)
text(regtree, pretty=0)



row = train[,1:(ncol(train)-1)]
row[,"PredictedPrice"] = predict(regtree, row)
row[,"RealPrice"] = train[,"SalePrice"]



