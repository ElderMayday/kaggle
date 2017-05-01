library(tree)

#Filters the train dataframe
#Only higher than 0.1 IG features were selected
#PoolQC, Fence and MiscFeature are however excluded since in is uneffective (sparse actual values - the information gain could be random)

feature_filter <- function(input) {
  result <- subset(input, select=c(OverallQual, Neighborhood, Alley, GrLivArea, 
                                   GarageCars, GarageArea, YearBuilt, ExterQual, KitchenQual, TotalBsmtSF, BsmtQual, MSSubClass,
                                   X1stFlrSF, FullBath, GarageYrBlt, GarageFinish, FireplaceQu, YearRemodAdd, Foundation,
                                   LotFrontage, GarageType, TotRmsAbvGrd, X2ndFlrSF, Fireplaces, LotArea, OpenPorchSF,
                                   Exterior1st, Exterior2nd, HeatingQC, BsmtFinType1, BsmtFinSF1, MSZoning, MasVnrArea, MasVnrType,
                                   HouseStyle, OverallCond, WoodDeckSF, HalfBath, BsmtUnfSF, SalePrice))
  return(result)
}

train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)
train = feature_filter(train_raw)
model = tree(SalePrice~., train, minsize = 5, mindev = 0) 
plot(model)
text(model)

prediction = train[,1:(ncol(train)-1)]
prediction[,"PredictedPrice"] = predict(model, prediction)
prediction[,"RealPrice"] = train[,"SalePrice"]

