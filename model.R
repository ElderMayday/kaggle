train_raw = read.csv("D:\\kaggle\\train.csv", header = TRUE)


#PoolQC is however excluded since in is uneffective (sparse actual values - the information gain could be random)
#Only higher than 0.1 IG features were selected

train = subset(train_raw, select=c(OverallQual, Neighborhood, Alley, GrLivArea, 
GarageCars, GarageArea, YearBuilt, ExterQual, KitchenQual, TotalBsmtSF, BsmtQual, MSSubClass,
X1stFlrSF, FullBath, GarageYrBlt, GarageFinish, FireplaceQu, YearRemodAdd, Foundation, Fence,
LotFrontage, GarageType, TotRmsAbvGrd, X2ndFlrSF, MiscFeature, Fireplaces, LotArea, OpenPorchSF,
Exterior1st, Exterior2nd, HeatingQC, BsmtFinType1, BsmtFinSF1, MSZoning, MasVnrArea, MasVnrType,
HouseStyle, OverallCond, WoodDeckSF, HalfBath, BsmtUnfSF, SalePrice))