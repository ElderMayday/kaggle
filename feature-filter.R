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


feature_filter_without_saleprice <- function(input) {
  result <- subset(input, select=c(OverallQual, GrLivArea, 
                                   GarageCars, GarageArea, YearBuilt, TotalBsmtSF, MSSubClass,
                                   X1stFlrSF, FullBath, YearRemodAdd,
                                   TotRmsAbvGrd, X2ndFlrSF, Fireplaces, LotArea, OpenPorchSF,
                                   BsmtFinSF1,
                                   OverallCond, WoodDeckSF, HalfBath, BsmtUnfSF))
  return(result)
}