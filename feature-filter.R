#Filters the train dataframe by features
#Only higher than 0.1 IG features were selected
#Non-numeric and sparse features are however excluded
#LotFrontage and GarageYrBlt and MasVnrArea excluded because of NaN values
feature_filter <- function(input) {
  result <- subset(input, select=c(MSSubClass,MSZoning,LotArea,Neighborhood,OverallQual,YearBuilt,
                                   YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,ExterQual,Foundation,
                                   BsmtQual,BsmtFinType1,BsmtFinSF1,TotalBsmtSF,HeatingQC,X1stFlrSF,
                                   X2ndFlrSF,GrLivArea,FullBath,KitchenQual,TotRmsAbvGrd,Fireplaces,
                                   FireplaceQu,GarageType,GarageFinish,GarageCars,GarageArea,OpenPorchSF,
                                   SalePrice))
  result = replace_na(result)
  return(result)
}


feature_filter_without_saleprice <- function(input) {
  result <- subset(input, select=c(MSSubClass,MSZoning,LotArea,Neighborhood,OverallQual,YearBuilt,
                                   YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,ExterQual,Foundation,
                                   BsmtQual,BsmtFinType1,BsmtFinSF1,TotalBsmtSF,HeatingQC,X1stFlrSF,
                                   X2ndFlrSF,GrLivArea,FullBath,KitchenQual,TotRmsAbvGrd,Fireplaces,
                                   FireplaceQu,GarageType,GarageFinish,GarageCars,GarageArea,OpenPorchSF))
  result = replace_na(result)
  return(result)
}

#replaces all NA values of a dataframe to string 'No'
replace_na <- function(data)
{
  copy = data.frame(data, stringsAsFactors = FALSE)
  for (i in 1:nrow(copy))
    for (j in 1:ncol(copy))
      if (is.na(copy[i,j]))
        copy[i,j] <- 'No'
  return(copy)
}

