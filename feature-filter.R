#Filters the train dataframe by features
#Only higher than 0.1 IG features were selected
feature_filter <- function(input) {
  result <- subset(input, select=c(MSSubClass,MSZoning,LotFrontage,LotArea,Neighborhood,OverallQual,
                                   YearBuilt,YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,
                                   ExterQual,Foundation,BsmtQual,BsmtFinType1,BsmtFinSF1,TotalBsmtSF,
                                   HeatingQC,X1stFlrSF,X2ndFlrSF,GrLivArea,FullBath,KitchenQual,
                                   TotRmsAbvGrd,Fireplaces,FireplaceQu,GarageType,GarageYrBlt,
                                   GarageFinish,GarageCars,GarageArea,OpenPorchSF,
                                   SalePrice))
  return(result)
}


#Same as above but does not remove SalePrice
feature_filter_without_saleprice <- function(input) {
  result <- subset(input, select=c(MSSubClass,MSZoning,LotFrontage,LotArea,Neighborhood,OverallQual,
                                   YearBuilt,YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,
                                   ExterQual,Foundation,BsmtQual,BsmtFinType1,BsmtFinSF1,TotalBsmtSF,
                                   HeatingQC,X1stFlrSF,X2ndFlrSF,GrLivArea,FullBath,KitchenQual,
                                   TotRmsAbvGrd,Fireplaces,FireplaceQu,GarageType,GarageYrBlt,
                                   GarageFinish,GarageCars,GarageArea,OpenPorchSF))
  return(result)
}

#replaces all NA values of a dataframe with numeric 0
replace_na <- function(data)
{
  copy = data.frame(data, stringsAsFactors = FALSE) #stringsAsFactors is necessary to remove NAs
  copy[is.na(copy)] <- 0
  
  return(copy)
}


#reassigns feature values to numeric values with [train] dataframe as determining its assignment for [data] dataframe
reassign_factors <- function(train, data)
{
  feature_names = c('MSZoning', 'Neighborhood', 'Exterior1st', 'Exterior2nd', 'MasVnrType', 'ExterQual', 'Foundation',
                    'BsmtQual', 'BsmtFinType1', 'HeatingQC', 'KitchenQual', 'FireplaceQu', 'GarageType', 'GarageFinish')
  
  for (feature_name in feature_names)
  {
    group = aggregate(train[,c('SalePrice')], list(train[,feature_name]), mean)  #group feature values by average price
    group = group[order(group$x),]
    
    newcolumn = c()          #column which will substitute the current feature column
    for (i in 1:nrow(data))
      newcolumn = c(newcolumn, match(data[i,feature_name],group[,1]))
    
    data[,feature_name] = newcolumn
  }

  return(data)
}

