#Filters the train dataframe by features
#Only higher than 0.1 IG features were selected
#Non-numeric and sparse features are however excluded
#LotFrontage and GarageYrBlt and MasVnrArea excluded because of NaN values
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


feature_filter_without_saleprice <- function(input) {
  result <- subset(input, select=c(MSSubClass,MSZoning,LotFrontage,LotArea,Neighborhood,OverallQual,
                                   YearBuilt,YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,
                                   ExterQual,Foundation,BsmtQual,BsmtFinType1,BsmtFinSF1,TotalBsmtSF,
                                   HeatingQC,X1stFlrSF,X2ndFlrSF,GrLivArea,FullBath,KitchenQual,
                                   TotRmsAbvGrd,Fireplaces,FireplaceQu,GarageType,GarageYrBlt,
                                   GarageFinish,GarageCars,GarageArea,OpenPorchSF,))
  return(result)
}

#replaces all NA values of a dataframe to string 'No'
replace_na <- function(data)
{
  copy = data.frame(data, stringsAsFactors = FALSE) #stringsAsFactors is necessary to remove NAs
  copy[is.na(copy)] <- 0
  
  return(copy)
}


#reassigns factors to numeric values, train - determines the factor values, data - the modified dataframe with factor columns
reassign_factors <- function(train, data)
{
  group = aggregate(train[,c('SalePrice')], list(train$Neighborhood), mean)
  group_neighborhood = group[order(group$x),]
  match('CollgCr',group_neighborhood[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'Neighborhood'],group_neighborhood[,1]))
  
  data[,'Neighborhood'] = newcolumn
  return(data)
}

