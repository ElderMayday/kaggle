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
  # MSZoning
  group = aggregate(train[,c('SalePrice')], list(train$MSZoning), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'MSZoning'],group[,1]))
  
  data[,'MSZoning'] = newcolumn
  
  
  # Neighborhood
  group = aggregate(train[,c('SalePrice')], list(train$Neighborhood), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'Neighborhood'],group[,1]))
  
  data[,'Neighborhood'] = newcolumn
  
  
  # Exterior1st
  group = aggregate(train[,c('SalePrice')], list(train$Exterior1st), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'Exterior1st'],group[,1]))
  
  data[,'Exterior1st'] = newcolumn
  
  # Exterior2nd
  group = aggregate(train[,c('SalePrice')], list(train$Exterior2nd), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'Exterior2nd'],group[,1]))
  
  data[,'Exterior2nd'] = newcolumn
  
  
  # MasVnrType
  group = aggregate(train[,c('SalePrice')], list(train$MasVnrType), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'MasVnrType'],group[,1]))
  
  data[,'MasVnrType'] = newcolumn
  
  
  
  # ExterQual
  group = aggregate(train[,c('SalePrice')], list(train$ExterQual), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'ExterQual'],group[,1]))
  
  data[,'ExterQual'] = newcolumn
  
  # Foundation
  group = aggregate(train[,c('SalePrice')], list(train$Foundation), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'Foundation'],group[,1]))
  
  data[,'Foundation'] = newcolumn
  
  
  # BsmtQual
  group = aggregate(train[,c('SalePrice')], list(train$BsmtQual), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'BsmtQual'],group[,1]))
  
  data[,'BsmtQual'] = newcolumn
  
  
  # BsmtFinType1
  group = aggregate(train[,c('SalePrice')], list(train$BsmtFinType1), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'BsmtFinType1'],group[,1]))
  
  data[,'BsmtFinType1'] = newcolumn
  
  # HeatingQC
  group = aggregate(train[,c('SalePrice')], list(train$HeatingQC), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'HeatingQC'],group[,1]))
  
  data[,'HeatingQC'] = newcolumn
  
  
  # KitchenQual
  group = aggregate(train[,c('SalePrice')], list(train$KitchenQual), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'KitchenQual'],group[,1]))
  
  data[,'KitchenQual'] = newcolumn
  
  
  # FireplaceQu
  group = aggregate(train[,c('SalePrice')], list(train$FireplaceQu), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'FireplaceQu'],group[,1]))
  
  data[,'FireplaceQu'] = newcolumn
  
  # GarageType
  group = aggregate(train[,c('SalePrice')], list(train$GarageType), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'GarageType'],group[,1]))
  
  data[,'GarageType'] = newcolumn
  
  # GarageFinish
  group = aggregate(train[,c('SalePrice')], list(train$GarageFinish), mean)
  group = group[order(group$x),]
  match('CollgCr',group[,1])
  
  newcolumn = c()
  for (i in 1:nrow(data))
    newcolumn = c(newcolumn, match(data[i,'GarageFinish'],group[,1]))
  
  data[,'GarageFinish'] = newcolumn
  
  
  return(data)
}

