library(FSelector)   #load the feature-selection library

setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!

#takes dataframe and shows the features with satisfactory information gain
feature_selector <- function(data)
{
  #calculate the information gain of each feature
  features <- information.gain(SalePrice~., data)
  
  #result dataframe
  result = data.frame()
  
  row_names = row.names(features)
  
  #select every feature with IF higher than 0.1
  for (i in 1:nrow(features))
  {
    if (features[i, 1] > 0.1)
    {
      result <- rbind(result, data.frame("feature_name"= row_names[i], "feature_gain" = features[i, 1]))
    }
  }

  #print the selected features
  print(result)
  
  return(result)
}

#load the train data
train = read.csv("./train.csv", header = TRUE)

#do feature selection
features = feature_selector(train)

