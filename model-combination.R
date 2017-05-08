#performs model combination, outputs the result on test data, estimates mse on train data

library(lazy)
library(tree)
library(e1071)

setwd('D:/kaggle')  #TO-MODIFY sets the defaul folder depending on the directory path!!!
source("parameters.R")
source("feature-filter.R")

train_raw = read.csv("./train.csv", header = TRUE)
train = feature_filter(train_raw)

tree_conf_id = 6
lazy_conf_id = 2
svm_conf_id = 129

tree_parameters = get_tree_parameters()[tree_conf_id,]
lazy_parameters = get_lazy_parameters()[lazy_conf_id,]
svm_parameters = get_svm_parameters()[svm_conf_id,]

rmse_tree = 81887
rmse_lazy = 41787
rmse_svm = 87749

p1 = 1 / rmse_tree
p2 = 1/ rmse_lazy
p3 = 1/ rmse_svm

s = p1 + p2 + p3
w1 = p1 / s
w2 = p2 / s
w3 = p3 / s

tree_model = teach_model(train, 1, tree_parameters)
lazy_model = teach_model(train, 2, lazy_parameters)
svm_model = teach_model(train, 3, svm_parameters)

prediction = train[,1:(ncol(train)-1)]
predicted_tree = predict(tree_model, prediction)

prediction = train[,1:(ncol(train)-1)]
predicted_lazy = predict(lazy_model, prediction)[[1]]  #[[1]] since lazy package implementation returns the result as a list containing a vector as the first element

prediction = train[,1:(ncol(train)-1)]
predicted_svm = predict(svm_model, prediction)

predicted = predicted_tree * w1 + predicted_lazy * w2 + predicted_svm * w3


mse = mean((predicted - train[,'SalePrice'])^2)
rmse = sqrt(mse)



#-----------------------------------------------------------------

test_raw = read.csv("./test.csv", header = TRUE)
test = feature_filter_without_saleprice(test_raw)

prediction = test
predicted_tree = predict(tree_model, prediction)

prediction = test
predicted_lazy = predict(lazy_model, prediction)[[1]]  #[[1]] since lazy package implementation returns the result as a list containing a vector as the first element

typeof(2.0)

r = c()
last = 0

for (i in 1:1459)
{
  prediction = test[i, ]
  predicted_svm = predict(svm_model, prediction)
  if (TRUE)
  {
    #print(i)
    #print(predicted_svm)
    r = c(r, predicted_svm)
    if (length(r) != last + 1)
    {
      r = c(r, 200000)
    }
  }
  
  last = length(r)
}

predicted_svm2 = r
predicted_svm2 = c(predicted_svm[1:699], 200000, 200000, predicted_svm[701:1458])

predicted = predicted_tree * w1 + predicted_lazy * w2 + predicted_svm2 * w3


p = abs(predicted_svm2 - predicted_lazy)
which.max(p)
f = data.frame(s1 = p)


plot(1:1459, p)

write.table(predicted,row.names=1461:2919, file = "foo.csv", sep = ",", col.names = 'Id',
            qmethod = "double", quote = FALSE)
