#model script must be run before this in order to get result_tree, result_lazy, result_svm


# tree

group = aggregate(result_tree[,c('rmse')], list(result_tree$nobs), mean) #no affected
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$mincut), mean)  # 10
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$minsize), mean) #~20-180
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$mindev), mean) #0.5
plot(group[,1],group[,2])





# lazy

group = aggregate(result_lazy[,c('rmse')], list(result_lazy$cmbPar), mean)  #3
plot(group[,1],group[,2])

group = aggregate(result_lazy[,c('rmse')], list(result_lazy$lambda), mean)  #200
plot(group[,1],group[,2])



# degree


group = aggregate(result_svm[,c('rmse')], list(result_svm$degree), mean)  #no affected
plot(group[,1],group[,2])

group = aggregate(result_svm[,c('rmse')], list(result_svm$nu), mean)  #no affected
plot(group[,1],group[,2])

group = aggregate(result_svm[,c('rmse')], list(result_svm$tolerance), mean)   #0.001
plot(group[,1],group[,2])

group = aggregate(result_svm[,c('rmse')], list(result_svm$epsilon), mean)  #0.1
plot(group[,1],group[,2])
