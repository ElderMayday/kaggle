#model script must be run before this in order to get result_tree, result_lazy, result_svm


# tree

group = aggregate(result_tree[,c('rmse')], list(result_tree$nobs), mean) #no affect
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$mincut), mean)
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$minsize), mean)
plot(group[,1],group[,2])

group = aggregate(result_tree[,c('rmse')], list(result_tree$mindev), mean)
plot(group[,1],group[,2])
