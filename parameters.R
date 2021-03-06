#contains functions for producing the model configuration sets

#creates a predefined set of hyperparameter configurations for tree model
get_parameters_tree <- function()
{
  param_tree_1 = data.frame(nobs = 1460)
  
  mincut_vector = c(1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50)
  minsize_vector = c(2:10)
  
  param_tree_2a = data.frame(mincut = mincut_vector)
  param_tree_2b = data.frame(minsize = minsize_vector)
  param_tree_2 = merge(param_tree_2a, param_tree_2b)
  param_tree_2[,'minsize'] = param_tree_2[,'minsize'] * param_tree_2[,'mincut']
  
  param_tree_3 = data.frame(mindev = c(0, 0.01, 0.05, 0.10, 0.20, 0.30, 0.50, 1.0))
  
  param_tree = merge(param_tree_1, param_tree_2)
  param_tree = merge(param_tree, param_tree_3)
  return(param_tree)
}

#creates a predefined set of hyperparameter configurations for lazy model
get_parameters_lazy <- function()
{
  param_lazy_1 = data.frame(linIdPar = c(1))
  param_lazy_2 = data.frame(distance = c(1, 2))
  param_lazy_3 = data.frame(cmbPar = c(1, 1.5, 2, 2.5, 3))
  param_lazy_4 = data.frame(lambda = c(10, 50, 100, 200, 1000))
  
  param_lazy = merge(param_lazy_1, param_lazy_2)
  param_lazy = merge(param_lazy, param_lazy_3)
  param_lazy = merge(param_lazy, param_lazy_4)
  
  return(param_lazy)
}

#creates a predefined set of hyperparameter configurations for svm model
get_parameters_svm <- function()
{
  param_svm_1 = data.frame(degree = c(3, 4, 5, 10))
  param_svm_2 = data.frame(nu = c(0.5, 0.7, 0.9, 1.0))
  param_svm_3 = data.frame(tolerance = c(0.001, 0.01, 0.1))
  param_svm_4 = data.frame(epsilon = c(0.1, 0.2, 0.5))
  
  param_svm = merge(param_svm_1, param_svm_2)
  param_svm = merge(param_svm, param_svm_3)
  param_svm = merge(param_svm, param_svm_4)
  
  return(param_svm)
}