#teaches a model depending of its type and hyperparameter
#model_flag: 1 - tree, 2 - lazy, 3 - svm
teach_model <- function(train, model_flag, param)
{
  if (model_flag == 1)
  {
    model = tree(SalePrice~., train, control = tree.control(nobs = param[1, 'nobs'], mincut = param[1, 'mincut'], minsize = param[1, 'minsize'], mindev = param[1, 'mindev']))
  }
  else if (model_flag == 2)
  {
    if (param[1, 'distance'] == 1)
      model = lazy(SalePrice~., train, control = lazy.control(conIdPar=NULL, linIdPar=param[1, 'linIdPar'], quaIdPar=NULL, distance=c("manhattan"), metric=NULL, cmbPar=param[1, 'cmbPar'], lambda=param[1, 'lambda']))
    else
      model = lazy(SalePrice~., train, control = lazy.control(conIdPar=NULL, linIdPar=param[1, 'linIdPar'], quaIdPar=NULL, distance=c("euclidean"), metric=NULL, cmbPar=param[1, 'cmbPar'], lambda=param[1, 'lambda']))
  }
  else if (model_flag == 3)
  {
    model = svm(SalePrice~., train, degree=param[1, 'degree'], nu=param[1, 'nu'], cachesize = 100, tolerance=param[1, 'tolerance'], epsilon=param[1, 'epsilon'])
  }
  return(model)
}