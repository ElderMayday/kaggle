#splits the [data_ordered] dataframe into [fold_num] folds
split_folds <- function(data_ordered, fold_num=5)
{
  #permutate the datarows in a random way
  data <- data_ordered[sample(nrow(data_ordered)),]
  
  #the size of a small fold
  size = nrow(data) %/% fold_num
  
  #number of (size+1) folds
  bigfold_num = nrow(data) %% fold_num
  
  #number of (size) folds
  smallfold_num = fold_num - bigfold_num
  
  #current datarow
  current_row = 1
  
  #current fold number
  current_fold = 1
  
  #the vector of all folds
  folds = list()
  
  #bigfold partition loop
  while (current_fold <= bigfold_num)
  {
    folds[[length(folds) + 1]] = data[current_row:(current_row + size),]
    current_fold = current_fold + 1
    current_row = current_row + size + 1
  }
  
  #reset the fold counter for the remaining small folds
  current_fold = 1
  
  #smallfold partition loop
  while (current_fold <= smallfold_num)
  {
    folds[[length(folds) + 1]] = data[current_row:(current_row + size - 1),]
    current_fold = current_fold + 1
    current_row = current_row + size
  }
  
  return(folds)
}