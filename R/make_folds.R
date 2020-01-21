make_folds <- function(dataset, k) {
  
  folds <- k
  idx <- sample(seq(folds), 
                size = nrow(dataset), 
                replace = TRUE)
  
  trainset <- list()
  testset  <- list()
  
  for (i in seq(k)) {
    
    trainset[[i]] <- dataset[idx != i, ]
    testset[[i]]  <- dataset[idx == i, ]
    
  }
  
  return(list(train = trainset, test = testset))
  
}
