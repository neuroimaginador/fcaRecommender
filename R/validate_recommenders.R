validate_recommenders <- function(trainset,
                                  testset,
                                  output_vars,
                                  positive_attr = output_vars[1]) {
  
  results <- data.frame(Rec = NULL,
                        Acc = NULL,
                        Sens = NULL,
                        Spec = NULL,
                        Prec = NULL,
                        Recall = NULL,
                        PPV = NULL,
                        NPV = NULL)
  
  y <- testset@data[, positive_attr]
  testset@data[, output_vars] <- 0
  
  all_recommenders <- recommenderRegistry$get_entry_names()
  real_recommenders <- grepl(pattern = "_realRatingMatrix", 
                             x = all_recommenders)
  my_recommenders <- gsub(pattern = "_realRatingMatrix", 
                          replacement = "", 
                          x = all_recommenders[real_recommenders])
  
  my_recommenders <- setdiff(my_recommenders, c("RANDOM", "RERECOMMEND"))
  
  for (rec in my_recommenders) {
    
    message("** ", rec, " **")
    
    parameter <- NULL
    
    if (rec %in% c("UBCF", "IBCF")) {
      
      parameter <- list(normalize = NULL, 
                        method="Cosine")
      
    }
    
    try({
      
      this_rec <- recommenderlab::Recommender(trainset, 
                                              method = rec, 
                                              param = parameter)
      
      P <- recommenderlab::predict(this_rec, 
                                   newdata = testset, 
                                   type = "ratingMatrix")
      y_hat <- P@data[, positive_attr] 
      y_hat[y_hat >= 0.5] <- 1
      y_hat[y_hat < 0.5] <- 0
      idx <- which(is.na(y_hat))
      
      if (length(idx) > 0) {
        
        M <- caret::confusionMatrix(data = factor(y_hat[-idx]), 
                                    reference = factor(y[-idx]),
                                    positive = "1")
        
      } else {
        
        M <- caret::confusionMatrix(data = factor(y_hat), 
                                    reference = factor(y),
                                    positive = "1")
      }
      
      tmp <- data.frame(Rec = rec,
                        Acc = M$overall["Accuracy"],
                        Sens = M$byClass["Sensitivity"],
                        Spec = M$byClass["Specificity"],
                        Prec = M$byClass["Precision"],
                        Recall = M$byClass["Recall"],
                        PPV = M$byClass["Pos Pred Value"],
                        NPV = M$byClass["Neg Pred Value"])
      
      results <- rbind(results, tmp)
      
      
    })
    
  }
  
  rownames(results) <- NULL
  
  return(results)
  
}

