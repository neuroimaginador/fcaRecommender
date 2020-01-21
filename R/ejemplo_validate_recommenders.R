k <- 5
folds <- make_folds(dataset = cobre32, k = k)

results <- data.frame(fold = NULL,
                      Rec = NULL,
                      Acc = NULL,
                      Sens = NULL,
                      Spec = NULL,
                      Prec = NULL,
                      Recall = NULL,
                      PPV = NULL,
                      NPV = NULL)

for (i in seq(k)) {
  
  trainset <- as(folds$train[[i]], "realRatingMatrix")
  testset <- as(folds$test[[i]], "realRatingMatrix")
  
  res <- validate_recommenders(trainset = trainset,
                               testset = testset,
                               output_vars = c("dx_ss", "dx_other"),
                               positive_attr = "dx_ss")
  
  tmp <- cbind(data.frame(fold = k), res)
  
  results <- rbind(results, tmp)
  
}

dataset <- as.matrix(t(fc2$I))
implications <- fc2$implications$clone()
supp <- implications$support()
implications <- implications[-which(supp == 0)]

S <- generate_subjects(n = 50, dataset = dataset,
                       parameters = list(implications = implications, 
                                         grades_set = fc2$expanded_grades_set,
                                         number_seeds = 3, 
                                         forbidden_attrs = c("dx_ss", "dx_other")))

res <- validate_recommenders(trainset = as(dataset, "realRatingMatrix"),
                             testset = as(S$valid, "realRatingMatrix"),
                             output_vars = c("dx_ss", "dx_other"),
                             positive_attr = "dx_ss")

                             
