generate_subjects <- function(n,
                              dataset,
                              method = c("stats", "valid"),
                              parameters = list()) {
  
  stats_subjects <- NULL
  valid_subjects <- NULL
  
  if ("stats" %in% method) {
    
    Sigma <- cov(dataset)
    mu <- colMeans(dataset)
    
    S <- as(Matrix::nearPD(Sigma)$mat, "matrix")

    stats_subjects <- tmvtnorm::rtmvt(n = n,
                    sigma = S,
                    mean = mu,
                    lower = rep(0, length(mu)),
                    upper = rep(1, length(mu)),
                    algorithm = "gibbs")
    
    colnames(stats_subjects) <- colnames(dataset)
    
  }
  
  if ("valid" %in% method) {
    
    if (!is.null(parameters$implications)) {
      
      grades_set <- parameters$grades_set
      number_seed <- parameters$number_seed
      forbidden_attrs <- parameters$forbidden_attrs
      implications <- parameters$implications
      
      attributes <- implications$get_attributes()
      names(grades_set) <- attributes

      valid_subjects <- matrix(0, 
                               nrow = n, 
                               ncol = length(attributes))
      valid_attrs <- setdiff(attributes, forbidden_attrs)
      
      print(valid_attrs)
      
      for (i in seq(n)) {
      
        my_attributes <- sample(valid_attrs, number_seed)  
        values <- simplify2array(lapply(grades_set[my_attributes], function(x) sample(x[x > 0], 1)))
        
        S <- SparseSet$new(attributes = attributes)
        S$assign(attributes = my_attributes, values = values)
        
        A <- implications$closure(S)
        valid_subjects[i, ] <- as.matrix(A$closure$get_vector())
        
      }
      
      colnames(valid_subjects) <- attributes
      
    }
    
  }
  
  return(list(stats = stats_subjects,
              valid = valid_subjects))
  
}
