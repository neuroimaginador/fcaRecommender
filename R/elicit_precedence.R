# Set attribute precedence in the ellicitation stage
elicit_precedence <- function(dataset, class_attrs,
                              subject, type) {
  
  params <- elicit_params(dataset, class_attrs, type)

  prec <- switch(tolower(type),
         "random" = runif(n = subject$length()),
         "zscore" = (as.matrix(subject$get_vector()) - params$mu) / params$sigma,
         "importance" = params,
         "knowledge" = params
  )
  
  names(prec) <- colnames(dataset)
  
  return(prec)
  
}


