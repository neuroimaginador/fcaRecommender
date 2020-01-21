# Parameters for elicitation
elicit_params <- function(dataset, type = "zscore") {
  
  if (type == "zscore") {
    
    mu <- colMeans(dataset)
    sigma <- apply(dataset, 2, sd)
    
    return(list(mu = mu, sigma = sigma))
    
  }
  
  if (type == "importance") {
    
    
    
  }
  
  if (type == "knowledge") {
    
    fc <- FormalContext$new(dataset) 
    
    attributes <- fc$attributes
    res <- vector(mode = "numeric", 
                  length = length(attributes)) 
    for (i in seq_along(attributes)) {
      
      cl <- fc$att_concept(attribute = attributes[i])
      res[i] <- cl$get_intent()$cardinal()

    }
    
    return(res)
    
  }
  
  return(NULL)
  
}
