# Parameters for elicitation
#' @importFrom fcaR FormalContext 
#' @importFrom caret filterVarImp
elicit_params <- function(dataset, 
                          class_attrs, 
                          type = "zscore") {
  
  if (type == "zscore") {
    
    mu <- colMeans(dataset)
    mu[class_attrs] <- 0
    sigma <- apply(dataset, 2, sd)
    sigma[class_attrs] <- 1
    
    return(list(mu = mu, sigma = sigma))
    
  }
  
  if (type == "importance") {
    
    x <- dataset[, !(colnames(dataset) %in% class_attrs)]
    y <- factor(dataset[, class_attrs[1]])
    
    importance <- vector(mode = "numeric", 
                         length = ncol(dataset)) 
    names(importance) <- colnames(dataset)
    
    res <- filterVarImp(as.data.frame(x), y)
    importance[rownames(res)] <- res[, 1]
    
    return(importance)
    
  }
  
  if (type == "knowledge") {
    
    fc <- FormalContext$new(dataset) 
    
    attributes <- fc$attributes
    res <- vector(mode = "numeric", 
                  length = length(attributes)) 
    names(res) <- attributes
    for (att in setdiff(attributes, class_attrs)) {
      
      cl <- fc$att_concept(attribute = att)
      res[att] <- cl$get_intent()$cardinal()

    }
    
    return(res)
    
  }
  
  return(NULL)
  
}
