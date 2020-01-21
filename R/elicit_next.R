elicit_next <- function(subject,
                        attributes, 
                        precedence, 
                        already_present,
                        mode = "absolute") {
  
  my_precedences <- precedence[setdiff(attributes[as.matrix(subject$get_vector()) > 0], already_present)]
  
  if (mode == "absolute") {
    
    names(which.max(my_precedences))
    
  } else {
    
    x <- runif(1)
    cum_prec <- c(0, cumsum(my_precedences / sum(my_precedences)))
    
    names(my_precedences[which(cum_prec >= x)[1] - 1])
    
  }
  
}

# attributes <- fc2$attributes
# precedence <- runif(n = length(attributes))
# names(precedence) <- attributes
# already_present <- sample(attributes, 3)
# 
# elicit_next(attributes, precedence, already_present)

