conversation <- function(subject, 
                         implications,
                         precedence,
                         critiquing = "compound",
                         class_attrs) {
  
  # Real value of subject class:
  y <- subject[class_attrs]
  # Remove class data in the subject
  subject$assign(attributes = class_attrs, values = rep(0, length(class_attrs)))
  
  # Attributes
  attributes <- implications$get_attributes()
  
  # Repeat until getting the class attribute in the closure
  # Or the closure remains unchanged in two iterations
  ok <- TRUE
  used_attrs <- c()
  partial_subject <- SparseSet$new(attributes = attributes)
  while (ok) {
    
    # Select new attribute
    new_attribute <- elicit_next(subject = subject,
                                 attributes = attributes, 
                                 precedence = precedence, 
                                 already_present = used_attrs)
    new_value <- as.matrix(subject$get_vector())[match(new_attribute, attributes)]
    partial_subject$assign(attributes = new_attribute,
                           values = new_value)
    
    # Compute closure
    tmp <- implications$closure(partial_subject, reduce = TRUE)
    closure <- tmp$closure
    implications <- tmp$implications
    
    # Critique
    partial_subject <- critique(Sclosure = closure, 
                                Sreal = subject, 
                                type = critiquing)
    
    # If critique, new closure
    if (critiquing != "none") {
      
      tmp <- implications$closure(partial_subject, reduce = TRUE)
      partial_subject <- tmp$closure
      implications <- tmp$implications      
      
    }
    
    # 
    used_attrs <- c(used_attrs, attributes[as.matrix(partial_subject$get_vector()) > 0])
    used_attrs <- unique(used_attrs)
    
    # Annotate step
    
    # Convergence test
    ok <- all(is.na(match(class_attrs, used_attrs)))
    ok <- ok & !(is.null(implications))
    ok <- ok & !implications$is_empty()
    
  }
  
  # Check if the predicted class is the same as the real class
  correct_class <- all(as.matrix(partial_subject[class_attrs]$get_vector()) ==  as.matrix(y$get_vector()))
  
  
}
