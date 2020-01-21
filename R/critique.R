critique <- function(Sclosure, Sreal, type = "compound") {
  
  v <- as.matrix(Sclosure$get_vector())
  w <- as.matrix(Sreal$get_vector())
  
  idx <- which((w > v) & (as.matrix(v) > 0))
  
  newS <- Sclosure$clone()
  
  if ((type != "none") && (length(idx) > 0) ) {
    
    if (type == "unit") {
      
      myid <- sample(idx, 1)
      v[myid] <- w[myid]
      newS <- SparseSet$new(Sclosure$get_attributes(), M = v)
      
    } else {
      
      v[idx] <- w[idx]
      newS <- SparseSet$new(Sclosure$get_attributes(), M = v)
      
    }
    
  }
  
  return(newS)
  
}
