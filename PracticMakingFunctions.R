fence <- function(original, wrapper) {   ##Practice one... Creating a function
  result <- c(wrapper, original, wrapper)
  return(result)
}
best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.
fence(best_practice, asterisk) 

#Practice 2... Creating functions
outside <- function(vector){
  result <- c(vector[1], vector[length(vector)])
  return(result)
}
dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
outside(dry_principle)
