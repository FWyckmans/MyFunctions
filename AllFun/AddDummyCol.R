AddDummyCol <- function(d, ToAdd, Val = NA){
  ##### Give the df you want to add dummy columns to and the column names #####
  for (i in ToAdd) {
    Vect <- rep(Val, length(d[[1]]))
    d <- cbind(d, Vect)
    colnames(d)[colnames(d) == "Vect"] <- i
  }
  return(d)
}

# FW