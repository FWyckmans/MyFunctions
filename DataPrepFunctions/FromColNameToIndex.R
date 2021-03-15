FromColNameToIndex <- function(d, CoI){
  compt = 1
  Indices <- c()
  for (i in CoI){
    coln <- paste0("\\b", i, "\\b")
    index <- grep(coln, colnames(d))
    
    Indices[compt] <- index
    compt = compt + 1
  }
  Indices
}


##### Function created by Florent Wyckmans