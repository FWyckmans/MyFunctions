InsertCol <- function(d, ToInsert, pos, name = "InsertedCol"){
  
  # Convert pos to column index
  if (is.character(pos)){
    x <- paste0("\\b", pos, "\\b")
    pos <- grep(x, colnames(d))+1
  }
  
  # Subset dataframe before and after the column of interest
  dt1 <- d[,1:(pos-1)] # Before
  cn1 <- colnames(d[1:(pos-1)]) # Names before
  
  dt2 <- d[,pos:length(d)] # After
  cn2 <- colnames(d[pos:length(d)]) # Names after
  
  
  d <- cbind(dt1, ToInsert, dt2) # bind
  
  colnames(d)[1:(pos-1)] <- cn1 # Rename columns before (necessary if only one column before)
  colnames(d)[(pos+1):length(d)] <- cn2 # Rename columns after (necessary if only one column after)
  colnames(d)[pos] <- name # Rename column of interest
  
  return(d)
}

# a <- c(1, 2, 3)
# b <- c("l", "n", "m")
# ab <- c(4, 5, 6)
# d <- data.frame(a, b, ab)
# 
# c <- c(7, 8, 9)
# 
# d <- InsertCol(d, c, "b")
# d
