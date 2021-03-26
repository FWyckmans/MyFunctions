TestDF <- function(nRow = 80){
  # This function is used to create a test tidy df. Which is easier to prepare new function
  # nRow is the number of rows you want in your test df
  
  # Create test df
  if (nRow %% 80 != 0){
    print("nRow not divisible by 80, default value taken")
    nRow = 80
  }
  Index <- rep(c(1:(nRow/2)), each = nRow/40)
  Btwn1 <- c(rep(c("PG", "HC"), each = nRow/2))
  Btwn2 <- rep(rep(c("G1", "G2"), each = nRow/40), nRow/4)
  Wthn1 <- rep(c("T1", "T2"), nRow/2)
  Val1 <- rep(rep(c("Male", "Female"), each = nRow/20), nRow/8)
  Val2 <- c(1:nRow)
  Val3 <- c((nRow+1):nRow*2)
  
  d <- data.frame(Index, Btwn1, Btwn2, Wthn1, Val1, Val2, Val3)
  
  for (i in colnames(d)) {
    if (is.character(d[[i]])){
      d[[i]] <- as.factor(d[[i]])
    }
  }
  dDev <<- d
  
}
TestDF()
# FW