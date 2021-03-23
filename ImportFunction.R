##### Import personal functions
# Meta-function I copy paste in any other script to import functions in these folders
# I update it each time I add a new functions folder

# FoldLocation: AllFun folder location.
# nRow: number of rows in the test df

Importer <- function(FunctionPath = "AllFun/", nRow = 80){
  # Import functions
  for (Fun in dir(FunctionPath)) {
    source(paste0(FunctionPath, Fun))
  }
  
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
  dtest <<- d
  
}

Importer()

# FW