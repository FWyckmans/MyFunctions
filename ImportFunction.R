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
  TestDF()
  
}

Importer()

# FW