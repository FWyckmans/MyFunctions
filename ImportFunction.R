##### Import personal functions
# Meta-function I copy paste in any other script to import functions in these folders
# I update it each time I add a new functions folder

# FoldLocation: AllFun folder location.

Importer <- function(FunctionPath = "AllFun/"){
  for (Fun in dir(FunctionPath)) {
    source(paste0(FunctionPath, Fun))
  }
}
# FW