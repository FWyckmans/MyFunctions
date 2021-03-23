##### Import personal functions
# Meta-function I copy paste in any other script to import functions in these folders
# I update it each time I add a new functions folder

# ToImport: Insert the name of the function (without .R).
# TypeFun: Stat, Graph, DataPrep or Table at the moment.
# FoldLocation: Default is my folder location, change it with yours.

Importer <- function(FunctionPath = "AllFun/"){
  for (Fun in dir(FunctionPath)) {
    source(paste0(FunctionPath, Fun))
  }
}
# FW