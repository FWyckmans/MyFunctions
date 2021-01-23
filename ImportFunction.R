##### Import personal functions
# Meta-function I copy paste in any other script to import functions in these folders
# I update it each time I add a new functions folder

# ToImport: Insert the name of the function (without .R).
# TypeFun: Stat, Graph, DataCleaning or Table at the moment.
# FoldLocation: Default is my folder location, change it with yours.


Importer <- function(ToImport, TypeFun = "Stat", FoldLocation = "C:/Users/fwyck/OneDrive/Documents/Project_DS/Project_R_Function/MyFunctions"){

## Type of functions  
    if (TypeFun == "Stat"){
    Location <- paste0(FoldLocation, "/StatFunctions/")
  }
  
  if (TypeFun == "Graph"){
    Location <- paste0(FoldLocation, "/GraphFunctions/")
  }
  
  if (TypeFun == "Table"){
    Location <- paste0(FoldLocation, "/TableFunctions/")
  }
  
  if (TypeFun == "DataCleaning"){
    Location <- paste0(FoldLocation, "/DataCleaningFunctions/")
  }

## Actual importer  
  for (i in length(ToImport)){
    Import <- paste0(Location, "/", ToImport[i], ".R")
    source(Import)
  }
}

# FW