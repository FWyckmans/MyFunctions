##### Import personal functions
ToImport <- "TestFunction"

Importer <- function(ToImport, TypeFun = "Stat", FoldLocation = "C:/Users/fwyck/OneDrive/Documents/Project_DS/Project_R_Function/MyFunctions"){

## Type of functions  
    if (TypeFun == "Stat"){
    Location <- paste0(FoldLocation, "/StatFunctions/")
  }
  
  if (TypeFun == "Graph"){
    Location <- paste0(FoldLocation, "/GraphFunctions/")
  }

## Actual importer  
  for (i in length(ToImport)){
    Import <- paste0(Location, "/", ToImport[i], ".R")
    source(Import)
  }
}

# FW