##### Import personal functions
LocationStat <- "~/Project_DS/Project_RFunction/MyFunctions/StatFunctions"

dir()

ToImport <- c("") # Insert here the function to import

for (i in length(ToImport)){
  source(ToImport[i])
}
