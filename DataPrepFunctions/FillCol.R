FillCol <- function(DestinationDF, SourceDF, ToFill){
  ##### Give the destination DF, the source DF and a vector with all the column names to fill in the destination DF #####
  ##### The first column MUST be the subjID (exact name) #####
  list = list(DestInd = DestinationDF[,1],
              SourcInd = SourceDF[,1])
  CommonSubj <- Reduce(intersect, list)
  # goodDest <- DestinationDF[1] == commonSubj
  # goodSourc <- SourceDF[1] == commonSubj
  SourceDF <- SourceDF%>%
    filter(.data[[colnames(SourceDF)[1]]] %in% CommonSubj)%>%
    arrange(.data[[colnames(SourceDF)[1]]])
  
  for (c in ToFill) {
    for (i in CommonSubj) {
      DestinationDF[[c]][DestinationDF[1]==i] <- SourceDF[[c]][SourceDF[1]==i]
    }
  }
  return(DestinationDF)
}