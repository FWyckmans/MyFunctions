OutliersModif <- function(d, Columns, Groups = F, Within = F, Proxy = "MAD", mult = 3, as = NA){   # Give the dataFrame, the indexes of the columns of interest as vector, the index of the column indicating the group
  ## Function used to analyse and modify outliers based on the MAD or the SD.
  
  # Several arguments:
  # d: the dataframe (tidied up!)
  # Columns: the name or the index of the column you want to modify
  # Groups: The name or the index of the columns corresponding to the between subject variable
  # Proxy: "MAD" if you find outliers based on the median and MAD
  #        "SD" if you find outliers based on mean and SD.
  # Mult: the multiplier you want to apply to the MAD/SD
  # as: What you want to replace the outlier with (default = NA)
  # Within: The names or the index of the column corresponding to the within subject variable
  
  ## Additionnal requirment
  # This function requires dplyr to work
  if(!require(dplyr)){install.packages('dplyr')}
  if(!require(tidyr)){install.packages('tidyr')}
  library(dplyr)
  library(tidyr)
  
  # Transformation from colnames to index function
  FromColNameToIndex <- function(d, CoI){
    compt = 1
    Indices <- c()
    for (i in CoI){
      coln <- paste0("\\b", i, "\\b")
      index <- grep(coln, colnames(d))
      
      Indices[compt] <- index
      compt = compt + 1
    }
    Indices
  }
  
  AddDummyCol <- function(d, ToAdd, Val = NA){
    for (i in ToAdd) {
      Vect <- rep(Val, length(d[[1]]))
      d <- cbind(d, Vect)
      colnames(d)[colnames(d) == "Vect"] <- i
    }
    return(d)
  }
  
  ##### Main function to remove outliers for each column of interest
  RemoveOutl <- function(d, ...){
    Gr = unique(d[[Groups]])
    dF <- data.frame()
    for (g in c(1:length(Gr))){
      dTemp <- filter(d, d[,Groups] == Gr[g])
      
      for (i in CoI){
        vect <- dTemp[[i]]
        Mdn <- median(vect, na.rm=T)
        SD <- sd(vect, na.rm=T)
        Mad = mad(vect, na.rm=T)
        
        if (Proxy == "MAD"){
          min = Mdn - mult*Mad
          max = Mdn + mult*Mad
        }
        if (Proxy == "SD"){
          min = Mdn - mult*SD
          max = Mdn + mult*SD
        }
        
        vect[vect<min] = as
        vect[vect>max] = as
        dTemp[,i] <- vect
      }
      # # Print outliers
      # ind <- match(as, dTemp)
      # for (i in 1:length(ind)) {
      #   cat(paste0("NS ", ind[i], " in group", Gr[g], " is an outlier for ", colnames(dTemp), "\n"))
      # }
      
      dF <- rbind(dF, dTemp)
    }
    return(dF)
  }
  
  # Add dummy col for groups if no groups
  dummyAdded = 0
  if (Groups[1] == F){
    dummyAdded = 1
    d <- AddDummyCol(d, ToAdd = "DummyCol", Val = "Dummy")
    Groups = length(d)
  }
  
  ##### Get indexes
  # for VoI
  CoI <- Columns
  if (is.character(CoI)){
    CoI <- FromColNameToIndex(d, CoI)
  }
  # for Btw
  if (is.character(Groups)){
    Groups <- FromColNameToIndex(d, Groups)
  }
  
  # for Within
  ## Within variable handling
  if (is.character(Within)){
    Within <- FromColNameToIndex(d, Within)
  }
  
  ## Unite columns
  # United the between-subject columns into one
  grn = length(Groups)
  MultBtwn = 0
  if(grn > 1){
    btwnName <- colnames(d[c(Groups)],)
    MultBtwn = 1
    d <- unite(d, GrCol, all_of(Groups), remove = T, sep = "_")
    Groups <- FromColNameToIndex(d, "GrCol")
    
    # Change CoI indexes to match the unite
    for(i in 1:length(CoI)){
      if(CoI[i] > Groups){
        CoI[i] <- CoI[i] - (grn-1)
      }
    }
    
    # Change Within indexes to match the unite
    if(!F %in% Within){
      for(i in 1:length(Within)){
        if(Within[i] > Groups){
          Within[i] <- Within[i] - (grn-1)
        }
      }
    }
  }
  
  # Unite the within-subject columns into one
  withn = length(Within)
  Multwithin = 0
  if(withn > 1){
    withinName <- colnames(d[c(Within)],)
    Multwithin = 1
    d <- unite(d, WithinCol, all_of(Within), remove = T, sep = "_")
    Within <- FromColNameToIndex(d, "WithinCol")
    
    # Change CoI indexes to match the unite
    for(i in 1:length(CoI)){
      if(CoI[i] > Within){
        CoI[i] <- CoI[i] - (withn-1)
      }
    }
    if(Groups > Within){
      Groups <- Groups - (withn-1)
    }
  }
  
  # Use the RemoveOutl function
  if(F %in% Within){
    df <- RemoveOutl(d)
  }
  
  if(!F %in% Within){
    # Get index for within variable
    l <- lapply(split(d,d[Within]),function(x) RemoveOutl(x))
    nF <- c(1:length(l))
    df <- data.frame()
    for (i in nF) {
      dt <- as.data.frame(l[i])
      colnames(dt) <- colnames(d)
      df <- rbind(df, dt)
    }
  }
  
  # Separate the united between variables column
  if (MultBtwn == 1){
    df <- separate(df, "GrCol", into = btwnName, sep = "_")
  }
  
  # Separate the united within variables column
  if (Multwithin == 1){
    df <- separate(df, "WithinCol", into = withinName, sep = "_")
  }
  
  # Remove the dummyCol
  if(dummyAdded == 1){
    df <- df[1:(length(df)-1)]
  }
  
  return(df)
}

# dt <- OutliersModif(dTest, c("Val2", "Val3"), Groups = c("Btwn1", "Btwn2"))
##### Function created by Florent Wyckmans