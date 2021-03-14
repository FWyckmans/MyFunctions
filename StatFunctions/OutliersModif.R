OutliersModif <- function(d, Columns, Groups, Proxy = "MAD", mult = 3, as = NA, Within = F){   # Give the dataFrame, the indexes of the columns of interest as vector, the index of the column indicating the group
  
  ## Additionnal requirment
  # This function requires dplyr to work
  if(!require(dplyr)){install.packages('dplyr')}
  library(dplyr)
  
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
  
  # Get index for VoI
  CoI <- Columns
  if (is.character(Columns)){
    CoI <- FromColNameToIndex(d, Columns)
  }
  
  # Get index for btwn variable column
  if (is.character(Groups)){
    Groups <- FromColNameToIndex(d, Groups)
  }
  
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
      dF <- rbind(dF, dTemp)
    }
    return(dF)
  }
  
  if(Within == F){
    df <- RemoveOutl(d)
  }
  
  if(!Within == F){
    # Get index for within variable
    # Not supported yet
    if (is.character(Within)){
      Within <- FromColNameToIndex(d, Within)
    }
    l <- lapply(split(d,d[Within]),function(x) RemoveOutl(x))
    nF <- c(1:length(l))
    df <- data.frame()
    for (i in nF) {
      dt <- as.data.frame(l[i])
      colnames(dt) <- colnames(d)
      df <- rbind(df, dt)
    }
  }
  df
}

# Index <- rep(c(1:10), times = 2)
# Intra <- rep(c("T1", "T2"), each = 10)
# Inter <- rep(c("PG", "HC"), times = 10)
# Value1 <- c(101:110, 2000, 102, 2001, 103, 2002, 101, 2003, 101, 2004, 2000)
# Value2 <- c(201:209, 2000, 2001:2010)
# d <- data.frame(Index, Intra, Inter, Value1, Value2)
# dTest <- OutliersModif(d, c("Value1", "Value2"), Groups = 3, Proxy = "MAD", Within = 2)
# dTest <- OutliersModif(d, c("Value1", "Value2"), Groups = 3, Proxy = "MAD", Within = F)

##### Function created by Florent Wyckmans