DescrFrame <- function(d, Btwn = NA){
  ########## Init
  #### Required packages
  packages <- c("dplyr")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  #### Required functions
  # Add Dummy Col
  AddDummyCol <- function(d, ToAdd, Val = NA){
    ##### Give the df you want to add dummy columns to and the column names #####
    for (i in ToAdd) {
      Vect <- rep(Val, length(d[[1]]))
      d <- cbind(d, Vect)
      colnames(d)[colnames(d) == "Vect"] <- i
    }
    return(d)
  }
  
  # From colname to Index
  FromColNameToIndex <- function(d, CoI){
    compt = 1
    Indices <- c()
    for (i in CoI){
      coln <- paste0("\\b", i, "\\b")
      index <- grep(coln, colnames(d))
      
      Indices[compt] <- index
      compt = compt + 1
    }
    as.integer(Indices)
  }
  
  # Compute Mean (SD)
  MeanSD <- function(v, digits = 2, na.rm = T, ...){
    m <- mean(v, na.rm = na.rm)
    sd <- sd(v, na.rm = na.rm)
    output <- paste0("Mean = ", round(m, digits), " (SD = ", round(sd, digits), ")")
    return(output)
  }
  
  # Compute n
  nPresAbs <- function(v, ...){
    if (!is.factor(v)){
      v <- as.factor(v)
    }
    levelname <- levels(v)
    nl <- length(levelname)
    n = length(v[!is.na(v)])
    nbylevel <- ""
    for (i in levelname) {
      output <- paste0(i, " (n = ", length(v[v==i]) ,")")
      nbylevel <- substring(paste(nbylevel, output, sep = " | "), 2)
    }
    return(nbylevel)
  }
  
  ##### Specific function
  # Compute mean or n according to the type of subject
  FinalFrame <- function(d){
    Descriptive <- rep(NA, each = (length(d)-1))
    for (i in 1:(length(d)-1)) {
      if (is.factor(d[[i]])){
        Descriptive[i] <- nPresAbs(d[[i]])
      }
      
      if (!is.factor(d[[i]])){
        Descriptive[i] <- MeanSD(d[[i]])
      }
    }
    return(Descriptive)
  }
  
  ########## Main code
  ##### Factorize character columns
  for (i in 1:length(d)) {
    if(is.character(d[[1,i]])){
      d[[i]] <- as.factor(d[[i]])
    }
  }
  
  ##### Get the index of the between subject column
  d <- AddDummyCol(d, "Dummy", "Every Subject")
  if (is.na(Btwn)){
    Btwn <- FromColNameToIndex(d, "Dummy")
  }
  
  if (is.character(Btwn)){
    Btwn <- FromColNameToIndex(d, Btwn)
  }
  
  ##### Prepare the new descriptive frame
  dDescr <- data.frame(c(colnames(d[1:(length(d)-1)])))
  colnames(dDescr)[1] <- "Variable"
  
  ##### Fill the descriptive frame by group
  for (i in levels(d[[Btwn]])){
    dt <- filter(d, d[[Btwn]] == i)
    dT <- FinalFrame(dt)
    dDescr <- cbind(dDescr, dT)
    colnames(dDescr)[length(dDescr)] <- paste0(i,
                                               " (n = ",
                                               length(d[[Btwn]][d[[Btwn]] == i]),
                                               ")")
  }
  return(dDescr)
}


##### Function created by Florent Wyckmans