NormalitySkewKurtosis <- function(d, VoI, Groups, Format = "Large") {
  # Give the dataframe, the index of the Variable of Interest and the index of the group column
  # Indicate if you want a between-subject comparison
  if (!(Format %in% c("Large", "Long"))){
    stop("Unsupported Format")
  }
  
  cn <- colnames(d)
  ##### Needed packages
  if(!require(dplyr)){install.packages('dplyr')}
  library(dplyr)
  if(!require(tidyr)){install.packages('tidyr')}
  library(tidyr)
  
  ##### Needed functions
  spssSkewKurtosis=function(x) {
    w=length(x)
    m1=mean(x, na.rm = T)
    m2=sum((x-m1)^2, na.rm = T)
    m3=sum((x-m1)^3, na.rm = T)
    m4=sum((x-m1)^4, na.rm = T)
    s1=sd(x, na.rm = T)
    skew=w*m3/(w-1)/(w-2)/s1^3
    sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
    kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
    sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
    mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
               dimnames=list(c("skew","kurtosis"), c("estimate","se")))
    return(mat)
  }
  
  ##### Main code
  d[[Groups]] <- factor(d[[Groups]], levels = unique(d[[Groups]]), ordered = F)
  
  Gr = unique(d[[Groups]])
  dT <- data.frame()
  for (g in c(1:length(Gr))){
    dt <- subset(d, d[[Groups]] == Gr[g])
    dTemp <- data.frame()
    n = 1
    for (i in VoI){
      vect <- dt[[i]]
      
      # Handle numeric
      if (!(is.character(vect) | is.factor(vect))){
        moy <- mean(vect, na.rm = T)
        ET <- sd(vect, na.rm = T)
        mdn <- median(vect, na.rm = T)
        dTemp[n,1] = colnames(dt[i], do.NULL = TRUE, prefix = "col")
        dTemp[n,2] = Gr[g]
        dTemp[n,3] = round(moy,digits=2)
        dTemp[n,4] = round(ET,digits=2)
        dTemp[n,5] = round(mdn, digits=2)
        dTemp[n,6] = round(spssSkewKurtosis(vect)[2]/spssSkewKurtosis(vect)[4],digits=5)
        dTemp[n,7] = round(spssSkewKurtosis(vect)[1]/spssSkewKurtosis(vect)[3],digits=5)
        dTemp[n,8] = round(spssSkewKurtosis(vect)[2],digits=5)
        dTemp[n,9] = round(spssSkewKurtosis(vect)[1],digits=5)
        dTemp[n,10] <- 'OK'
        dTemp[n,11] <- length(vect)
        if ((dTemp[n,6] < -1.96 | dTemp[n,6] > 1.96) | (dTemp[n,7] < -1.96 | dTemp[n,7] > 1.96)){
          dTemp[n,10] <- "Not_OK"
        }
        n = n+1
      }
      
      # Handle character
      if (is.character(vect) | is.factor(vect)){
        nByGroups <- function(v){
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
        
        dTemp[n,1] = colnames(dt[i], do.NULL = TRUE, prefix = "col")
        dTemp[n,2] = Gr[g]
        dTemp[n,3] = NA
        dTemp[n,4] = NA
        dTemp[n,5] = NA
        dTemp[n,6] = NA
        dTemp[n,7] = NA
        dTemp[n,8] = NA
        dTemp[n,9] = NA
        dTemp[n,10] <- "Not_OK"
        dTemp[n,11] <- nByGroups(vect)
        n = n+1
      }
    }
    
    dT <- rbind(dT, dTemp)
    
  }
  
  dDescr <- rename(dT, Variable = V1, Group = V2, Mean = V3, SD = V4, Median = V5,
                   KurtosisBYSD = V6, SkewnessBYSD = V7, Kurtosis = V8, Skewness = V9,
                   Normality = V10, N = V11)
  
  # Evaluate if normality is ok in each group
  AllNorm <- rep("OK", length(dDescr$Variable))
  dDescr <- cbind(dDescr, AllNorm)
  dDescr$AllNorm <- as.character(dDescr$AllNorm)
  for (i in unique(dDescr$Variable)) {
    if ("Not_OK" %in% dDescr$Normality[dDescr$Variable == i]){
      dDescr$AllNorm[dDescr$Variable == i] <- "Not_OK"}
  }
  
  dDescrLarge <- arrange(dDescr, Variable)
  # dDescrLarge
  if (Format == "Large"){
    return(dDescrLarge)}
  
  if (Format == "Long"){
    dMean <- dT%>%
      select(c(1, 2, 3))%>%
      spread(key = 2, value = 3)
    dSD <- dT%>%
      select(c(1, 2, 4))%>%
      spread(key = 2, value = 3)
    dN <- dT%>%
      select(c(1, 2, 11))%>%
      spread(key = 2, value = 3)
    dMedian <- dT%>%
      select(c(1, 2, 5))%>%
      spread(key = 2, value = 3)
  
    dDescr2 <- cbind(dMean, dSD[,c(2:3)], dN[,c(2:3)])
    colnames(dDescr2) <- c("Variable", paste0("Mean_", colnames(dDescr2)[2]), paste0("Mean_", colnames(dDescr2)[3]),
                           paste0("SD_", colnames(dDescr2)[4]), paste0("SD_", colnames(dDescr2)[5]),
                           paste0("N_", colnames(dDescr2)[6]), paste0("N_", colnames(dDescr2)[7]))
    # return(dDescr2)
    
    
    # Creation of dDescrLong
    Variable <- dDescr2$Variable
    Test <- rep("", length(Variable))
    dDescrLong <- data.frame(Variable, Test)
    # G = unique(dDescr$Group)
    
    for (i in 1:g) {
      Value <- rep("", length(Variable))
      dDescrLong <- cbind(dDescrLong, Value)
      n = sum(d[Groups]==levels(Gr)[i])
      colnames(dDescrLong)[length(dDescrLong)] <- paste0(levels(Gr)[i], " (n = ", n, ")")
    }
    dDescrLong <- dDescrLong[, c(1, 3:length(dDescrLong), 2)]
    
    for (i in dDescr2$Variable) {
      if (g == 2){
        if ("OK" %in% dDescr$Normality[dDescr$Variable == i]){
          # Display mean (SD) | Median
          compt = 2
          for (grpe in levels(Gr)) {
            M = sum(subset(dMean, V1 == i)[grpe])
            sd = sum(subset(dSD, V1 == i)[grpe])
            Mdn = sum(subset(dMedian, V1 == i)[grpe])
            
            Display <- paste0(M, " (", sd, ") | Mdn = ", Mdn)
            
            dDescrLong[dDescrLong$Variable == i, compt] <- Display
            
            compt = compt+1
          }
          
          # Test difference
          Test <- t.test(d[i][d[Groups]==levels(Gr)[1]], d[i][d[Groups]==levels(Gr)[2]])
          ddl = round(Test$parameter, digits = 2)
          t = round(abs(Test$statistic), digits = 2)
          p = round(Test$p.value, digits = 3)
          
          if (p >= 0.05){
            Display <- paste0("t(", ddl, ") = ", t, ", p = ", p) #, ", d = ", "d")
          }
          
          if ((p > 0) & (p < 0.05)){
            Display <- paste0("t(", ddl, ") = ", t, ", p = ", p, "*") #, ", d = ", "d")
          }
          
          if (p == 0){
            Display <- paste0("t(", ddl, ") = ", t, ", p < 0.001*") #, ", d = ", "d")
          }
          
          if (p == 1){
            Display <- paste0("t(", ddl, ") = ", t, ", p > 0.99") #, ", d = ", "d")
          }
          
          dDescrLong$Test[dDescrLong$Variable == i] <- Display
        }
        
        if (("Not_OK" %in% dDescr$Normality[dDescr$Variable == i]) & (F %in% is.na(dDescr$Mean[dDescr$Variable == i]))){
          # Display mean (SD) | Median
          compt = 2
          for (grpe in levels(Gr)) {
            M = sum(subset(dMean, V1 == i)[grpe])
            sd = sum(subset(dSD, V1 == i)[grpe])
            Mdn = sum(subset(dMedian, V1 == i)[grpe])
            Display <- paste0(M, " (", sd, ") | Mdn = ", Mdn)
            dDescrLong[dDescrLong$Variable == i, compt] <- Display
            compt = compt+1
          }
          
          # Test difference
          Test <- wilcox.test(d[i][d[Groups]==levels(Gr)[1]], d[i][d[Groups]==levels(Gr)[2]])
          w = round(Test$statistic, digits = 2)
          p = round(Test$p.value, digits = 3)
          ddl = sum(as.numeric(dDescr$N[dDescr$Variable == i]))
          
          
          if (p >= 0.05){
            Display <- paste0("U(", ddl, ") = ", w, ", p = ", p)
          }
          
          if ((p > 0) & (p < 0.05)){
            Display <- paste0("U(", ddl, ") = ", w, ", p = ", p, "*")
          }
          
          if (p == 0){
            Display <- paste0("U(", ddl, ") = ", w, ", p < 0.001*")
          }
          
          if (p == 1){
            Display <- paste0("U(", ddl, ") = ", w, ", p > 0.99 ")
          }
          
          dDescrLong$Test[dDescrLong$Variable == i] <- Display
        }
        
        if (("Not_OK" %in% dDescr$Normality[dDescr$Variable == i]) & (T %in% is.na(dDescr$Mean[dDescr$Variable == i]))){
          # Display N (...) | N (...)
          compt = 2
          for (grpe in levels(Gr)) {
            
            
            Display <- as.character(subset(dN,V1 == i)[grpe])
            dDescrLong[dDescrLong$Variable == i, compt] <- Display
            compt = compt+1
          }
          
          # Test difference
          Test <- chisq.test(d[[i]], d[[Groups]])
          X2 = round(Test$statistic, digits = 2)
          p = round(Test$p.value, digits = 3)
          ddl = round(Test$parameter, digits = 3)
          
          if (p >= 0.05){
            Display <- paste0("X2(", ddl, ") = ", X2, ", p = ", p)
          }
          
          if ((p > 0) & (p < 0.05)){
            Display <- paste0("X2(", ddl, ") = ", X2, ", p = ", p, "*")
          }
          
          if (p == 0){
            Display <- paste0("X2(", ddl, ") = ", X2, ", p < 0.001 ")
          }
          
          if (p == 1){
            Display <- paste0("X2(", ddl, ") = ", X2, ", p > 0.99 ")
          }
          
          dDescrLong$Test[dDescrLong$Variable == i] <- Display
        }
      }
    }
    # x <- colnames(d)
    dDescrLong <- dDescrLong %>%
      slice(match(cn, Variable))
    
    # dDescr %>%
    #   slice(match(x, Variable))

    return(dDescrLong)
  }
}