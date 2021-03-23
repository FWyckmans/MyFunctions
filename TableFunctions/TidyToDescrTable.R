# remove(list = ls())
# 
# Index <- rep(c(1:10), each = 2)
# Intra <- rep(c("T1", "T2"), times = 10)
# Inter <- rep(c("PG", "HC"), each = 10)
# Value1 <- c(101:120)
# Value2 <- c(201:220)
# d <- data.frame(Index, Intra, Inter, Value1, Value2)
# 
# TidyToDescrTable <- function(){
#   library(dplyr)
# }
# 
# VoI <- c("Value1", "Value2")
# dDescr <- d%>%
#   group_by("Inter", Intra)%>%
#   summarise_at(vars(all_of(VoI)),list(M = mean, SD = sd, N = n_distinct), na.rm = T)
# 
# UInter <- unique(d[,grep("\\bInter\\b", colnames(d))])
# UIntra <- unique(d[,grep("\\bIntra\\b", colnames(d))])
# 
# df <- as.data.frame(table(UIntra, UInter, VoI))%>%
#   rename("Mean (SD)" = Freq)


for (i in VoI){
  
}