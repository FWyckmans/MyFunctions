Get95CI <- function(d, VoI, GoI, Grp){
  dt <- subset(d, d[GoI] == Grp)
  error <- qt(0.975,df=length(dt[[VoI]])-1)*sd(dt$Abstinence_2semaines)/sqrt(length(dt$Abstinence_2semaines))
  M = mean(dt$Abstinence_2semaines)
  left <- M-error
  right <- M+error
  CI <- paste0(round(100*M, digits = 2), "% [95% CI = ", round(100*left, digits = 2), ", ", round(100*right, digits = 2), "]")
  CI
}