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