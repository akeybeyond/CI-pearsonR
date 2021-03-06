#Fuctions for using Fisher's r to z transformation in order to create confidence intervals.


rtoz <- function(pearson_r){
  0.5*log((1+pearson_r)/(1-pearson_r))
  
}

rtoz_ci <- function(pearson_r, n, z_value){
  r = rtoz(pearson_r)
  sd = sqrt(1/(n-3))
  ci_low = round(r - sd*z_value,2)
  ci_high = round(r + sd*z_value,2)
  ci = c(ci_low, ci_high)
  return(ci)
}