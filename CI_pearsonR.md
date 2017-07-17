Confidence intervals around Pearson r’s
================

First we use Fisher's r-to-z transformation which is as follow:

![](http://latex.codecogs.com/gif.latex?z%20%3D%200.5%20%5Cln%7B%5Cfrac%7B1+r%7D%7B1-r%7D%7D)

z is approximately normally distributed, with an expectation equal to

![](http://latex.codecogs.com/gif.latex?z%20%3D%200.5%20%5Cln%7B%5Cfrac%7B1+p%7D%7B1-p%7D%7D)

where p is the population correlation of which r is an estimate of. The standard deviation is

![](http://latex.codecogs.com/gif.latex?%5Csigma%20%3D%20%5Csqrt%7B%5Cfrac%7B1%7D%7Bn-3%7D%7D)

So we can obtain confidence intervals using

*z* ± (*z* − *v**a**l**u**e*)(*σ*)

We can turn this into a function to make use of this transformation.

``` r
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
```
