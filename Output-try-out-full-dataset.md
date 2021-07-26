Output try-out full dataset
================
Anne Margit
07/26/2021

> I only selected participants with 3 completed assessments or more

``` r
library(lcmm)
```

    ## Warning: package 'lcmm' was built under R version 4.0.5

    ## Loading required package: survival

    ## Loading required package: parallel

    ## Loading required package: mvtnorm

    ## Warning: package 'mvtnorm' was built under R version 4.0.3

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(survival)
library(parallel)
```

``` r
load("lcga1.Rdata")
load("lcga2.Rdata")
load("lcga3.Rdata")
load("lcga4.Rdata")
```

``` r
summarytable(lcga1, lcga2, lcga3, lcga4)
```

    ##       G    loglik npm      BIC   %class1   %class2  %class3  %class4
    ## lcga1 1 -151034.8   3 302097.9 100.00000                            
    ## lcga2 2 -135471.0   6 270998.7  67.55842 32.441577                  
    ## lcga3 3 -129694.5   9 259474.1  15.79196 45.613345 38.59470         
    ## lcga4 4 -127206.6  12 254526.6  23.47156  7.726808 41.66339 27.13825

``` r
summary(lcga2)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 2, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 12709 
    ##      Number of observations: 71942 
    ##      Number of observations deleted: 26257 
    ##      Number of latent classes: 2 
    ##      Number of parameters: 6  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  1 
    ##      Convergence criteria: parameters= 6.6e-12 
    ##                          : likelihood= 1.3e-07 
    ##                          : second derivatives= 9e-08 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -135471.01  
    ##      AIC: 270954.02  
    ##      BIC: 270998.72  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  0.65348 0.02352  27.782 0.00000
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  7.48870 0.01233 607.350 0.00000
    ## intercept class2  4.76400 0.01901 250.595 0.00000
    ## days class1       0.00032 0.00006   4.935 0.00000
    ## days class2      -0.00062 0.00010  -6.357 0.00000
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.45542 0.00394

``` r
summary(lcga3)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 3, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 12709 
    ##      Number of observations: 71942 
    ##      Number of observations deleted: 26257 
    ##      Number of latent classes: 3 
    ##      Number of parameters: 9  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  5 
    ##      Convergence criteria: parameters= 3.6e-08 
    ##                          : likelihood= 9.7e-06 
    ##                          : second derivatives= 1.6e-10 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -129694.54  
    ##      AIC: 259407.08  
    ##      BIC: 259474.13  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1 -0.93884 0.03596 -26.105 0.00000
    ## intercept class2  0.12124 0.02718   4.460 0.00001
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  3.79990 0.02676 142.011 0.00000
    ## intercept class2  6.18722 0.01867 331.424 0.00000
    ## intercept class3  8.02920 0.01522 527.551 0.00000
    ## days class1      -0.00092 0.00013  -6.899 0.00000
    ## days class2      -0.00012 0.00008  -1.542 0.12301
    ## days class3       0.00043 0.00007   5.810 0.00000
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.28388 0.00353

``` r
summary(lcga4)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 4, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 12709 
    ##      Number of observations: 71942 
    ##      Number of observations deleted: 26257 
    ##      Number of latent classes: 4 
    ##      Number of parameters: 12  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  7 
    ##      Convergence criteria: parameters= 8e-11 
    ##                          : likelihood= 1.3e-08 
    ##                          : second derivatives= 1.1e-13 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -127206.61  
    ##      AIC: 254437.23  
    ##      BIC: 254526.63  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1 -0.13008 0.03897  -3.337 0.00085
    ## intercept class2 -1.23412 0.04781 -25.813 0.00000
    ## intercept class3  0.37006 0.03418  10.826 0.00000
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  5.23719 0.02493 210.047 0.00000
    ## intercept class2  3.09753 0.03318  93.365 0.00000
    ## intercept class3  6.81441 0.02081 327.511 0.00000
    ## intercept class4  8.30841 0.01832 453.561 0.00000
    ## days class1      -0.00070 0.00011  -6.606 0.00000
    ## days class2      -0.00101 0.00017  -5.883 0.00000
    ## days class3       0.00009 0.00008   1.105 0.26928
    ## days class4       0.00046 0.00009   5.389 0.00000
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.20901 0.00337
