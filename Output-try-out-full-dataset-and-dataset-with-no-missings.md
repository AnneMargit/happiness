Output try-out full dataset and dataset with no missings
================
Anne Margit
08/06/2021

> Participants with 3 completed assessments or more

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
load("lcga5.Rdata")
```

``` r
summarytable(lcga1, lcga2, lcga3, lcga4,lcga5)
```

    ##       G    loglik npm      BIC   %class1   %class2  %class3  %class4  %class5
    ## lcga1 1 -151034.8   3 302097.9 100.00000                                     
    ## lcga2 2 -135471.0   6 270998.7  67.55842 32.441577                           
    ## lcga3 3 -129694.5   9 259474.1  15.79196 45.613345 38.59470                  
    ## lcga4 4 -127206.6  12 254526.6  23.47156  7.726808 41.66339 27.13825         
    ## lcga5 5 -126164.4  15 252470.6  36.92659 10.504367 18.16036 28.56244 5.846251

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

``` r
summary(lcga5)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 5, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 12709 
    ##      Number of observations: 71942 
    ##      Number of observations deleted: 26257 
    ##      Number of latent classes: 5 
    ##      Number of parameters: 15  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  21 
    ##      Convergence criteria: parameters= 1.1e-09 
    ##                          : likelihood= 6.8e-08 
    ##                          : second derivatives= 7.1e-14 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -126164.4  
    ##      AIC: 252358.8  
    ##      BIC: 252470.56  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  1.67764 0.05319  31.538 0.00000
    ## intercept class2  0.69867 0.06659  10.493 0.00000
    ## intercept class3  1.07679 0.05326  20.219 0.00000
    ## intercept class4  1.60663 0.05124  31.352 0.00000
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se    Wald p-value
    ## intercept class1  7.55870 0.02421 312.152 0.00000
    ## intercept class2  8.78223 0.02836 309.636 0.00000
    ## intercept class3  4.84864 0.03142 154.294 0.00000
    ## intercept class4  6.29131 0.02674 235.237 0.00000
    ## intercept class5  2.85428 0.03879  73.590 0.00000
    ## days class1       0.00028 0.00008   3.359 0.00078
    ## days class2       0.00064 0.00013   5.039 0.00000
    ## days class3      -0.00090 0.00013  -7.073 0.00000
    ## days class4      -0.00005 0.00009  -0.476 0.63380
    ## days class5      -0.00127 0.00020  -6.275 0.00000
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.17309 0.00330

> Participants with \>20 completed assessments

``` r
load("lcga1_nomiss.Rdata")
load("lcga2_nomiss.Rdata")
load("lcga3_nomiss.Rdata")
load("lcga4_nomiss.Rdata")
load("lcga5_nomiss.Rdata")
```

``` r
summarytable(lcga1_nomiss, lcga2_nomiss, lcga3_nomiss, lcga4_nomiss,lcga5_nomiss)
```

    ##              G    loglik npm      BIC   %class1  %class2  %class3  %class4
    ## lcga1_nomiss 1 -2402.245   3 4817.482 100.00000                           
    ## lcga2_nomiss 2 -1998.012   6 4022.009  65.78947 34.21053                  
    ## lcga3_nomiss 3 -1860.784   9 3760.544  46.05263 35.52632 18.42105         
    ## lcga4_nomiss 4 -1811.212  12 3674.392  26.31579 10.52632 44.73684 18.42105
    ## lcga5_nomiss 5 -1860.784  15 3786.529  46.05263  0.00000  0.00000 18.42105
    ##               %class5
    ## lcga1_nomiss         
    ## lcga2_nomiss         
    ## lcga3_nomiss         
    ## lcga4_nomiss         
    ## lcga5_nomiss 35.52632

``` r
summary(lcga2_nomiss)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 2, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 76 
    ##      Number of observations: 1140 
    ##      Number of observations deleted: 456 
    ##      Number of latent classes: 2 
    ##      Number of parameters: 6  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  4 
    ##      Convergence criteria: parameters= 2.5e-06 
    ##                          : likelihood= 2.4e-05 
    ##                          : second derivatives= 3.1e-08 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -1998.01  
    ##      AIC: 4008.02  
    ##      BIC: 4022.01  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  0.67714 0.24608  2.752 0.00593
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  7.48869 0.09004 83.175 0.00000
    ## intercept class2  4.62730 0.12565 36.826 0.00000
    ## days class1      -0.00042 0.00044 -0.969 0.33261
    ## days class2      -0.00193 0.00061 -3.153 0.00162
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.33907 0.02811

``` r
summary(lcga3_nomiss)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 3, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 76 
    ##      Number of observations: 1140 
    ##      Number of observations deleted: 456 
    ##      Number of latent classes: 3 
    ##      Number of parameters: 9  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  6 
    ##      Convergence criteria: parameters= 3.9e-06 
    ##                          : likelihood= 1.1e-05 
    ##                          : second derivatives= 4.1e-11 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -1860.78  
    ##      AIC: 3739.57  
    ##      BIC: 3760.54  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  0.92168 0.31643  2.913 0.00358
    ## intercept class2  0.64685 0.33083  1.955 0.05056
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  7.86160 0.09069 86.687 0.00000
    ## intercept class2  6.15948 0.10428 59.065 0.00000
    ## intercept class3  3.86746 0.14338 26.973 0.00000
    ## days class1      -0.00007 0.00045 -0.154 0.87743
    ## days class2      -0.00137 0.00052 -2.641 0.00826
    ## days class3      -0.00230 0.00071 -3.229 0.00124
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.15628 0.02429

``` r
summary(lcga4_nomiss)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 4, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 76 
    ##      Number of observations: 1140 
    ##      Number of observations deleted: 456 
    ##      Number of latent classes: 4 
    ##      Number of parameters: 12  
    ##  
    ## Iteration process: 
    ##      Convergence criteria satisfied 
    ##      Number of iterations:  12 
    ##      Convergence criteria: parameters= 3.1e-07 
    ##                          : likelihood= 5.3e-07 
    ##                          : second derivatives= 8.7e-13 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -1811.21  
    ##      AIC: 3646.42  
    ##      BIC: 3674.39  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  0.40426 0.35607  1.135 0.25622
    ## intercept class2 -0.56280 0.45972 -1.224 0.22086
    ## intercept class3  0.89973 0.32596  2.760 0.00578
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                      coef      Se   Wald p-value
    ## intercept class1  5.95923 0.12158 49.017 0.00000
    ## intercept class2  8.79189 0.19146 45.920 0.00000
    ## intercept class3  7.44329 0.09463 78.660 0.00000
    ## intercept class4  3.83690 0.15388 24.935 0.00000
    ## days class1      -0.00165 0.00060 -2.740 0.00614
    ## days class2       0.00040 0.00092  0.436 0.66284
    ## days class3      -0.00031 0.00045 -0.681 0.49569
    ## days class4      -0.00224 0.00070 -3.214 0.00131
    ## 
    ##                              coef      Se
    ## Residual standard error:  1.09324 0.02300

``` r
summary(lcga5_nomiss)
```

    ## Heterogenous linear mixed model 
    ##      fitted by maximum likelihood method 
    ##  
    ## hlme(fixed = happy ~ days, mixture = ~days, subject = "id", ng = 5, 
    ##     data = dl_mini)
    ##  
    ## Statistical Model: 
    ##      Dataset: dl_mini 
    ##      Number of subjects: 76 
    ##      Number of observations: 1140 
    ##      Number of observations deleted: 456 
    ##      Number of latent classes: 5 
    ##      Number of parameters: 15  
    ##  
    ## Iteration process: 
    ##      Maximum number of iteration reached without convergence 
    ##      Number of iterations:  500 
    ##      Convergence criteria: parameters= 2.7e-05 
    ##                          : likelihood= 8.1e-09 
    ##                          : second derivatives= 1 
    ##  
    ## Goodness-of-fit statistics: 
    ##      maximum log-likelihood: -1860.78  
    ##      AIC: 3751.57  
    ##      BIC: 3786.53  
    ##  
    ##  
    ## Maximum Likelihood Estimates: 
    ##  
    ## Fixed effects in the class-membership model:
    ## (the class of reference is the last class) 
    ## 
    ##                       coef Se Wald p-value
    ## intercept class1   0.27483                
    ## intercept class2 -16.99698                
    ## intercept class3 -16.99688                
    ## intercept class4  -0.64685                
    ## 
    ## Fixed effects in the longitudinal model:
    ## 
    ##                       coef Se Wald p-value
    ## intercept class1   7.86160                
    ## intercept class2   6.64543                
    ## intercept class3   6.46212                
    ## intercept class4   3.86746                
    ## intercept class5   6.15948                
    ## days class1       -0.00007                
    ## days class2        0.03453                
    ## days class3       -0.04264                
    ## days class4       -0.00230                
    ## days class5       -0.00137                
    ## 
    ##                               coef Se
    ## Residual standard error:   1.15628
