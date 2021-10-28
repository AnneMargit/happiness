211027 Create missing values in complete dataset
================
Anne Margit
10/27/2021

``` r
library(lcmm)
```

    ## Loading required package: survival

    ## Loading required package: parallel

    ## Loading required package: mvtnorm

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
library(mice)
```

    ## 
    ## Attaching package: 'mice'

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

``` r
library(tidyr)
library(skimr)
```

``` r
load("dl.Rdata")
```

``` r
dl$id <- as.numeric(dl$id)
dl$days <- as.numeric(dl$days)

dl <- as.data.frame(dl)
```

Select participants with \>20 measurements (= baseline + 19)

``` r
dl$id <- as.factor(dl$id)
dl2 <- dl %>%
  group_by(id) %>%
  add_tally(wt = !is.na(Date))

dl_mini <- dl2 %>%
  filter(n > 20)
dl_mini$id <- as.numeric(dl_mini$id)
dl_mini$days <- as.numeric(dl_mini$days)
dl_mini <- as.data.frame(dl_mini)
```

Recode NAs into zeros for employment, coronaclose, leavehousewhy Fill in
missing living situation values (backwards; assuming living situation is
somewhat stable), and recode NAs into zeros

Delete physical health (measured too infrequently), countryCitizen,
Citizen, Neuro1-3 (we have sum score) Fill in missing mental health
values (from wave 1 to baseline) Fill in happiness values (up) Fill in
PFS values (up) Fill in neuroticism (sumscore) values (up)

Replace NAs in friend\_online and other\_online with 0

``` r
dl_mini <- dl_mini %>%
  mutate_at(vars(Emp1:Emp14, CoronaClose1:CoronaClose6, HouseLeaveWhy1:HouseLeaveWhy8),
            ~replace(., is.na(.), 0))

dl_mini <- dl_mini %>%
  group_by(id) %>%
  fill(liv_sit:liv_other, .direction = "up")

dl_mini <- dl_mini %>%
  mutate_at(vars(liv_sit:liv_other), ~replace(., is.na(.), 0))

dl_mini <- dl_mini %>%
  select(-c(phys_health, countryCitizen, Citizen, Neuro1, Neuro2, Neuro3))

dl_mini <- dl_mini %>%
  group_by(id) %>%
  fill(ment_health, .direction = "up")

dl_mini <- dl_mini %>%
  group_by(id) %>%
  fill(happy, .direction = "up")

dl_mini <- dl_mini %>%
  group_by(id) %>%
  fill(PFS, .direction = "up") %>%
  ungroup()

dl_mini <- dl_mini %>%
  group_by(id) %>%
  fill(neuroticism, .direction = "up") %>%
  ungroup()

dl_mini <- dl_mini %>%
  mutate_at(vars(friend_online, other_online), ~replace(., is.na(.), 0))

skim(dl_mini)
```

|                                                  |          |
| :----------------------------------------------- | :------- |
| Name                                             | dl\_mini |
| Number of rows                                   | 1596     |
| Number of columns                                | 59       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 1        |
| Date                                             | 1        |
| factor                                           | 3        |
| numeric                                          | 54       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type:
character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| liv\_sit       |          0 |              1 |   1 |   1 |     0 |         2 |          0 |

**Variable type:
Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
| :------------- | ---------: | -------------: | :--------- | :--------- | :--------- | --------: |
| Date           |          0 |              1 | 2020-03-19 | 2021-03-30 | 2020-06-06 |       144 |

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                           |
| :------------- | ---------: | -------------: | :------ | --------: | :------------------------------------ |
| country        |          0 |              1 | FALSE   |        13 | Spa: 903, Uni: 252, Uni: 126, Net: 63 |
| Age            |          0 |              1 | FALSE   |         6 | 2: 378, 3: 357, 4: 294, 5: 294        |
| Wave           |          0 |              1 | FALSE   |        21 | w0: 76, w1: 76, w2: 76, w3: 76        |

**Variable type:
numeric**

| skim\_variable  | n\_missing | complete\_rate |       mean |         sd |      p0 |       p25 |       p50 |        p75 |        p100 | hist  |
| :-------------- | ---------: | -------------: | ---------: | ---------: | ------: | --------: | --------: | ---------: | ----------: | :---- |
| id              |          0 |              1 |   25049.75 |   12232.74 | 2846.00 |  10475.00 |  31700.00 |   33953.00 |    47611.00 | ▅▁▁▇▁ |
| gender          |          0 |              1 |       0.29 |       0.48 |    0.00 |      0.00 |      0.00 |       1.00 |        2.00 | ▇▁▃▁▁ |
| Edu             |          0 |              1 |       4.54 |       1.49 |    1.00 |      4.00 |      5.00 |       6.00 |        7.00 | ▃▂▇▇▇ |
| Emp1            |          0 |              1 |       0.06 |       0.23 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp2            |          0 |              1 |       0.11 |       0.32 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp3            |          0 |              1 |       0.16 |       0.37 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| Emp4            |          0 |              1 |       0.06 |       0.23 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp5            |          0 |              1 |       0.04 |       0.19 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp6            |          0 |              1 |       0.08 |       0.27 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp7            |          0 |              1 |       0.04 |       0.20 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp8            |          0 |              1 |       0.02 |       0.13 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp9            |          0 |              1 |       0.19 |       0.39 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| Emp10           |          0 |              1 |       0.04 |       0.20 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp11           |          0 |              1 |       0.00 |       0.04 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp12           |          0 |              1 |       0.01 |       0.11 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp13           |          0 |              1 |       0.05 |       0.22 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp14           |          0 |              1 |       0.03 |       0.16 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| PFS             |          0 |              1 |     \-0.55 |       1.13 |  \-2.00 |    \-1.00 |    \-1.00 |       0.00 |        2.00 | ▅▇▃▃▁ |
| friend\_rl      |          0 |              1 |       2.26 |       2.48 |    0.00 |      0.00 |      1.00 |       4.00 |        7.00 | ▇▂▂▁▂ |
| other\_rl       |          0 |              1 |       2.59 |       2.33 |    0.00 |      1.00 |      2.00 |       4.00 |        7.00 | ▇▃▃▂▃ |
| friend\_online  |          0 |              1 |       3.34 |       2.82 |    0.00 |      0.00 |      3.00 |       7.00 |        7.00 | ▇▂▃▂▇ |
| other\_online   |          0 |              1 |       1.93 |       2.43 |    0.00 |      0.00 |      1.00 |       3.00 |        7.00 | ▇▂▂▁▂ |
| liv\_spouse     |          0 |              1 |       0.28 |       0.45 |    0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| liv\_mother     |          0 |              1 |       0.14 |       0.34 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_father     |          0 |              1 |       0.10 |       0.30 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_child      |          0 |              1 |       0.13 |       0.34 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_house      |          0 |              1 |       0.04 |       0.20 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_other      |          0 |              1 |       0.18 |       0.39 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| rel\_sat        |          0 |              1 |       6.85 |       2.05 |    1.00 |      6.00 |      7.00 |       8.00 |       10.00 | ▁▂▃▇▃ |
| lonely          |          0 |              1 |       2.22 |       1.03 |    1.00 |      1.00 |      2.00 |       3.00 |        5.00 | ▇▇▇▂▁ |
| ment\_health    |          0 |              1 |       6.77 |       1.94 |    1.00 |      6.00 |      7.00 |       8.00 |       10.00 | ▁▂▅▇▃ |
| happy           |          0 |              1 |       6.40 |       1.98 |    1.00 |      5.00 |      7.00 |       8.00 |       10.00 | ▁▂▅▇▂ |
| CoronaClose1    |          0 |              1 |       0.00 |       0.04 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose2    |          0 |              1 |       0.04 |       0.20 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose3    |          0 |              1 |       0.08 |       0.27 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose4    |          0 |              1 |       0.17 |       0.37 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| CoronaClose5    |          0 |              1 |       0.09 |       0.29 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose6    |          0 |              1 |       0.53 |       0.50 |    0.00 |      0.00 |      1.00 |       1.00 |        1.00 | ▇▁▁▁▇ |
| leave\_house    |          0 |              1 |       3.14 |       1.05 |    1.00 |      2.00 |      4.00 |       4.00 |        4.00 | ▁▃▁▂▇ |
| HouseLeaveWhy1  |          0 |              1 |       0.27 |       0.44 |    0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| HouseLeaveWhy2  |          0 |              1 |       0.59 |       0.49 |    0.00 |      0.00 |      1.00 |       1.00 |        1.00 | ▆▁▁▁▇ |
| HouseLeaveWhy4  |          0 |              1 |       0.27 |       0.44 |    0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| HouseLeaveWhy6  |          0 |              1 |       0.22 |       0.42 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| HouseLeaveWhy7  |          0 |              1 |       0.43 |       0.50 |    0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▆ |
| HouseLeaveWhy8  |          0 |              1 |       0.16 |       0.37 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| stringency      |          0 |              1 |      71.46 |       9.77 |   34.72 |     66.20 |     71.30 |      78.70 |      100.00 | ▁▂▇▅▁ |
| ConfirmedCases  |          0 |              1 | 1672854.56 | 4365144.45 |  552.00 | 191726.00 | 243928.00 | 1458591.00 | 29610445.00 | ▇▁▁▁▁ |
| ConfirmedDeaths |          0 |              1 |   50296.61 |   79887.67 |    0.00 |  17209.00 |  27921.00 |   48013.00 |   538750.00 | ▇▁▁▁▁ |
| neuroticism     |          0 |              1 |       0.20 |       0.73 |  \-2.00 |    \-0.33 |      0.33 |       0.67 |        2.33 | ▁▇▇▇▁ |
| job\_parttime   |          0 |              1 |       0.17 |       0.38 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| job\_fulltime   |          0 |              1 |       0.16 |       0.37 |    0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| know\_corona    |          0 |              1 |       0.47 |       0.50 |    0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▇ |
| days            |          0 |              1 |     130.07 |     110.38 |    0.00 |     45.00 |     79.00 |     216.00 |      376.00 | ▇▃▂▂▂ |
| n               |          0 |              1 |      21.00 |       0.00 |   21.00 |     21.00 |     21.00 |      21.00 |       21.00 | ▁▁▇▁▁ |

Save as dl\_mini\_complete

``` r
dl_mini_complete <- dl_mini

save(dl_mini_complete, file="dl_mini_complete")
```

Create random missings in happy

``` r
result <- dl_mini %>%
  ampute(prop= 0.3)
```

    ## Warning: Data is made numeric because the calculation of weights requires
    ## numeric data

``` r
mypatterns <- result$patterns

mypatterns[37] <- 0
mypatterns[1:36] <- 1
mypatterns[38:59] <- 1

result <- dl_mini %>%
  ampute(prop= 0.3, patterns = mypatterns)
```

    ## Warning: Data is made numeric because the calculation of weights requires
    ## numeric data

``` r
dl_mini_miss_happy <- result$amp
```

Create missings in other variables too, except ID, Date, stringency,
confirmedcases, confirmeddeaths, days, n

``` r
result <- dl_mini %>%
  ampute(prop= 0.3)
```

    ## Warning: Data is made numeric because the calculation of weights requires
    ## numeric data

``` r
mypatterns <- result$patterns
  
mypatterns[2:6] <- 0
mypatterns[8:50] <- 0
mypatterns[54:57] <- 0

mypatterns[1] <- 1
mypatterns[7] <- 1
mypatterns[51:53] <- 1
mypatterns[58:59] <- 1

result <- dl_mini %>%
  ampute(prop= 0.3, patterns = mypatterns)
```

    ## Warning: Data is made numeric because the calculation of weights requires
    ## numeric data

``` r
dl_mini_miss_all <- result$amp

skim(dl_mini_miss_all)
```

|                                                  |                     |
| :----------------------------------------------- | :------------------ |
| Name                                             | dl\_mini\_miss\_all |
| Number of rows                                   | 1596                |
| Number of columns                                | 59                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                     |
| Column type frequency:                           |                     |
| numeric                                          | 59                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                     |
| Group variables                                  | None                |

Data summary

**Variable type:
numeric**

| skim\_variable  | n\_missing | complete\_rate |       mean |         sd |       p0 |       p25 |       p50 |        p75 |        p100 | hist  |
| :-------------- | ---------: | -------------: | ---------: | ---------: | -------: | --------: | --------: | ---------: | ----------: | :---- |
| id              |          0 |           1.00 |   25049.75 |   12232.74 |  2846.00 |  10475.00 |  31700.00 |   33953.00 |    47611.00 | ▅▁▁▇▁ |
| gender          |        448 |           0.72 |       0.27 |       0.46 |     0.00 |      0.00 |      0.00 |       1.00 |        2.00 | ▇▁▃▁▁ |
| country         |        448 |           0.72 |      92.32 |      22.29 |    21.00 |     99.00 |     99.00 |      99.00 |      111.00 | ▁▁▁▁▇ |
| Age             |        448 |           0.72 |       3.20 |       1.43 |     1.00 |      2.00 |      3.00 |       4.00 |        6.00 | ▇▅▅▅▁ |
| Edu             |        448 |           0.72 |       4.52 |       1.47 |     1.00 |      4.00 |      5.00 |       6.00 |        7.00 | ▅▂▇▇▇ |
| Wave            |        448 |           0.72 |      10.02 |       5.83 |     1.00 |      5.00 |     10.00 |      15.00 |       21.00 | ▇▆▆▅▃ |
| Date            |          0 |           1.00 |   18470.07 |     110.38 | 18340.00 |  18385.00 |  18419.00 |   18556.00 |    18716.00 | ▇▃▂▂▂ |
| Emp1            |        448 |           0.72 |       0.06 |       0.25 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp2            |        448 |           0.72 |       0.12 |       0.33 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp3            |        448 |           0.72 |       0.17 |       0.37 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| Emp4            |        448 |           0.72 |       0.06 |       0.24 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp5            |        448 |           0.72 |       0.04 |       0.19 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp6            |        448 |           0.72 |       0.09 |       0.29 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp7            |        448 |           0.72 |       0.05 |       0.21 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp8            |        448 |           0.72 |       0.02 |       0.13 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp9            |        448 |           0.72 |       0.20 |       0.40 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| Emp10           |        448 |           0.72 |       0.04 |       0.20 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp11           |        448 |           0.72 |       0.00 |       0.04 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp12           |        448 |           0.72 |       0.01 |       0.11 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp13           |        448 |           0.72 |       0.05 |       0.21 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| Emp14           |        448 |           0.72 |       0.03 |       0.16 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| PFS             |        448 |           0.72 |     \-0.51 |       1.15 |   \-2.00 |    \-1.00 |    \-1.00 |       0.00 |        2.00 | ▅▇▅▅▁ |
| friend\_rl      |        448 |           0.72 |       2.24 |       2.52 |     0.00 |      0.00 |      1.00 |       4.00 |        7.00 | ▇▂▂▁▂ |
| other\_rl       |        448 |           0.72 |       2.56 |       2.32 |     0.00 |      1.00 |      2.00 |       4.00 |        7.00 | ▇▃▃▂▃ |
| friend\_online  |        448 |           0.72 |       3.57 |       2.77 |     0.00 |      1.00 |      3.00 |       7.00 |        7.00 | ▇▂▃▂▇ |
| other\_online   |        448 |           0.72 |       1.99 |       2.42 |     0.00 |      0.00 |      1.00 |       3.00 |        7.00 | ▇▂▂▁▂ |
| liv\_sit        |        448 |           0.72 |       0.49 |       0.50 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▇ |
| liv\_spouse     |        448 |           0.72 |       0.32 |       0.47 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| liv\_mother     |        448 |           0.72 |       0.15 |       0.36 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| liv\_father     |        448 |           0.72 |       0.11 |       0.31 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_child      |        448 |           0.72 |       0.15 |       0.36 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| liv\_house      |        448 |           0.72 |       0.05 |       0.21 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| liv\_other      |        448 |           0.72 |       0.21 |       0.40 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| rel\_sat        |        448 |           0.72 |       6.90 |       2.03 |     1.00 |      6.00 |      7.00 |       8.00 |       10.00 | ▁▂▃▇▃ |
| lonely          |        448 |           0.72 |       2.19 |       1.02 |     1.00 |      1.00 |      2.00 |       3.00 |        5.00 | ▇▇▇▂▁ |
| ment\_health    |        448 |           0.72 |       6.86 |       1.93 |     1.00 |      6.00 |      7.00 |       8.00 |       10.00 | ▁▂▅▇▃ |
| happy           |        448 |           0.72 |       6.46 |       1.95 |     1.00 |      5.00 |      7.00 |       8.00 |       10.00 | ▁▂▅▇▂ |
| CoronaClose1    |        448 |           0.72 |       0.00 |       0.05 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose2    |        448 |           0.72 |       0.05 |       0.21 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose3    |        448 |           0.72 |       0.09 |       0.29 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose4    |        448 |           0.72 |       0.18 |       0.38 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| CoronaClose5    |        448 |           0.72 |       0.09 |       0.29 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▁ |
| CoronaClose6    |        448 |           0.72 |       0.57 |       0.49 |     0.00 |      0.00 |      1.00 |       1.00 |        1.00 | ▆▁▁▁▇ |
| leave\_house    |        448 |           0.72 |       3.10 |       1.07 |     1.00 |      2.00 |      4.00 |       4.00 |        4.00 | ▁▃▁▂▇ |
| HouseLeaveWhy1  |        448 |           0.72 |       0.26 |       0.44 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| HouseLeaveWhy2  |        448 |           0.72 |       0.57 |       0.50 |     0.00 |      0.00 |      1.00 |       1.00 |        1.00 | ▆▁▁▁▇ |
| HouseLeaveWhy4  |        448 |           0.72 |       0.26 |       0.44 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▃ |
| HouseLeaveWhy6  |        448 |           0.72 |       0.24 |       0.43 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| HouseLeaveWhy7  |        448 |           0.72 |       0.40 |       0.49 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▅ |
| HouseLeaveWhy8  |        448 |           0.72 |       0.16 |       0.37 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| stringency      |          0 |           1.00 |      71.46 |       9.77 |    34.72 |     66.20 |     71.30 |      78.70 |      100.00 | ▁▂▇▅▁ |
| ConfirmedCases  |          0 |           1.00 | 1672854.56 | 4365144.45 |   552.00 | 191726.00 | 243928.00 | 1458591.00 | 29610445.00 | ▇▁▁▁▁ |
| ConfirmedDeaths |          0 |           1.00 |   50296.61 |   79887.67 |     0.00 |  17209.00 |  27921.00 |   48013.00 |   538750.00 | ▇▁▁▁▁ |
| neuroticism     |        448 |           0.72 |       0.18 |       0.73 |   \-2.00 |    \-0.33 |      0.33 |       0.67 |        2.33 | ▁▇▇▇▁ |
| job\_parttime   |        448 |           0.72 |       0.19 |       0.39 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| job\_fulltime   |        448 |           0.72 |       0.17 |       0.37 |     0.00 |      0.00 |      0.00 |       0.00 |        1.00 | ▇▁▁▁▂ |
| know\_corona    |        448 |           0.72 |       0.43 |       0.49 |     0.00 |      0.00 |      0.00 |       1.00 |        1.00 | ▇▁▁▁▆ |
| days            |          0 |           1.00 |     130.07 |     110.38 |     0.00 |     45.00 |     79.00 |     216.00 |      376.00 | ▇▃▂▂▂ |
| n               |          0 |           1.00 |      21.00 |       0.00 |    21.00 |     21.00 |     21.00 |      21.00 |       21.00 | ▁▁▇▁▁ |

Save data

``` r
save(dl_mini_miss_happy, file="dl_mini_miss_happy.Rdata")
save(dl_mini_miss_all, file="dl_mini_miss_all.Rdata")
```
