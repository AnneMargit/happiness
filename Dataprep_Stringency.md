Dataprep\_Stringency
================
Anne Margit
4/28/2021

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
library(tidyr)
library(anytime)
```

    ## Warning: package 'anytime' was built under R version 4.0.3

``` r
library(arsenal)
```

    ## Warning: package 'arsenal' was built under R version 4.0.3

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(knitr)
```

``` r
load("data_long.Rdata")
oxforddata <- read.csv("OxCGRT_latest.csv", header=TRUE)
```

Check which country names are different in the datasets:

``` r
data_long$coded_country <- as.factor(data_long$coded_country)
oxforddata$CountryName <- as.factor(oxforddata$CountryName)

levels1 <- levels(data_long$coded_country)
levels2 <- levels(oxforddata$CountryName)

levels1<- as.data.frame(levels1)
levels2<- as.data.frame(levels2)

levels1$levels1 <-as.character(levels1$levels1)
levels2$levels2 <-as.character(levels2$levels2)

check <- comparedf(levels1, levels2, by.x="levels1", by.y="levels2")
summary(check)
```

    ## 
    ## 
    ## Table: Summary of data.frames
    ## 
    ## version   arg        ncol   nrow
    ## --------  --------  -----  -----
    ## x         levels1       1    116
    ## y         levels2       1    186
    ## 
    ## 
    ## 
    ## Table: Summary of overall comparison
    ## 
    ## statistic                                                      value
    ## ------------------------------------------------------------  ------
    ## Number of by-variables                                             1
    ## Number of non-by variables in common                               0
    ## Number of variables compared                                       0
    ## Number of variables in x but not y                                 0
    ## Number of variables in y but not x                                 0
    ## Number of variables compared with some values unequal              0
    ## Number of variables compared with all values equal                 0
    ## Number of observations in common                                 107
    ## Number of observations in x but not y                              9
    ## Number of observations in y but not x                             79
    ## Number of observations with some compared variables unequal        0
    ## Number of observations with all compared variables equal         107
    ## Number of values unequal                                           0
    ## 
    ## 
    ## 
    ## Table: Variables not shared
    ## 
    ## |                        |
    ## |:-----------------------|
    ## |No variables not shared |
    ## 
    ## 
    ## 
    ## Table: Other variables not compared
    ## 
    ## |                                |
    ## |:-------------------------------|
    ## |No other variables not compared |
    ## 
    ## 
    ## 
    ## Table: Observations not shared
    ## 
    ## version   levels1                         observation
    ## --------  -----------------------------  ------------
    ## x                                                   1
    ## x         Armenia                                   6
    ## x         Hong Kong S.A.R.                         43
    ## x         Kyrgyzstan                               60
    ## x         Montenegro                               74
    ## x         Republic of Serbia                       91
    ## x         Slovakia                                 96
    ## x         United Republic of Tanzania             111
    ## x         United States of America                112
    ## y         Afghanistan                               1
    ## y         Angola                                    5
    ## y         Aruba                                     7
    ## y         Bahamas                                  11
    ## y         Barbados                                 14
    ## y         Belize                                   17
    ## y         Bermuda                                  19
    ## y         Bhutan                                   20
    ## y         Bolivia                                  21
    ## y         Burkina Faso                             27
    ## y         Burundi                                  28
    ## y         Cape Verde                               32
    ## y         Central African Republic                 33
    ## y         Chad                                     34
    ## y         Comoros                                  38
    ## y         Congo                                    39
    ## y         Cote d'Ivoire                            41
    ## y         Cuba                                     43
    ## y         Democratic Republic of Congo             46
    ## y         Djibouti                                 48
    ## y         Dominica                                 49
    ## y         Eritrea                                  54
    ## y         Eswatini                                 56
    ## y         Faeroe Islands                           58
    ## y         Fiji                                     59
    ## y         Gabon                                    62
    ## y         Gambia                                   63
    ## y         Ghana                                    66
    ## y         Greenland                                68
    ## y         Guam                                     69
    ## y         Guinea                                   71
    ## y         Guyana                                   72
    ## y         Haiti                                    73
    ## y         Honduras                                 74
    ## y         Hong Kong                                75
    ## y         Kiribati                                 90
    ## y         Kyrgyz Republic                          93
    ## y         Lesotho                                  97
    ## y         Liberia                                  98
    ## y         Liechtenstein                           100
    ## y         Macao                                   103
    ## y         Madagascar                              104
    ## y         Malawi                                  105
    ## y         Mauritania                              109
    ## y         Monaco                                  113
    ## y         Mozambique                              116
    ## y         Namibia                                 118
    ## y         Nicaragua                               122
    ## y         Niger                                   123
    ## y         Papua New Guinea                        130
    ## y         Paraguay                                131
    ## y         Puerto Rico                             136
    ## y         Rwanda                                  140
    ## y         San Marino                              141
    ## y         Senegal                                 143
    ## y         Serbia                                  144
    ## y         Seychelles                              145
    ## y         Sierra Leone                            146
    ## y         Slovak Republic                         148
    ## y         Solomon Islands                         150
    ## y         Somalia                                 151
    ## y         South Sudan                             154
    ## y         Sri Lanka                               156
    ## y         Sudan                                   157
    ## y         Suriname                                158
    ## y         Syria                                   161
    ## y         Tajikistan                              163
    ## y         Tanzania                                164
    ## y         Timor-Leste                             166
    ## y         Togo                                    167
    ## y         Tonga                                   168
    ## y         Turkmenistan                            172
    ## y         Uganda                                  173
    ## y         United States                           177
    ## y         United States Virgin Islands            178
    ## y         Vanuatu                                 181
    ## y         Yemen                                   184
    ## y         Zambia                                  185
    ## y         Zimbabwe                                186
    ## 
    ## 
    ## 
    ## Table: Differences detected by variable
    ## 
    ## |                                    |
    ## |:-----------------------------------|
    ## |No differences detected by variable |
    ## 
    ## 
    ## 
    ## Table: Differences detected
    ## 
    ## |                        |
    ## |:-----------------------|
    ## |No differences detected |
    ## 
    ## 
    ## 
    ## Table: Non-identical attributes
    ## 
    ## |                            |
    ## |:---------------------------|
    ## |No non-identical attributes |

Rename countries

``` r
data_long$coded_country <- 
  plyr::revalue(data_long$coded_country , c("Hong Kong S.A.R."="Hong Kong", 
                                       "Kyrgyzstan" = "Kyrgyz Republic",
                                       "United Republic of Tanzania" = "Tanzania", 
                                       "United States of America" = "United States",
                                       "Republic of Serbia" = "Serbia",
                                       "Slovakia" = "Slovak Republic"))
```

Renaming and recoding variables:

``` r
data_long$age <- as.factor(data_long$age)
names(data_long)[names(data_long) == "age"] <- "Age"
names(data_long)[names(data_long) == "coded_country"] <- "Country"
names(data_long)[names(data_long) == "X"] <- "ID"
names(data_long)[names(data_long) == "RecordedDate"] <- "Date"
names(data_long)[names(data_long) == "gender"] <- "Gender"
names(data_long)[names(data_long) == "edu"] <- "Edu"
names(oxforddata)[names(oxforddata) == "CountryName"] <- "Country"

data_long$Country <- as.character(data_long$Country)
data_long$Country[data_long$Country==""] <- NA
data_long$Country <- as.factor(data_long$Country)

oxforddata$Date <-anydate(oxforddata$Date)
data_long$Date <- anydate(data_long$Date)
```

Select nation-wide data:

``` r
oxforddata <- oxforddata %>%
filter(Jurisdiction == "NAT_TOTAL")
```

Drop not used variables:

``` r
stringency_data <- oxforddata %>% select(Date, Country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Join

``` r
data_long_str <- left_join(data_long, stringency_data, by=c("Date", "Country"))
```

Save

``` r
save(data_long_str, file="data_long_str.Rdata")
```
