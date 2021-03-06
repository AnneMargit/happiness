Happiness\_widetolong
================
Anne Margit
4/26/2021

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
library(papaja)
```

``` r
load("data.Rdata")
```

``` r
colnames(data)

data.all <- data %>%
  select(,1:729)

data.all <- data.all %>%
  select(,-c(2:43))

colnames(data.all)
```

``` r
data.all$RecordedDate <- anydate(data.all$RecordedDate)
data.all$w1_RecordedDate <- anydate(data.all$w1_RecordedDate)
data.all$w2_RecordedDate <- anydate(data.all$w2_RecordedDate)
data.all$w3_RecordedDate <- anydate(data.all$w3_RecordedDate)
data.all$w4_RecordedDate <- anydate(data.all$w4_RecordedDate)
data.all$w5_RecordedDate <- anydate(data.all$w5_RecordedDate)
data.all$w6_RecordedDate <- anydate(data.all$w6_RecordedDate)
data.all$w7_RecordedDate <- anydate(data.all$w7_RecordedDate)
data.all$w8_RecordedDate <- anydate(data.all$w8_RecordedDate)
data.all$w9_RecordedDate <- anydate(data.all$w9_RecordedDate)
data.all$w10_RecordedDate <- anydate(data.all$w10_RecordedDate)
data.all$w11_RecordedDate <- anydate(data.all$w11_RecordedDate)
data.all$w12_RecordedDate <- anydate(data.all$w12_RecordedDate)
data.all$w13_RecordedDate <- anydate(data.all$w13_RecordedDate)
data.all$w14_RecordedDate <- anydate(data.all$w14_RecordedDate)
data.all$w15_RecordedDate <- anydate(data.all$w15_RecordedDate)
data.all$w16_RecordedDate <- anydate(data.all$w16_RecordedDate)
data.all$w17_RecordedDate <- anydate(data.all$w17_RecordedDate)
data.all$w18_RecordedDate <- anydate(data.all$w18_RecordedDate)
data.all$w19_RecordedDate <- anydate(data.all$w19_RecordedDate)
data.all$w20_RecordedDate <- anydate(data.all$w20_RecordedDate)
```

``` r
data_Date <- melt(data.all,
    id.vars=c("X", "gender", "coded_country", "age", "edu", "countryCitizen", "Citizen"),
    measure.vars=c("RecordedDate", "w1_RecordedDate", "w2_RecordedDate", "w3_RecordedDate", "w4_RecordedDate", "w5_RecordedDate", "w6_RecordedDate", "w7_RecordedDate",  "w8_RecordedDate",  "w9_RecordedDate",  "w10_RecordedDate",  "w11_RecordedDate",  "w12_RecordedDate",  "w13_RecordedDate",  "w14_RecordedDate",  "w15_RecordedDate",  "w16_RecordedDate",  "w17_RecordedDate",  "w18_RecordedDate",  "w19_RecordedDate",  "w20_RecordedDate"),
    variable.name="Wave",
    value.name="RecordedDate")
```

``` r
data_Date$Wave <- data_Date$Wave %>%
  recode(RecordedDate = "w0",
         w1_RecordedDate = "w1",
         w2_RecordedDate = "w2",
         w3_RecordedDate = "w3",
         w4_RecordedDate = "w4",
         w5_RecordedDate = "w5",
         w6_RecordedDate = "w6",
         w7_RecordedDate = "w7",
         w8_RecordedDate = "w8",
         w9_RecordedDate = "w9",
         w10_RecordedDate = "w10",
         w11_RecordedDate = "w11",
         w12_RecordedDate = "w12",
         w13_RecordedDate = "w13",
         w14_RecordedDate = "w14",
         w15_RecordedDate = "w15",
         w16_RecordedDate = "w16",
         w17_RecordedDate = "w17",
         w18_RecordedDate = "w18",
         w19_RecordedDate = "w19",
         w20_RecordedDate = "w20"
         )
```

``` r
data_Emp1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_1", "w1_employstatus_1", "w2_employstatus_1", "w3_employstatus_1", "w4_employstatus_1", "w5_employstatus_1", "w7_employstatus_1",  "w11_employstatus_1", "w12_employstatus_1", "w13_employstatus_1", "w14_employstatus_1", "w15_employstatus_1", "w16_employstatus_1"),
    variable.name="Wave",
    value.name="Emp1")

data_Emp1$Wave <- data_Emp1$Wave %>%
  recode(employstatus_1 = "w0",
         w1_employstatus_1 = "w1",
         w2_employstatus_1 = "w2",
         w3_employstatus_1 = "w3",
         w4_employstatus_1 = "w4",
         w5_employstatus_1 = "w5",
         w7_employstatus_1 = "w7",
         w11_employstatus_1 = "w11",
         w12_employstatus_1 = "w12",
         w13_employstatus_1 = "w13",
         w14_employstatus_1 = "w14",
         w15_employstatus_1 = "w15",
         w16_employstatus_1 = "w16"
         )
```

``` r
data_new <- left_join(data_Date, data_Emp1, by=c("X", "Wave"))
```

``` r
data_Emp2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_2", "w1_employstatus_2", "w2_employstatus_2", "w3_employstatus_2", "w4_employstatus_2", "w5_employstatus_2", "w7_employstatus_2",  "w11_employstatus_2", "w12_employstatus_2", "w13_employstatus_2", "w14_employstatus_2", "w15_employstatus_2", "w16_employstatus_2"),
    variable.name="Wave",
    value.name="Emp2")

data_Emp2$Wave <- data_Emp2$Wave %>%
  recode(employstatus_2 = "w0",
         w1_employstatus_2 = "w1",
         w2_employstatus_2 = "w2",
         w3_employstatus_2 = "w3",
         w4_employstatus_2 = "w4",
         w5_employstatus_2 = "w5",
         w7_employstatus_2 = "w7",
         w11_employstatus_2 = "w11",
         w12_employstatus_2 = "w12",
         w13_employstatus_2 = "w13",
         w14_employstatus_2 = "w14",
         w15_employstatus_2 = "w15",
         w16_employstatus_2 = "w16"
         )
```

``` r
data_new2 <- left_join(data_new, data_Emp2, by=c("X", "Wave"))
```

``` r
data_Emp3 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_3", "w1_employstatus_3", "w2_employstatus_3", "w3_employstatus_3", "w4_employstatus_3", "w5_employstatus_3", "w7_employstatus_3",  "w11_employstatus_3", "w12_employstatus_3", "w13_employstatus_3", "w14_employstatus_3", "w15_employstatus_3", "w16_employstatus_3"),
    variable.name="Wave",
    value.name="Emp3")

data_Emp3$Wave <- data_Emp3$Wave %>%
  recode(employstatus_3 = "w0",
         w1_employstatus_3 = "w1",
         w2_employstatus_3 = "w2",
         w3_employstatus_3 = "w3",
         w4_employstatus_3 = "w4",
         w5_employstatus_3 = "w5",
         w7_employstatus_3 = "w7",
         w11_employstatus_3 = "w11",
         w12_employstatus_3 = "w12",
         w13_employstatus_3 = "w13",
         w14_employstatus_3 = "w14",
         w15_employstatus_3 = "w15",
         w16_employstatus_3 = "w16"
         )
```

``` r
data_new3 <- left_join(data_new2, data_Emp3, by=c("X", "Wave"))
```

``` r
data_Emp4 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_4", "w1_employstatus_4", "w2_employstatus_4", "w3_employstatus_4", "w4_employstatus_4", "w5_employstatus_4", "w7_employstatus_4",  "w11_employstatus_4", "w12_employstatus_4", "w13_employstatus_4", "w14_employstatus_4", "w15_employstatus_4", "w16_employstatus_4"),
    variable.name="Wave",
    value.name="Emp4")

data_Emp4$Wave <- data_Emp4$Wave %>%
  recode(employstatus_4 = "w0",
         w1_employstatus_4 = "w1",
         w2_employstatus_4 = "w2",
         w3_employstatus_4 = "w3",
         w4_employstatus_4 = "w4",
         w5_employstatus_4 = "w5",
         w7_employstatus_4 = "w7",
         w11_employstatus_4 = "w11",
         w12_employstatus_4 = "w12",
         w13_employstatus_4 = "w13",
         w14_employstatus_4 = "w14",
         w15_employstatus_4 = "w15",
         w16_employstatus_4 = "w16"
         )
```

``` r
data_new4 <- left_join(data_new3, data_Emp4, by=c("X", "Wave"))
```

``` r
data_Emp5 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_5", "w1_employstatus_5", "w2_employstatus_5", "w3_employstatus_5", "w4_employstatus_5", "w5_employstatus_5", "w7_employstatus_5",  "w11_employstatus_5", "w12_employstatus_5", "w13_employstatus_5", "w14_employstatus_5", "w15_employstatus_5", "w16_employstatus_5"),
    variable.name="Wave",
    value.name="Emp5")

data_Emp5$Wave <- data_Emp5$Wave %>%
  recode(employstatus_5 = "w0",
         w1_employstatus_5 = "w1",
         w2_employstatus_5 = "w2",
         w3_employstatus_5 = "w3",
         w4_employstatus_5 = "w4",
         w5_employstatus_5 = "w5",
         w7_employstatus_5 = "w7",
         w11_employstatus_5 = "w11",
         w12_employstatus_5 = "w12",
         w13_employstatus_5 = "w13",
         w14_employstatus_5 = "w14",
         w15_employstatus_5 = "w15",
         w16_employstatus_5 = "w16"
         )
```

``` r
data_new5 <- left_join(data_new4, data_Emp5, by=c("X", "Wave"))
```

``` r
data_Emp6 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_6", "w1_employstatus_6", "w2_employstatus_6", "w3_employstatus_6", "w4_employstatus_6", "w5_employstatus_6", "w7_employstatus_6",  "w11_employstatus_6", "w12_employstatus_6", "w13_employstatus_6", "w14_employstatus_6", "w15_employstatus_6", "w16_employstatus_6"),
    variable.name="Wave",
    value.name="Emp6")

data_Emp6$Wave <- data_Emp6$Wave %>%
  recode(employstatus_6 = "w0",
         w1_employstatus_6 = "w1",
         w2_employstatus_6 = "w2",
         w3_employstatus_6 = "w3",
         w4_employstatus_6 = "w4",
         w5_employstatus_6 = "w5",
         w7_employstatus_6 = "w7",
         w11_employstatus_6 = "w11",
         w12_employstatus_6 = "w12",
         w13_employstatus_6 = "w13",
         w14_employstatus_6 = "w14",
         w15_employstatus_6 = "w15",
         w16_employstatus_6 = "w16"
         )
```

``` r
data_new6 <- left_join(data_new5, data_Emp6, by=c("X", "Wave"))
```

``` r
data_Emp7 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_7", "w1_employstatus_7", "w2_employstatus_7", "w3_employstatus_7", "w4_employstatus_7", "w5_employstatus_7", "w7_employstatus_7",  "w11_employstatus_7", "w12_employstatus_7", "w13_employstatus_7", "w14_employstatus_7", "w15_employstatus_7", "w16_employstatus_7"),
    variable.name="Wave",
    value.name="Emp7")

data_Emp7$Wave <- data_Emp7$Wave %>%
  recode(employstatus_7 = "w0",
         w1_employstatus_7 = "w1",
         w2_employstatus_7 = "w2",
         w3_employstatus_7 = "w3",
         w4_employstatus_7 = "w4",
         w5_employstatus_7 = "w5",
         w7_employstatus_7 = "w7",
         w11_employstatus_7 = "w11",
         w12_employstatus_7 = "w12",
         w13_employstatus_7 = "w13",
         w14_employstatus_7 = "w14",
         w15_employstatus_7 = "w15",
         w16_employstatus_7 = "w16"
         )
```

``` r
data_new7 <- left_join(data_new6, data_Emp7, by=c("X", "Wave"))
```

``` r
data_Emp8 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_8", "w1_employstatus_8", "w2_employstatus_8", "w3_employstatus_8", "w4_employstatus_8", "w5_employstatus_8", "w7_employstatus_8",  "w11_employstatus_8", "w12_employstatus_8", "w13_employstatus_8", "w14_employstatus_8", "w15_employstatus_8", "w16_employstatus_8"),
    variable.name="Wave",
    value.name="Emp8")

data_Emp8$Wave <- data_Emp8$Wave %>%
  recode(employstatus_8 = "w0",
         w1_employstatus_8 = "w1",
         w2_employstatus_8 = "w2",
         w3_employstatus_8 = "w3",
         w4_employstatus_8 = "w4",
         w5_employstatus_8 = "w5",
         w7_employstatus_8 = "w7",
         w11_employstatus_8 = "w11",
         w12_employstatus_8 = "w12",
         w13_employstatus_8 = "w13",
         w14_employstatus_8 = "w14",
         w15_employstatus_8 = "w15",
         w16_employstatus_8 = "w16"
         )
```

``` r
data_new8 <- left_join(data_new7, data_Emp8, by=c("X", "Wave"))
```

``` r
data_Emp9 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("employstatus_9", "w1_employstatus_9", "w2_employstatus_9", "w3_employstatus_9", "w4_employstatus_9", "w5_employstatus_9", "w7_employstatus_9",  "w11_employstatus_9", "w12_employstatus_9", "w13_employstatus_9", "w14_employstatus_9", "w15_employstatus_9", "w16_employstatus_9"),
    variable.name="Wave",
    value.name="Emp9")

data_Emp9$Wave <- data_Emp9$Wave %>%
  recode(employstatus_9 = "w0",
         w1_employstatus_9 = "w1",
         w2_employstatus_9 = "w2",
         w3_employstatus_9 = "w3",
         w4_employstatus_9 = "w4",
         w5_employstatus_9 = "w5",
         w7_employstatus_9 = "w7",
         w11_employstatus_9 = "w11",
         w12_employstatus_9 = "w12",
         w13_employstatus_9 = "w13",
         w14_employstatus_9 = "w14",
         w15_employstatus_9 = "w15",
         w16_employstatus_9 = "w16"
         )
```

``` r
data_new9 <- left_join(data_new8, data_Emp9, by=c("X", "Wave"))
```

``` r
data_Emp10 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(173:185),
    variable.name="Wave",
    value.name="Emp10")
    
data_Emp10$Wave <- data_Emp10$Wave %>%
  recode(employstatus_10 = "w0",
         w1_employstatus_10 = "w1",
         w2_employstatus_10 = "w2",
         w3_employstatus_10 = "w3",
         w4_employstatus_10 = "w4",
         w5_employstatus_10 = "w5",
         w7_employstatus_10 = "w7",
         w11_employstatus_10 = "w11",
         w12_employstatus_10 = "w12",
         w13_employstatus_10 = "w13",
         w14_employstatus_10 = "w14",
         w15_employstatus_10 = "w15",
         w16_employstatus_10 = "w16"
         )
```

``` r
data_new10 <- left_join(data_new9, data_Emp10, by=c("X", "Wave"))
```

``` r
data_Emp11 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(97:108),
    variable.name="Wave",
    value.name="Emp11")
    
data_Emp11$Wave <- data_Emp11$Wave %>%
  recode(w1_employstatus_11 = "w1",
         w2_employstatus_11 = "w2",
         w3_employstatus_11 = "w3",
         w4_employstatus_11 = "w4",
         w5_employstatus_11 = "w5",
         w7_employstatus_11 = "w7",
         w11_employstatus_11 = "w11",
         w12_employstatus_11 = "w12",
         w13_employstatus_11 = "w13",
         w14_employstatus_11 = "w14",
         w15_employstatus_11 = "w15",
         w16_employstatus_11 = "w16"
         )
```

``` r
data_new11 <- left_join(data_new10, data_Emp11, by=c("X", "Wave"))
```

``` r
data_Emp12 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(109:118),
    variable.name="Wave",
    value.name="Emp12")
    
data_Emp12$Wave <- data_Emp12$Wave %>%
  recode(w1_employstatus_12 = "w1",
         w2_employstatus_12 = "w2",
         w3_employstatus_12 = "w3",
         w4_employstatus_12 = "w4",
         w5_employstatus_12 = "w5",
         w7_employstatus_12 = "w7",
         w11_employstatus_12 = "w11",
         w12_employstatus_12 = "w12",
         w13_employstatus_12 = "w13",
         w14_employstatus_12 = "w14",
         w15_employstatus_12 = "w15",
         w16_employstatus_12 = "w16"
         )
```

``` r
data_new12 <- left_join(data_new11, data_Emp12, by=c("X", "Wave"))
```

``` r
data_Emp13 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(186:197),
    variable.name="Wave",
    value.name="Emp13")
    
data_Emp13$Wave <- data_Emp13$Wave %>%
  recode(w1_employstatus_13 = "w1",
         w2_employstatus_13 = "w2",
         w3_employstatus_13 = "w3",
         w4_employstatus_13 = "w4",
         w5_employstatus_13 = "w5",
         w7_employstatus_13 = "w7",
         w11_employstatus_13 = "w11",
         w12_employstatus_13 = "w12",
         w13_employstatus_13 = "w13",
         w14_employstatus_13 = "w14",
         w15_employstatus_13 = "w15",
         w16_employstatus_13 = "w16"
         )
```

``` r
data_new13 <- left_join(data_new12, data_Emp13, by=c("X", "Wave"))
```

``` r
data_Emp14 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(62:70),
    variable.name="Wave",
    value.name="Emp14")
    
data_Emp14$Wave <- data_Emp14$Wave %>%
  recode(w4_employstatus_14 = "w4",
         w5_employstatus_14 = "w5",
         w7_employstatus_14 = "w7",
         w11_employstatus_14 = "w11",
         w12_employstatus_14 = "w12",
         w13_employstatus_14 = "w13",
         w14_employstatus_14 = "w14",
         w15_employstatus_14 = "w15",
         w16_employstatus_14 = "w16"
         )
```

``` r
data_new14 <- left_join(data_new13, data_Emp14, by=c("X", "Wave"))
```

``` r
data_PFS <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(198:214),
    variable.name="Wave",
    value.name="PFS")
    
data_PFS$Wave <- data_PFS$Wave %>%
  recode(PFS01 = "w0",
         w1_PFS01 = "w1",
         w2_PFS01 = "w2",
         w3_PFS01 = "w3",
         w4_PFS01 = "w4",
         w5_PFS01 = "w5",
         w6_PFS01 = "w6",
         w7_PFS01 = "w7",
         w8_PFS01 = "w8",
         w9_PFS01 = "w9",
         w10_PFS01 = "w10",
         w11_PFS01 = "w11",
         w12_PFS01 = "w12",
         w13_PFS01 = "w13",
         w14_PFS01 = "w14",
         w15_PFS01 = "w15",
         w16_PFS01 = "w16",
         w17_PFS01 = "w17",
         w18_PFS01 = "w18",
         w19_PFS01 = "w19",
         w20_PFS01 = "w20"
  )
```

``` r
data_new15 <- left_join(data_new14, data_PFS, by=c("X", "Wave"))
```

``` r
data_ISO1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(215:235),
    variable.name="Wave",
    value.name="ISO_Friends_inperson")
    
data_ISO1$Wave <- data_ISO1$Wave %>%
  recode(isoFriends_inPerson = "w0",
         w1_isoFriends_inPerson = "w1",
         w2_isoFriends_inPerson = "w2",
         w3_isoFriends_inPerson = "w3",
         w4_isoFriends_inPerson = "w4",
         w5_isoFriends_inPerson = "w5",
         w6_isoFriends_inPerson = "w6",
         w7_isoFriends_inPerson = "w7",
         w8_isoFriends_inPerson = "w8",
         w9_isoFriends_inPerson = "w9",
         w10_isoFriends_inPerson = "w10",
         w11_isoFriends_inPerson = "w11",
         w12_isoFriends_inPerson = "w12",
         w13_isoFriends_inPerson = "w13",
         w14_isoFriends_inPerson = "w14",
         w15_isoFriends_inPerson = "w15",
         w16_isoFriends_inPerson = "w16",
         w17_isoFriends_inPerson = "w17",
         w18_isoFriends_inPerson = "w18",
         w19_isoFriends_inPerson = "w19",
         w20_isoFriends_inPerson = "w20"
  )
```

``` r
data_new16 <- left_join(data_new15, data_ISO1, by=c("X", "Wave"))
```

``` r
data_ISO2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(236:256),
    variable.name="Wave",
    value.name="ISO_OthPpl_inperson")
    
data_ISO2$Wave <- data_ISO2$Wave %>%
  recode(isoOthPpl_inPerson = "w0",
         w1_isoOthPpl_inPerson = "w1",
         w2_isoOthPpl_inPerson = "w2",
         w3_isoOthPpl_inPerson = "w3",
         w4_isoOthPpl_inPerson = "w4",
         w5_isoOthPpl_inPerson = "w5",
         w6_isoOthPpl_inPerson = "w6",
         w7_isoOthPpl_inPerson = "w7",
         w8_isoOthPpl_inPerson = "w8",
         w9_isoOthPpl_inPerson = "w9",
         w10_isoOthPpl_inPerson = "w10",
         w11_isoOthPpl_inPerson = "w11",
         w12_isoOthPpl_inPerson = "w12",
         w13_isoOthPpl_inPerson = "w13",
         w14_isoOthPpl_inPerson = "w14",
         w15_isoOthPpl_inPerson = "w15",
         w16_isoOthPpl_inPerson = "w16",
         w17_isoOthPpl_inPerson = "w17",
         w18_isoOthPpl_inPerson = "w18",
         w19_isoOthPpl_inPerson = "w19",
         w20_isoOthPpl_inPerson = "w20"
  )
```

``` r
data_new17 <- left_join(data_new16, data_ISO2, by=c("X", "Wave"))
```

``` r
data_ISO3 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(257:273),
    variable.name="Wave",
    value.name="ISO_Friends_online")
    
data_ISO3$Wave <- data_ISO3$Wave %>%
  recode(isoFriends_online = "w0",
         w1_isoFriends_online = "w1",
         w2_isoFriends_online = "w2",
         w3_isoFriends_online = "w3",
         w4_isoFriends_online = "w4",
         w5_isoFriends_online = "w5",
         w6_isoFriends_online = "w6",
         w7_isoFriends_online = "w7",
         w8_isoFriends_online = "w8",
         w9_isoFriends_online = "w9",
         w10_isoFriends_online = "w10",
         w11_isoFriends_online = "w11",
         w12_isoFriends_online = "w12",
         w13_isoFriends_online = "w13",
         w14_isoFriends_online = "w14",
         w15_isoFriends_online = "w15",
         w16_isoFriends_online = "w16",
         w17_isoFriends_online = "w17",
         w18_isoFriends_online = "w18",
         w19_isoFriends_online = "w19",
         w20_isoFriends_online = "w20"
  )
```

``` r
data_new18 <- left_join(data_new17, data_ISO3, by=c("X", "Wave"))
```

``` r
data_ISO4 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(274:290),
    variable.name="Wave",
    value.name="ISO_OthPpl_online")
    
data_ISO4$Wave <- data_ISO4$Wave %>%
  recode(isoOthPpl_online = "w0",
         w1_isoOthPpl_online = "w1",
         w2_isoOthPpl_online = "w2",
         w3_isoOthPpl_online = "w3",
         w4_isoOthPpl_online = "w4",
         w5_isoOthPpl_online = "w5",
         w6_isoOthPpl_online = "w6",
         w7_isoOthPpl_online = "w7",
         w8_isoOthPpl_online = "w8",
         w9_isoOthPpl_online = "w9",
         w10_isoOthPpl_online = "w10",
         w11_isoOthPpl_online = "w11",
         w12_isoOthPpl_online = "w12",
         w13_isoOthPpl_online = "w13",
         w14_isoOthPpl_online = "w14",
         w15_isoOthPpl_online = "w15",
         w16_isoOthPpl_online = "w16",
         w17_isoOthPpl_online = "w17",
         w18_isoOthPpl_online = "w18",
         w19_isoOthPpl_online = "w19",
         w20_isoOthPpl_online = "w20"
  )
```

``` r
data_new19 <- left_join(data_new18, data_ISO4, by=c("X", "Wave"))
```

``` r
data_ISO5 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(291:293),
    variable.name="Wave",
    value.name="ISO_Obj")
    
data_ISO5$Wave <- data_ISO5$Wave %>%
  recode(
         w1_isoObj = "w1",
         w6_isoObj = "w6",
         w10_isoObj = "w10"
  )
```

``` r
data_new20 <- left_join(data_new19, data_ISO5, by=c("X", "Wave"))
```

``` r
data_ISO6 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(294:296),
    variable.name="Wave",
    value.name="ISO_Obj_who1")
    
data_ISO6$Wave <- data_ISO6$Wave %>%
  recode(
         w1_isoObjWho_1 = "w1",
         w6_isoObjWho_1 = "w6",
         w10_isoObjWho_1 = "w10"
  )
```

``` r
data_new21 <- left_join(data_new20, data_ISO6, by=c("X", "Wave"))
```

``` r
data_ISO7 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(297:299),
    variable.name="Wave",
    value.name="ISO_Obj_who2")
    
data_ISO7$Wave <- data_ISO7$Wave %>%
  recode(
         w1_isoObjWho_2 = "w1",
         w6_isoObjWho_2 = "w6",
         w10_isoObjWho_2 = "w10"
  )
```

``` r
data_new22 <- left_join(data_new21, data_ISO7, by=c("X", "Wave"))
```

``` r
data_ISO8 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(300:302),
    variable.name="Wave",
    value.name="ISO_Obj_who3")
    
data_ISO8$Wave <- data_ISO8$Wave %>%
  recode(
         w1_isoObjWho_3 = "w1",
         w6_isoObjWho_3 = "w6",
         w10_isoObjWho_3 = "w10"
  )
```

``` r
data_new23 <- left_join(data_new22, data_ISO8, by=c("X", "Wave"))
```

``` r
data_ISO9 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(303:305),
    variable.name="Wave",
    value.name="ISO_Obj_who4")
    
data_ISO9$Wave <- data_ISO9$Wave %>%
  recode(
         w1_isoObjWho_4 = "w1",
         w6_isoObjWho_4 = "w6",
         w10_isoObjWho_4 = "w10"
  )
```

``` r
data_new24 <- left_join(data_new23, data_ISO9, by=c("X", "Wave"))
```

``` r
data_ISO10 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(306:308),
    variable.name="Wave",
    value.name="ISO_Obj_who5")
    
data_ISO10$Wave <- data_ISO10$Wave %>%
  recode(
         w1_isoObjWho_5 = "w1",
         w6_isoObjWho_5 = "w6",
         w10_isoObjWho_5 = "w10"
  )
```

``` r
data_new25 <- left_join(data_new24, data_ISO10, by=c("X", "Wave"))
```

``` r
data_ISO11 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(309:311),
    variable.name="Wave",
    value.name="ISO_Obj_who6")
    
data_ISO11$Wave <- data_ISO11$Wave %>%
  recode(
         w1_isoObjWho_6 = "w1",
         w6_isoObjWho_6 = "w6",
         w10_isoObjWho_6 = "w10"
  )
```

``` r
data_new26 <- left_join(data_new25, data_ISO11, by=c("X", "Wave"))
```

``` r
data_RelSat <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(312:332),
    variable.name="Wave",
    value.name="RelSat")
    
data_RelSat$Wave <- data_RelSat$Wave %>%
  recode(persRelSat = "w0",
         w1_persRelSat = "w1",
         w2_persRelSat = "w2",
         w3_persRelSat = "w3",
         w4_persRelSat = "w4",
         w5_persRelSat = "w5",
         w6_persRelSat = "w6",
         w7_persRelSat = "w7",
         w8_persRelSat = "w8",
         w9_persRelSat = "w9",
         w10_persRelSat = "w10",
         w11_persRelSat = "w11",
         w12_persRelSat = "w12",
         w13_persRelSat = "w13",
         w14_persRelSat = "w14",
         w15_persRelSat = "w15",
         w16_persRelSat = "w16",
         w17_persRelSat = "w17",
         w18_persRelSat = "w18",
         w19_persRelSat = "w19",
         w20_persRelSat = "w20"
  )
```

``` r
data_new27 <- left_join(data_new26, data_RelSat, by=c("X", "Wave"))
```

``` r
data_Lone <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(333:353),
    variable.name="Wave",
    value.name="Lonely")
    
data_Lone$Wave <- data_Lone$Wave %>%
  recode(lone01 = "w0",
         w1_lone01 = "w1",
         w2_lone01 = "w2",
         w3_lone01 = "w3",
         w4_lone01 = "w4",
         w5_lone01 = "w5",
         w6_lone01 = "w6",
         w7_lone01 = "w7",
         w8_lone01 = "w8",
         w9_lone01 = "w9",
         w10_lone01 = "w10",
         w11_lone01 = "w11",
         w12_lone01 = "w12",
         w13_lone01 = "w13",
         w14_lone01 = "w14",
         w15_lone01 = "w15",
         w16_lone01 = "w16",
         w17_lone01 = "w17",
         w18_lone01 = "w18",
         w19_lone01 = "w19",
         w20_lone01 = "w20"
  )
```

``` r
data_new28 <- left_join(data_new27, data_Lone, by=c("X", "Wave"))
```

``` r
data_MentHealth <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(354:373),
    variable.name="Wave",
    value.name="MentHealth")
    
data_MentHealth$Wave <- data_MentHealth$Wave %>%
  recode(
         w1_mentHealth = "w1",
         w2_mentHealth = "w2",
         w3_mentHealth = "w3",
         w4_mentHealth = "w4",
         w5_mentHealth = "w5",
         w6_mentHealth = "w6",
         w7_mentHealth = "w7",
         w8_mentHealth = "w8",
         w9_mentHealth = "w9",
         w10_mentHealth = "w10",
         w11_mentHealth = "w11",
         w12_mentHealth = "w12",
         w13_mentHealth = "w13",
         w14_mentHealth = "w14",
         w15_mentHealth = "w15",
         w16_mentHealth = "w16",
         w17_mentHealth = "w17",
         w18_mentHealth = "w18",
         w19_mentHealth = "w19",
         w20_mentHealth = "w20"
  )
```

``` r
data_new29 <- left_join(data_new28, data_MentHealth, by=c("X", "Wave"))
```

``` r
data_PhysHealth <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(374:385),
    variable.name="Wave",
    value.name="PhysHealth")

data_PhysHealth$Wave <- data_PhysHealth$Wave %>%
  recode(
         w1_mentPhys = "w1",
         w5_mentPhys = "w5",
         w11_mentPhys = "w11",
         w12_mentPhys = "w12",
         w13_mentPhys = "w13",
         w14_mentPhys = "w14",
         w15_mentPhys = "w15",
         w16_mentPhys = "w16",
         w17_mentPhys = "w17",
         w18_mentPhys = "w18",
         w19_mentPhys = "w19",
         w20_mentPhys = "w20"
  )
```

``` r
data_new30 <- left_join(data_new29, data_PhysHealth, by=c("X", "Wave"))
```

``` r
data_Happy <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(386:400),
    variable.name="Wave",
    value.name="Happy")

data_Happy$Wave <- data_Happy$Wave %>%
  recode(happy = "w0",
         w4_happy = "w4",
         w5_happy = "w5",
         w8_happy = "w8",
         w9_happy = "w9",
         w11_happy = "w11",
         w12_happy = "w12",
         w13_happy = "w13",
         w14_happy = "w14",
         w15_happy = "w15",
         w16_happy = "w16",
         w17_happy = "w17",
         w18_happy = "w18",
         w19_happy = "w19",
         w20_happy = "w20"
  )
```

``` r
data_new31 <- left_join(data_new30, data_Happy, by=c("X", "Wave"))
```

``` r
data_coronaClose1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(402:417),
    variable.name="Wave",
    value.name="CoronaClose1")

data_coronaClose1$Wave <- data_coronaClose1$Wave %>%
  recode( coronaClose_1 = "w0",
         w1_coronaClose_1 = "w1",
         w2_coronaClose_1 = "w2",
         w3_coronaClose_1 = "w3",
         w4_coronaClose_1 = "w4",
         w5_coronaClose_1 = "w5",
         w6_coronaClose_1 = "w6",
         w7_coronaClose_1 = "w7",
         w8_coronaClose_1 = "w8",
         w9_coronaClose_1 = "w9",
         w10_coronaClose_1 = "w10",
         w11_coronaClose_1 = "w11",
         w12_coronaClose_1 = "w12",
         w13_coronaClose_1 = "w13",
         w14_coronaClose_1 = "w14",
         w15_coronaClose_1 = "w15",
         w16_coronaClose_1 = "w16",
  )
```

``` r
data_new32 <- left_join(data_new31, data_coronaClose1, by=c("X", "Wave"))
```

``` r
data_coronaClose2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(418:434),
    variable.name="Wave",
    value.name="CoronaClose2")

data_coronaClose2$Wave <- data_coronaClose2$Wave %>%
  recode( coronaClose_2 = "w0",
         w1_coronaClose_2 = "w1",
         w2_coronaClose_2 = "w2",
         w3_coronaClose_2 = "w3",
         w4_coronaClose_2 = "w4",
         w5_coronaClose_2 = "w5",
         w6_coronaClose_2 = "w6",
         w7_coronaClose_2 = "w7",
         w8_coronaClose_2 = "w8",
         w9_coronaClose_2 = "w9",
         w10_coronaClose_2 = "w10",
         w11_coronaClose_2 = "w11",
         w12_coronaClose_2 = "w12",
         w13_coronaClose_2 = "w13",
         w14_coronaClose_2 = "w14",
         w15_coronaClose_2 = "w15",
         w16_coronaClose_2 = "w16",
  )
```

``` r
data_new33 <- left_join(data_new32, data_coronaClose2, by=c("X", "Wave"))
```

``` r
data_coronaClose3 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(435:451),
    variable.name="Wave",
    value.name="CoronaClose3")

data_coronaClose3$Wave <- data_coronaClose3$Wave %>%
  recode( coronaClose_3 = "w0",
         w1_coronaClose_3 = "w1",
         w2_coronaClose_3 = "w2",
         w3_coronaClose_3 = "w3",
         w4_coronaClose_3 = "w4",
         w5_coronaClose_3 = "w5",
         w6_coronaClose_3 = "w6",
         w7_coronaClose_3 = "w7",
         w8_coronaClose_3 = "w8",
         w9_coronaClose_3 = "w9",
         w10_coronaClose_3 = "w10",
         w11_coronaClose_3 = "w11",
         w12_coronaClose_3 = "w12",
         w13_coronaClose_3 = "w13",
         w14_coronaClose_3 = "w14",
         w15_coronaClose_3 = "w15",
         w16_coronaClose_3 = "w16",
  )
```

``` r
data_new34 <- left_join(data_new33, data_coronaClose3, by=c("X", "Wave"))
```

``` r
data_coronaClose4 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(452:468),
    variable.name="Wave",
    value.name="CoronaClose4")

data_coronaClose4$Wave <- data_coronaClose4$Wave %>%
  recode( coronaClose_4 = "w0",
         w1_coronaClose_4 = "w1",
         w2_coronaClose_4 = "w2",
         w3_coronaClose_4 = "w3",
         w4_coronaClose_4 = "w4",
         w5_coronaClose_4 = "w5",
         w6_coronaClose_4 = "w6",
         w7_coronaClose_4 = "w7",
         w8_coronaClose_4 = "w8",
         w9_coronaClose_4 = "w9",
         w10_coronaClose_4 = "w10",
         w11_coronaClose_4 = "w11",
         w12_coronaClose_4 = "w12",
         w13_coronaClose_4 = "w13",
         w14_coronaClose_4 = "w14",
         w15_coronaClose_4 = "w15",
         w16_coronaClose_4 = "w16",
  )
```

``` r
data_new35 <- left_join(data_new34, data_coronaClose4, by=c("X", "Wave"))
```

``` r
data_coronaClose5 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(469:485),
    variable.name="Wave",
    value.name="CoronaClose5")

data_coronaClose5$Wave <- data_coronaClose5$Wave %>%
  recode( coronaClose_5 = "w0",
         w1_coronaClose_5 = "w1",
         w2_coronaClose_5 = "w2",
         w3_coronaClose_5 = "w3",
         w4_coronaClose_5 = "w4",
         w5_coronaClose_5 = "w5",
         w6_coronaClose_5 = "w6",
         w7_coronaClose_5 = "w7",
         w8_coronaClose_5 = "w8",
         w9_coronaClose_5 = "w9",
         w10_coronaClose_5 = "w10",
         w11_coronaClose_5 = "w11",
         w12_coronaClose_5 = "w12",
         w13_coronaClose_5 = "w13",
         w14_coronaClose_5 = "w14",
         w15_coronaClose_5 = "w15",
         w16_coronaClose_5 = "w16",
  )
```

``` r
data_new36 <- left_join(data_new35, data_coronaClose5, by=c("X", "Wave"))
```

``` r
data_coronaClose6 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(486:502),
    variable.name="Wave",
    value.name="CoronaClose6")

data_coronaClose6$Wave <- data_coronaClose6$Wave %>%
  recode( coronaClose_6 = "w0",
         w1_coronaClose_6 = "w1",
         w2_coronaClose_6 = "w2",
         w3_coronaClose_6 = "w3",
         w4_coronaClose_6 = "w4",
         w5_coronaClose_6 = "w5",
         w6_coronaClose_6 = "w6",
         w7_coronaClose_6 = "w7",
         w8_coronaClose_6 = "w8",
         w9_coronaClose_6 = "w9",
         w10_coronaClose_6 = "w10",
         w11_coronaClose_6 = "w11",
         w12_coronaClose_6 = "w12",
         w13_coronaClose_6 = "w13",
         w14_coronaClose_6 = "w14",
         w15_coronaClose_6 = "w15",
         w16_coronaClose_6 = "w16",
  )
```

``` r
data_new37 <- left_join(data_new36, data_coronaClose6, by=c("X", "Wave"))
```

``` r
data_HouseLeave <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(503:523),
    variable.name="Wave",
    value.name="HouseLeave")

data_HouseLeave$Wave <- data_HouseLeave$Wave %>%
  recode(houseLeave = "w0",
         w1_houseLeave = "w1",
         w2_houseLeave = "w2",
         w3_houseLeave = "w3",
         w4_houseLeave = "w4",
         w5_houseLeave = "w5",
         w6_houseLeave = "w6",
         w7_houseLeave = "w7",
         w8_houseLeave = "w8",
         w9_houseLeave = "w9",
         w10_houseLeave = "w10",
         w11_houseLeave = "w11",
         w12_houseLeave = "w12",
         w13_houseLeave = "w13",
         w14_houseLeave = "w14",
         w15_houseLeave = "w15",
         w16_houseLeave = "w16",
         w17_houseLeave = "w17",
         w18_houseLeave = "w18",
         w19_houseLeave = "w19",
         w20_houseLeave = "w20"
  )
```

``` r
data_new38 <- left_join(data_new37, data_HouseLeave, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(524:544),
    variable.name="Wave",
    value.name="HouseLeaveWhy1")

data_HouseLeaveWhy1$Wave <- data_HouseLeaveWhy1$Wave %>%
  recode(houseLeaveWhy_1 = "w0",
         w1_houseLeaveWhy_1 = "w1",
         w2_houseLeaveWhy_1 = "w2",
         w3_houseLeaveWhy_1 = "w3",
         w4_houseLeaveWhy_1 = "w4",
         w5_houseLeaveWhy_1 = "w5",
         w6_houseLeaveWhy_1 = "w6",
         w7_houseLeaveWhy_1 = "w7",
         w8_houseLeaveWhy_1 = "w8",
         w9_houseLeaveWhy_1 = "w9",
         w10_houseLeaveWhy_1 = "w10",
         w11_houseLeaveWhy_1 = "w11",
         w12_houseLeaveWhy_1 = "w12",
         w13_houseLeaveWhy_1 = "w13",
         w14_houseLeaveWhy_1 = "w14",
         w15_houseLeaveWhy_1 = "w15",
         w16_houseLeaveWhy_1 = "w16",
         w17_houseLeaveWhy_1 = "w17",
         w18_houseLeaveWhy_1 = "w18",
         w19_houseLeaveWhy_1 = "w19",
         w20_houseLeaveWhy_1 = "w20"
  )
```

``` r
data_new39 <- left_join(data_new38, data_HouseLeaveWhy1, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(545:565),
    variable.name="Wave",
    value.name="HouseLeaveWhy2")

data_HouseLeaveWhy2$Wave <- data_HouseLeaveWhy2$Wave %>%
  recode(houseLeaveWhy_2 = "w0",
         w1_houseLeaveWhy_2 = "w1",
         w2_houseLeaveWhy_2 = "w2",
         w3_houseLeaveWhy_2 = "w3",
         w4_houseLeaveWhy_2 = "w4",
         w5_houseLeaveWhy_2 = "w5",
         w6_houseLeaveWhy_2 = "w6",
         w7_houseLeaveWhy_2 = "w7",
         w8_houseLeaveWhy_2 = "w8",
         w9_houseLeaveWhy_2 = "w9",
         w10_houseLeaveWhy_2 = "w10",
         w11_houseLeaveWhy_2 = "w11",
         w12_houseLeaveWhy_2 = "w12",
         w13_houseLeaveWhy_2 = "w13",
         w14_houseLeaveWhy_2 = "w14",
         w15_houseLeaveWhy_2 = "w15",
         w16_houseLeaveWhy_2 = "w16",
         w17_houseLeaveWhy_2 = "w17",
         w18_houseLeaveWhy_2 = "w18",
         w19_houseLeaveWhy_2 = "w19",
         w20_houseLeaveWhy_2 = "w20"
  )
```

``` r
data_new40 <- left_join(data_new39, data_HouseLeaveWhy2, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy4 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(586:606),
    variable.name="Wave",
    value.name="HouseLeaveWhy4")

data_HouseLeaveWhy4$Wave <- data_HouseLeaveWhy4$Wave %>%
  recode(houseLeaveWhy_4 = "w0",
         w1_houseLeaveWhy_4 = "w1",
         w2_houseLeaveWhy_4 = "w2",
         w3_houseLeaveWhy_4 = "w3",
         w4_houseLeaveWhy_4 = "w4",
         w5_houseLeaveWhy_4 = "w5",
         w6_houseLeaveWhy_4 = "w6",
         w7_houseLeaveWhy_4 = "w7",
         w8_houseLeaveWhy_4 = "w8",
         w9_houseLeaveWhy_4 = "w9",
         w10_houseLeaveWhy_4 = "w10",
         w11_houseLeaveWhy_4 = "w11",
         w12_houseLeaveWhy_4 = "w12",
         w13_houseLeaveWhy_4 = "w13",
         w14_houseLeaveWhy_4 = "w14",
         w15_houseLeaveWhy_4 = "w15",
         w16_houseLeaveWhy_4 = "w16",
         w17_houseLeaveWhy_4 = "w17",
         w18_houseLeaveWhy_4 = "w18",
         w19_houseLeaveWhy_4 = "w19",
         w20_houseLeaveWhy_4 = "w20"
  )
```

``` r
data_new41 <- left_join(data_new40, data_HouseLeaveWhy4, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy6 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(628:648),
    variable.name="Wave",
    value.name="HouseLeaveWhy6")

data_HouseLeaveWhy6$Wave <- data_HouseLeaveWhy6$Wave %>%
  recode(houseLeaveWhy_6 = "w0",
         w1_houseLeaveWhy_6 = "w1",
         w2_houseLeaveWhy_6 = "w2",
         w3_houseLeaveWhy_6 = "w3",
         w4_houseLeaveWhy_6 = "w4",
         w5_houseLeaveWhy_6 = "w5",
         w6_houseLeaveWhy_6 = "w6",
         w7_houseLeaveWhy_6 = "w7",
         w8_houseLeaveWhy_6 = "w8",
         w9_houseLeaveWhy_6 = "w9",
         w10_houseLeaveWhy_6 = "w10",
         w11_houseLeaveWhy_6 = "w11",
         w12_houseLeaveWhy_6 = "w12",
         w13_houseLeaveWhy_6 = "w13",
         w14_houseLeaveWhy_6 = "w14",
         w15_houseLeaveWhy_6 = "w15",
         w16_houseLeaveWhy_6 = "w16",
         w17_houseLeaveWhy_6 = "w17",
         w18_houseLeaveWhy_6 = "w18",
         w19_houseLeaveWhy_6 = "w19",
         w20_houseLeaveWhy_6 = "w20"
  )
```

``` r
data_new42 <- left_join(data_new41, data_HouseLeaveWhy6, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy7 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(607:627),
    variable.name="Wave",
    value.name="HouseLeaveWhy7")

data_HouseLeaveWhy7$Wave <- data_HouseLeaveWhy7$Wave %>%
  recode(houseLeaveWhy_7 = "w0",
         w1_houseLeaveWhy_7 = "w1",
         w2_houseLeaveWhy_7 = "w2",
         w3_houseLeaveWhy_7 = "w3",
         w4_houseLeaveWhy_7 = "w4",
         w5_houseLeaveWhy_7 = "w5",
         w6_houseLeaveWhy_7 = "w6",
         w7_houseLeaveWhy_7 = "w7",
         w8_houseLeaveWhy_7 = "w8",
         w9_houseLeaveWhy_7 = "w9",
         w10_houseLeaveWhy_7 = "w10",
         w11_houseLeaveWhy_7 = "w11",
         w12_houseLeaveWhy_7 = "w12",
         w13_houseLeaveWhy_7 = "w13",
         w14_houseLeaveWhy_7 = "w14",
         w15_houseLeaveWhy_7 = "w15",
         w16_houseLeaveWhy_7 = "w16",
         w17_houseLeaveWhy_7 = "w17",
         w18_houseLeaveWhy_7 = "w18",
         w19_houseLeaveWhy_7 = "w19",
         w20_houseLeaveWhy_7 = "w20"
  )
```

``` r
data_new43 <- left_join(data_new42, data_HouseLeaveWhy7, by=c("X", "Wave"))
```

``` r
data_HouseLeaveWhy8 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(566:586),
    variable.name="Wave",
    value.name="HouseLeaveWhy8")

data_HouseLeaveWhy8$Wave <- data_HouseLeaveWhy8$Wave %>%
  recode(houseLeaveWhy_8 = "w0",
         w1_houseLeaveWhy_8 = "w1",
         w2_houseLeaveWhy_8 = "w2",
         w3_houseLeaveWhy_8 = "w3",
         w4_houseLeaveWhy_8 = "w4",
         w5_houseLeaveWhy_8 = "w5",
         w6_houseLeaveWhy_8 = "w6",
         w7_houseLeaveWhy_8 = "w7",
         w8_houseLeaveWhy_8 = "w8",
         w9_houseLeaveWhy_8 = "w9",
         w10_houseLeaveWhy_8 = "w10",
         w11_houseLeaveWhy_8 = "w11",
         w12_houseLeaveWhy_8 = "w12",
         w13_houseLeaveWhy_8 = "w13",
         w14_houseLeaveWhy_8 = "w14",
         w15_houseLeaveWhy_8 = "w15",
         w16_houseLeaveWhy_8 = "w16",
         w17_houseLeaveWhy_8 = "w17",
         w18_houseLeaveWhy_8 = "w18",
         w19_houseLeaveWhy_8 = "w19",
         w20_houseLeaveWhy_8 = "w20"
  )
```

``` r
data_new44 <- left_join(data_new43, data_HouseLeaveWhy8, by=c("X", "Wave"))
```

``` r
data_Neuro1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(649:659),
    variable.name="Wave",
    value.name="Neuro1")

data_Neuro1$Wave <- data_Neuro1$Wave %>%
  recode(neuro01 = "w0",
         w6_neuro01 = "w6",
         w10_neuro01 = "w10",
         w12_neuro01 = "w12",
         w14_neuro01 = "w14",
         w15_neuro01 = "w15",
         w16_neuro01 = "w16",
         w17_neuro01 = "w17",
         w18_neuro01 = "w18",
         w19_neuro01 = "w19",
         w20_neuro01 = "w20"
  )
```

``` r
data_new45 <- left_join(data_new44, data_Neuro1, by=c("X", "Wave"))
```

``` r
data_Neuro1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(649:659),
    variable.name="Wave",
    value.name="Neuro1")

data_Neuro1$Wave <- data_Neuro1$Wave %>%
  recode(neuro01 = "w0",
         w6_neuro01 = "w6",
         w10_neuro01 = "w10",
         w12_neuro01 = "w12",
         w14_neuro01 = "w14",
         w15_neuro01 = "w15",
         w16_neuro01 = "w16",
         w17_neuro01 = "w17",
         w18_neuro01 = "w18",
         w19_neuro01 = "w19",
         w20_neuro01 = "w20"
  )
```

``` r
data_new45 <- left_join(data_new44, data_Neuro1, by=c("X", "Wave"))
```

``` r
data_Neuro2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(660:670),
    variable.name="Wave",
    value.name="Neuro2")

data_Neuro2$Wave <- data_Neuro2$Wave %>%
  recode(neuro02 = "w0",
         w6_neuro02 = "w6",
         w10_neuro02 = "w10",
         w12_neuro02 = "w12",
         w14_neuro02 = "w14",
         w15_neuro02 = "w15",
         w16_neuro02 = "w16",
         w17_neuro02 = "w17",
         w18_neuro02 = "w18",
         w19_neuro02 = "w19",
         w20_neuro02 = "w20"
  )
```

``` r
data_new46 <- left_join(data_new45, data_Neuro2, by=c("X", "Wave"))
```

``` r
data_Neuro3 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c(671:681),
    variable.name="Wave",
    value.name="Neuro3")

data_Neuro3$Wave <- data_Neuro3$Wave %>%
  recode(neuro03 = "w0",
         w6_neuro03 = "w6",
         w10_neuro03 = "w10",
         w12_neuro03 = "w12",
         w14_neuro03 = "w14",
         w15_neuro03 = "w15",
         w16_neuro03 = "w16",
         w17_neuro03 = "w17",
         w18_neuro03 = "w18",
         w19_neuro03 = "w19",
         w20_neuro03 = "w20"
  )
```

``` r
data_new47 <- left_join(data_new46, data_Neuro3, by=c("X", "Wave"))
```

``` r
data_long <- data_new47

save(data_long, file="data_long.Rdata")
```
