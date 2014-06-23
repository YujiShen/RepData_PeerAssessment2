

# NOAA Storm Data Analysis

## Synopsis
This document is an elementary data analysis for the NOAA storm data from 1950-2011. It focuses on two points: 

1. Population casualties
2. Economic damage

I will do some simple processing work on the data, and use the plot to present my result.

## Introduction
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) [47Mb]

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
* National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

## Data Processing
Set the working directory.

```r
setwd("/Users/Yuji/Documents/R/RepData/Peer2/")
```
Download the file and read into R.

```r
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "/Users/Yuji/Documents/R/RepData/Peer2/StormData.csv.bz2")
data <- read.csv("StormData.csv.bz2")
```

Look at the data.

```r
dim(data)
```

```
## [1] 902297     37
```

```r
library(plyr)
nmissing <- function(x) sum(is.na(x))
colwise(nmissing)(data)
```

```
##   STATE__ BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE EVTYPE
## 1       0        0        0         0      0          0     0      0
##   BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN
## 1         0       0          0        0        0          0     902297
##   END_RANGE END_AZI END_LOCATI LENGTH WIDTH      F MAG FATALITIES INJURIES
## 1         0       0          0      0     0 843563   0          0        0
##   PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES LATITUDE
## 1       0          0       0          0   0          0         0       47
##   LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1         0         40          0       0      0
```

### Data Transformation
Transfer the `EVTYPE`,`PROPDMGEXP` and `CROPDMGEXP`  to uppercase for aggregation.
Transfer the `BGN_DATE` to date time format for future work.

```r
#data <- mutate(data, EVTYPE = toupper(EVTYPE), PROPDMGEXP = toupper(PROPDMGEXP), CROPDMGEXP = toupper(CROPDMGEXP), BGN_DATE = as.POSIXlt(data$BGN_DATE, format="%m/%d/%Y %H:%M:%S"))
```

## Calculation
Sum the `FATALITIES` and `INJURIES` by `EVTYPE`, and get the top 10 Harmful types.

```r
DeathInjury <- ddply(data, .(EVTYPE), summarize, TotalHarm = sum(FATALITIES + INJURIES))
DeathInjury <- DeathInjury[order(DeathInjury$TotalHarm, decreasing = T), ]
TopHarm <- DeathInjury[1:10, ]
```


Sum the `PROPDMG` by `EVTYPE` and `PROPDMGEXP`. Then calculate real property damage by accounting `PROPDMGEXP`. Last step, sum the new property damage data by `EVTYPE`

```r
prop <- ddply(data, .(EVTYPE, PROPDMGEXP), summarize, PROPDMG = sum(PROPDMG))
prop <- mutate(prop, PropertyDamage = ifelse(toupper(PROPDMGEXP) =='K', PROPDMG*1000, ifelse(toupper(PROPDMGEXP) =='M', PROPDMG*1000000, ifelse(toupper(PROPDMGEXP) == 'B', PROPDMG*1000000000, ifelse(toupper(PROPDMGEXP) == 'H', PROPDMG*100, PROPDMG)))))
prop <- subset(prop, select = c("EVTYPE", "PropertyDamage"))
prop.total <- ddply(prop, .(EVTYPE), summarize, TotalPropDamage = sum(PropertyDamage))
```


Sum the `CROPDMG` by `EVTYPE` and `CROPDMGEXP`. Then calculate real crop damage by accounting `CROPDMGEXP`. Last step, sum the new crop damage data by `EVTYPE`

```r
crop <- ddply(data, .(EVTYPE, CROPDMGEXP), summarize, CROPDMG = sum(CROPDMG))
crop <- mutate(crop, CropDamage = ifelse(toupper(CROPDMGEXP) =='K', CROPDMG*1000, ifelse(toupper(CROPDMGEXP) =='M', CROPDMG*1000000, ifelse(toupper(CROPDMGEXP) == 'B', CROPDMG*1000000000, ifelse(toupper(CROPDMGEXP) == 'H', CROPDMG*100, CROPDMG)))))
crop <- subset(crop, select = c("EVTYPE", "CropDamage"))
crop.total <- ddply(crop, .(EVTYPE), summarize, TotalCropDamage = sum(CropDamage))
```

Merge the property damage data and crop damage, and select the top ten damage.

```r
damage <- merge(prop.total, crop.total, by="EVTYPE")
damage <- mutate(damage, TotalDamage = TotalPropDamage + TotalCropDamage)
damage <- damage[order(damage$TotalDamage, decreasing = T), ]
TopDamage <- damage[1:10, ]
```

## Result
### 1.Population casualities
This is the result of top 10 harmful type base on the sum of casualties.

```r
TopHarm
```

```
##                EVTYPE TotalHarm
## 834           TORNADO     96979
## 130    EXCESSIVE HEAT      8428
## 856         TSTM WIND      7461
## 170             FLOOD      7259
## 464         LIGHTNING      6046
## 275              HEAT      3037
## 153       FLASH FLOOD      2755
## 427         ICE STORM      2064
## 760 THUNDERSTORM WIND      1621
## 972      WINTER STORM      1527
```
This is the plot base on previous data.

```r
library(ggplot2)
p <- qplot(EVTYPE, TotalHarm, data = TopHarm, stat='identity',geom = "bar", fill= EVTYPE,xlab="Top 10 events",ylab="Casualties",main="Casualties due to severe weather events\nin the U.S from 1950-2011")
p + theme(axis.text.x = element_text(angle = 45))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

### 2.Economic damage
This is the result of top 10 harmful type base on the sum of damage.

```r
TopDamage
```

```
##                EVTYPE TotalPropDamage TotalCropDamage TotalDamage
## 170             FLOOD       1.447e+11       5.662e+09   1.503e+11
## 411 HURRICANE/TYPHOON       6.931e+10       2.608e+09   7.191e+10
## 834           TORNADO       5.694e+10       4.150e+08   5.735e+10
## 670       STORM SURGE       4.332e+10       5.000e+03   4.332e+10
## 244              HAIL       1.573e+10       3.026e+09   1.876e+10
## 153       FLASH FLOOD       1.614e+10       1.421e+09   1.756e+10
## 95            DROUGHT       1.046e+09       1.397e+10   1.502e+10
## 402         HURRICANE       1.187e+10       2.742e+09   1.461e+10
## 590       RIVER FLOOD       5.119e+09       5.029e+09   1.015e+10
## 427         ICE STORM       3.945e+09       5.022e+09   8.967e+09
```
This is the plot base on previous data.

```r
p <- qplot(EVTYPE, TotalDamage, data = TopDamage, stat='identity',geom = "bar", fill= EVTYPE,xlab="Top 10 events",ylab="Economic damage",main="Economic damage due to severe weather events\nin the U.S from 1950-2011")
p + theme(axis.text.x = element_text(angle = 45))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

## Future work
If have chance and time, I will explore the damage and casualties based on state and time attributes.
