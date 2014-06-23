
setwd("/Users/Yuji/Documents/R/RepData/Peer2/")

# Download the file and read into R.

download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "/Users/Yuji/Documents/R/RepData/Peer2/StormData.csv.bz2")
data <- read.csv("StormData.csv.bz2")


# Look at the data.

dim(data)
library(plyr)
nmissing <- function(x) sum(is.na(x))
colwise(nmissing)(data)


### Data Transformation
# Transfer the `EVTYPE`,`PROPDMGEXP` and `CROPDMGEXP`  to uppercase for aggregation.
# Transfer the `BGN_DATE` to date time format for future work.

data <- mutate(data, EVTYPE = toupper(EVTYPE), PROPDMGEXP = toupper(PROPDMGEXP), CROPDMGEXP = toupper(CROPDMGEXP), BGN_DATE = as.POSIXlt(BGN_DATE, format="%Y-%m-%d"))


## Calculation
# Sum the `FATALITIES` and `INJURIES` by `EVTYPE`, and get the top 10 Harmful types.

DeathInjury <- ddply(data, .(EVTYPE), summarize, TotalHarm = sum(FATALITIES + INJURIES))
DeathInjury <- DeathInjury[order(DeathInjury$TotalHarm, decreasing = T), ]
TopHarm <- DeathInjury[1:10, ]

# Sum the `PROPDMG` by `EVTYPE` and `PROPDMGEXP`. Then calculate real property damage by accounting `PROPDMGEXP`. Last step, sum the new property damage data by `EVTYPE`

prop <- ddply(data, .(EVTYPE, PROPDMGEXP), summarize, PROPDMG = sum(PROPDMG))
prop <- mutate(prop, PropertyDamage = ifelse(toupper(PROPDMGEXP) =='K', PROPDMG*1000, ifelse(toupper(PROPDMGEXP) =='M', PROPDMG*1000000, ifelse(toupper(PROPDMGEXP) == 'B', PROPDMG*1000000000, ifelse(toupper(PROPDMGEXP) == 'H', PROPDMG*100, PROPDMG)))))
prop <- subset(prop, select = c("EVTYPE", "PropertyDamage"))
prop.total <- ddply(prop, .(EVTYPE), summarize, TotalPropDamage = sum(PropertyDamage))

# Sum the `CROPDMG` by `EVTYPE` and `CROPDMGEXP`. Then calculate real crop damage by accounting `CROPDMGEXP`. Last step, sum the new crop damage data by `EVTYPE`

crop <- ddply(data, .(EVTYPE, CROPDMGEXP), summarize, CROPDMG = sum(CROPDMG))
crop <- mutate(crop, CropDamage = ifelse(toupper(CROPDMGEXP) =='K', CROPDMG*1000, ifelse(toupper(CROPDMGEXP) =='M', CROPDMG*1000000, ifelse(toupper(CROPDMGEXP) == 'B', CROPDMG*1000000000, ifelse(toupper(CROPDMGEXP) == 'H', CROPDMG*100, CROPDMG)))))
crop <- subset(crop, select = c("EVTYPE", "CropDamage"))
crop.total <- ddply(crop, .(EVTYPE), summarize, TotalCropDamage = sum(CropDamage))


# Merge the property damage data and crop damage, and select the top ten damage.

damage <- merge(prop.total, crop.total, by="EVTYPE")
damage <- mutate(damage, TotalDamage = TotalPropDamage + TotalCropDamage)
damage <- damage[order(damage$TotalDamage, decreasing = T), ]
TopDamage <- damage[1:10, ]


## Result
### 1.Population casualities
#This is the result of top 10 harmful type base on the sum of casualties.

TopHarm

# This is the plot base on previous data.

library(ggplot2)
p <- qplot(EVTYPE, TotalHarm, data = TopHarm, stat='identity',geom = "bar", fill= EVTYPE,xlab="Top 10 events",ylab="Casualties",main="Casualties due to severe weather events\nin the U.S from 1950-2011")
p + theme(axis.text.x = element_text(angle = 45))


### 2.Economic damage
# This is the result of top 10 harmful type base on the sum of damage.

TopDamage

# This is the plot base on previous data.

p <- qplot(EVTYPE, TotalDamage, data = TopDamage, stat='identity',geom = "bar", fill= EVTYPE,xlab="Top 10 events",ylab="Economic damage",main="Economic damage due to severe weather events\nin the U.S from 1950-2011")
p + theme(axis.text.x = element_text(angle = 45))