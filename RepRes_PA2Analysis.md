TODO - Title that briefly summarises the data analysis
========================================================

> Synopsis which describes and summarizes analysis in < 10 sentences

## Section 1: Data Processing TODO
The data for this analysis is created by the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States.

This database is made available as part of the Coursera course Reproduceable research and was downloaded on 24 May 2014 ([link](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)) and loaded using the following code.


```r
# download data - if needed
dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists("../PA2_rawdata/repdata-data-StormData.csv.bz2")) {
    download.file(url = dataURL, destfile = "../PA2_rawdata/repdata-data-StormData.csv.bz2", 
        method = "curl")
    print("File downloaded")
} else {
    print("File was already downloaded")
}
```

```
## [1] "File was already downloaded"
```

```r

# load raw data - if needed
if (!exists("rawdata")) {
    rawdata <- read.csv("../PA2_rawdata//repdata-data-StormData.csv.bz2")
}
```


This database provides 902297 event logs, each with an event classification code (in the original dataset as "EVDATA"). For the purpose of this analysis a simple cleaning procedure was followed.



```r
# duplicate dataset
stormdata <- rawdata

# create a modified events column (event)
stormdata$event <- rawdata$EVTYPE

# lowercase all
stormdata$event <- tolower(stormdata$event)

# convert to factor
stormdata$event <- as.factor(stormdata$event)

# remove all event summary events
stormdata <- stormdata[grep(pattern = "^summary", x = stormdata$event, invert = TRUE), 
    ]

# remove all events without economic or population health impacts
stormdata <- subset(x = stormdata, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 
    0 | CROPDMG > 0)

# combinations

# converting property damage using multiples

# stormdata$propertydamage <- stormdata$PROPDMG
# stormdata[stormdata$PROPDMGEXP = 'K']
```


This cleaning procedure took the following steps

1. Converting all event type descriptions to lower case: reduce 488 event types to 447 event types.

2. Removing events which represented "event summaries, rather than particular events: removed 75 records or 66 events.

3. Removing all event records where no fatalities, injuries, property or crop damage occured: removed 647664 records.

4. 



## Section 2: Results TODO
The NOAA storm database provides information on: (1) the population health impacts as measured by direct fatalities and injuries, and (2) direct or estimated econonomic impacts measured in property damage dollars associated with hydro-meteorological events.




TODO - Identify which events (identified by EVTYPE) are most harmful with respect to population health


```r
library(plyr)

consequences <- ddply(rawdata, c("EVTYPE"), summarize, deaths = sum(FATALITIES, 
    na.rm = TRUE), injuries = sum(INJURIES, na.rm = TRUE), property_damage = sum(PROPDMG, 
    na.rm = TRUE), crop_damage = sum(CROPDMG, na.rm = TRUE))
```



TODO - Identify which types of events have the greatest economic consequences

TODO - Figure 1
TODO - Figure 2
TODO - Figure 3

TODO - link to github
