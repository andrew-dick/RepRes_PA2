Tornados most damaging weather event in the United states
========================================================
> This study examines the population health and economic impacts associated with a range of major storm and weather events within the united states as recorded within the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. The findings of this report are that >80% of the mortality and economic consequences can be attributed to 11 event types, and that the #1 cause of deaths, injuries and economic losses in the United States is tornados. 

## Section 1: Data Processing
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

# create and convert property damage using multiples

stormdata$propertydamage <- 0
stormdata[stormdata$PROPDMGEXP == "K", ]$propertydamage <- stormdata[stormdata$PROPDMGEXP == 
    "K", ]$PROPDMG * 1000
stormdata[stormdata$PROPDMGEXP == "M", ]$propertydamage <- stormdata[stormdata$PROPDMGEXP == 
    "M", ]$PROPDMG * 1e+06

# create and convert crop damage using multiples
stormdata$cropdamage <- 0
stormdata[stormdata$CROPDMGEXP == "K", ]$cropdamage <- stormdata[stormdata$CROPDMGEXP == 
    "K", ]$CROPDMG * 1000
stormdata[stormdata$CROPDMGEXP == "M", ]$cropdamage <- stormdata[stormdata$CROPDMGEXP == 
    "M", ]$CROPDMG * 1e+06

# create total damage variable
stormdata$totaldamage <- stormdata$propertydamage + stormdata$cropdamage
```


This cleaning procedure took the following steps

1. Converting all event type descriptions to lower case: reduce 488 event types to 447 event types.

2. Removing events which represented "event summaries, rather than particular events: removed 75 records or 66 events.

3. Removing all event records where no fatalities, injuries, property or crop damage occured: removed 647664 records.

4. Converting PROPDMG and CROPDMG variable to propertydamage and cropdamage (in USD), multiplying by 1,000 or 1,000,000 where appropriate according to the "K" or "M" values of PROPDMGEXP (other values were set as 0)

5. Creating a totaldamage variable which is the sum of propertydamage and cropdamage

## Section 2: Results
The NOAA storm database provides information on: (1) the population health impacts as measured by direct fatalities and injuries, and (2) direct or estimated econonomic impacts measured in property damage dollars associated with hydro-meteorological events.


```r
library(plyr)

consequences <- ddply(stormdata, c("event"), summarize, deaths = sum(FATALITIES, 
    na.rm = TRUE), injuries = sum(INJURIES, na.rm = TRUE), property_damage = sum(propertydamage, 
    na.rm = TRUE), crop_damage = sum(CROPDMG, na.rm = TRUE), total_damage = sum(totaldamage, 
    na.rm = TRUE))
```


### 2.1: Population Health Impacts
The NOAA database records 1.5145 &times; 10<sup>4</sup> deaths and 1.4053 &times; 10<sup>5</sup> injuries associated with hydro-meteorological events.
 

```r
# take death statistics
death_analysis <- consequences[with(consequences, order(-deaths)), ]
# remove other columns
death_analysis <- death_analysis[, !names(death_analysis) %in% c("injuries", 
    "property_damage", "crop_damage")]
# remove 0 values
death_analysis <- subset(x = death_analysis, deaths > 0)
# calculate percentage (presentation form)
death_analysis$deathprop <- 100 * death_analysis$deaths/sum(death_analysis$deaths)
# calculate cumulative sum
death_analysis <- within(death_analysis, death_sum <- cumsum(deaths))
# calculate cumulative proportion
death_analysis <- within(death_analysis, death_prop_sum <- cumsum(deathprop))
library(xtable)
```


Analysis of the proportion of deaths associated with each event type shows that 80% of the fatalities can be attributed to 10 event types. Table 1 below shows the causes of approximately 80% of fatalities in the NOAA database.


```r
print(xtable(subset(death_analysis, death_prop_sum <= 80), type = "html"))
```

```
## % latex table generated in R 3.0.2 by xtable 1.7-3 package
## % Sun May 25 20:13:24 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rlrrrrr}
##   \hline
##  & event & deaths & total\_damage & deathprop & death\_sum & death\_prop\_sum \\ 
##   \hline
## 371 & tornado & 5633.00 & 52040613590.00 & 37.19 & 5633.00 & 37.19 \\ 
##   54 & excessive heat & 1903.00 & 500155700.00 & 12.57 & 7536.00 & 49.76 \\ 
##   65 & flash flood & 978.00 & 16562128610.00 & 6.46 & 8514.00 & 56.22 \\ 
##   133 & heat & 937.00 & 3258500.00 & 6.19 & 9451.00 & 62.40 \\ 
##   233 & lightning & 816.00 & 940751370.00 & 5.39 & 10267.00 & 67.79 \\ 
##   386 & tstm wind & 504.00 & 5038965790.00 & 3.33 & 10771.00 & 71.12 \\ 
##   78 & flood & 470.00 & 27819678250.00 & 3.10 & 11241.00 & 74.22 \\ 
##   276 & rip current & 368.00 & 1000.00 & 2.43 & 11609.00 & 76.65 \\ 
##   179 & high wind & 248.00 & 4608617560.00 & 1.64 & 11857.00 & 78.29 \\ 
##   11 & avalanche & 224.00 & 3721800.00 & 1.48 & 12081.00 & 79.77 \\ 
##    \hline
## \end{tabular}
## \end{table}
```

__Table 1: Meteorological events responsible for >80% of fatalities reported within the NOAA storm database.__


```r
# take injury statistics
injury_analysis <- consequences[with(consequences, order(-injuries)), ]
# remove other columns
injury_analysis <- injury_analysis[, !names(injury_analysis) %in% c("deaths", 
    "property_damage", "crop_damage")]
# remove 0 values
injury_analysis <- subset(x = injury_analysis, injuries > 0)
# calculate percentage (presentation form)
injury_analysis$injuryprop <- 100 * injury_analysis$injuries/sum(injury_analysis$injuries)
# calculate cumulative sum
injury_analysis <- within(injury_analysis, injury_sum <- cumsum(injuries))
# calculate cumulative proportion
injury_analysis <- within(injury_analysis, injury_prop_sum <- cumsum(injuryprop))
```


Analysis of the proportion of injuries associated with each event type shows that 80% of the fatalities can be attributed to 4 event types. Interestingly, these values are even more concentrated than for fatalities Table 2 below shows the causes of approximately 80% of fatalities in the NOAA database.


```r
print(xtable(subset(injury_analysis, injury_prop_sum <= 80), type = "html"))
```

```
## % latex table generated in R 3.0.2 by xtable 1.7-3 package
## % Sun May 25 20:13:24 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rlrrrrr}
##   \hline
##  & event & injuries & total\_damage & injuryprop & injury\_sum & injury\_prop\_sum \\ 
##   \hline
## 371 & tornado & 91346.00 & 52040613590.00 & 65.00 & 91346.00 & 65.00 \\ 
##   386 & tstm wind & 6957.00 & 5038965790.00 & 4.95 & 98303.00 & 69.95 \\ 
##   78 & flood & 6789.00 & 27819678250.00 & 4.83 & 105092.00 & 74.78 \\ 
##   54 & excessive heat & 6525.00 & 500155700.00 & 4.64 & 111617.00 & 79.43 \\ 
##    \hline
## \end{tabular}
## \end{table}
```

__Table 2: Meteorological events responsible for >80% of injuries reported within the NOAA storm database.__

### 2.2: Economic impacts of storm events

```r
printMoney <- function(x) {
    format(x, nsmall = 2, decimal.mark = ".", big.mark = ",")
}
```

The NOAA database records $151,429.74M in property damage, $1.378M crop damage, or $186,913.50M associated with hydro-meteorological events.


```r
# take economic cost
economic_cost <- consequences[with(consequences, order(-total_damage)), ]

# remove other columns
economic_cost <- economic_cost[, !names(economic_cost) %in% c("deaths", "injuries")]

# remove 0 values
economic_cost <- subset(x = economic_cost, total_damage > 0)

# calculate percentage (presentation form)
economic_cost$costprop <- 100 * economic_cost$total_damage/sum(economic_cost$total_damage)

# calculate cumulative sum
economic_cost <- within(economic_cost, cost_sum <- cumsum(total_damage))

# calculate cumulative proportion
economic_cost <- within(economic_cost, cost_prop_sum <- cumsum(costprop))
```


Analysis of the proportion of economic loss associated with each event type shows that 80% of the economic loss can be attributed to 8 event types. Table 3 below shows the causes of approximately 80% of fatalities in the NOAA database.


```r
print(xtable(subset(economic_cost, cost_prop_sum <= 80), type = "html"))
```

```
## % latex table generated in R 3.0.2 by xtable 1.7-3 package
## % Sun May 25 20:13:24 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rlrrrrrr}
##   \hline
##  & event & property\_damage & crop\_damage & total\_damage & costprop & cost\_sum & cost\_prop\_sum \\ 
##   \hline
## 371 & tornado & 51625660480.00 & 100018.52 & 52040613590.00 & 27.84 & 52040613590.00 & 27.84 \\ 
##   78 & flood & 22157709800.00 & 168037.88 & 27819678250.00 & 14.88 & 79860291840.00 & 42.73 \\ 
##   116 & hail & 13927366720.00 & 579596.28 & 16952904170.00 & 9.07 & 96813196010.00 & 51.80 \\ 
##   65 & flash flood & 15140811510.00 & 179200.46 & 16562128610.00 & 8.86 & 113375324620.00 & 60.66 \\ 
##   43 & drought & 1046106000.00 & 33898.62 & 13518672000.00 & 7.23 & 126893996620.00 & 67.89 \\ 
##   194 & hurricane & 6168319010.00 & 5339.31 & 8910229010.00 & 4.77 & 135804225630.00 & 72.66 \\ 
##   386 & tstm wind & 4484958440.00 & 109202.60 & 5038965790.00 & 2.70 & 140843191420.00 & 75.35 \\ 
##   203 & hurricane/typhoon & 3805840000.00 & 4798.48 & 4903712800.00 & 2.62 & 145746904220.00 & 77.98 \\ 
##    \hline
## \end{tabular}
## \end{table}
```

__Table 2: Meteorological events responsible for >80% of economic losses calculated as a sum of property and cropping losses reported within the NOAA storm database.__

### 2.3: Summary


```r

top_death <- subset(death_analysis, death_prop_sum <= 80)$event
top_econ <- subset(economic_cost, cost_prop_sum <= 80)$event
top_events <- consequences[consequences$event %in% union(top_death, top_econ), 
    ]
top_events <- top_events[, !names(top_events) %in% c("injuries", "property_damage", 
    "crop_damage")]
```


A subset of 14 meteorological events (Figure 1 below) correspond to 80% of the fatalities and economic losses reported within the NOAA storm database.

__Figure 1: Summary plot of the meteorological events with the greatest recorded mortality and economic consequence__

```r
par(mfrow = c(2, 1))
par(mar = c(8, 5, 1, 5))
par(cex = 0.8)
barplot(height = top_events$total_damage, names.arg = top_events$event, las = 3, 
    ylab = "Damage (USD)")
barplot(height = top_events$deaths, names.arg = top_events$event, las = 3, ylab = "Fatalities")
```

![plot of chunk summary_chart](figure/summary_chart.png) 


### Section 3: Conclusion
This analysis shows that a small number of key event types as indicated in Table 3 below are responsible for >80% of the mortality and economic losses reported within the NOAA storm database.


```r
print(xtable(top_events, type = "html"))
```

```
## % latex table generated in R 3.0.2 by xtable 1.7-3 package
## % Sun May 25 20:13:25 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rlrr}
##   \hline
##  & event & deaths & total\_damage \\ 
##   \hline
## 11 & avalanche & 224.00 & 3721800.00 \\ 
##   43 & drought & 0.00 & 13518672000.00 \\ 
##   54 & excessive heat & 1903.00 & 500155700.00 \\ 
##   65 & flash flood & 978.00 & 16562128610.00 \\ 
##   78 & flood & 470.00 & 27819678250.00 \\ 
##   116 & hail & 15.00 & 16952904170.00 \\ 
##   133 & heat & 937.00 & 3258500.00 \\ 
##   179 & high wind & 248.00 & 4608617560.00 \\ 
##   194 & hurricane & 61.00 & 8910229010.00 \\ 
##   203 & hurricane/typhoon & 64.00 & 4903712800.00 \\ 
##   233 & lightning & 816.00 & 940751370.00 \\ 
##   276 & rip current & 368.00 & 1000.00 \\ 
##   371 & tornado & 5633.00 & 52040613590.00 \\ 
##   386 & tstm wind & 504.00 & 5038965790.00 \\ 
##    \hline
## \end{tabular}
## \end{table}
```

__Table 3: Meteorological events responsible for >80% of economic losses calculated as a sum of property and cropping losses reported within the NOAA storm database.__

It can also be seen that tornados are the most damaging weather event, accounting for 27.8421% of economic impact, 37.1938% of deaths, and 65.002% of injuries/

As such, these are the events that any municipal manager should be considering first, dependent upon the particular meteorological and geographic circumstances of the municipality.
