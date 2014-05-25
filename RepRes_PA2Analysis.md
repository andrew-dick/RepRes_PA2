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
}

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
print(xtable(subset(death_analysis, death_prop_sum <= 80)), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun May 25 20:38:37 2014 -->
<TABLE border=1>
<TR> <TH> event </TH> <TH> deaths </TH> <TH> total_damage </TH> <TH> deathprop </TH> <TH> death_sum </TH> <TH> death_prop_sum </TH>  </TR>
  <TR> <TD> tornado </TD> <TD align="right"> 5633.00 </TD> <TD align="right"> 52040613590.00 </TD> <TD align="right"> 37.19 </TD> <TD align="right"> 5633.00 </TD> <TD align="right"> 37.19 </TD> </TR>
  <TR> <TD> excessive heat </TD> <TD align="right"> 1903.00 </TD> <TD align="right"> 500155700.00 </TD> <TD align="right"> 12.57 </TD> <TD align="right"> 7536.00 </TD> <TD align="right"> 49.76 </TD> </TR>
  <TR> <TD> flash flood </TD> <TD align="right"> 978.00 </TD> <TD align="right"> 16562128610.00 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 8514.00 </TD> <TD align="right"> 56.22 </TD> </TR>
  <TR> <TD> heat </TD> <TD align="right"> 937.00 </TD> <TD align="right"> 3258500.00 </TD> <TD align="right"> 6.19 </TD> <TD align="right"> 9451.00 </TD> <TD align="right"> 62.40 </TD> </TR>
  <TR> <TD> lightning </TD> <TD align="right"> 816.00 </TD> <TD align="right"> 940751370.00 </TD> <TD align="right"> 5.39 </TD> <TD align="right"> 10267.00 </TD> <TD align="right"> 67.79 </TD> </TR>
  <TR> <TD> tstm wind </TD> <TD align="right"> 504.00 </TD> <TD align="right"> 5038965790.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 10771.00 </TD> <TD align="right"> 71.12 </TD> </TR>
  <TR> <TD> flood </TD> <TD align="right"> 470.00 </TD> <TD align="right"> 27819678250.00 </TD> <TD align="right"> 3.10 </TD> <TD align="right"> 11241.00 </TD> <TD align="right"> 74.22 </TD> </TR>
  <TR> <TD> rip current </TD> <TD align="right"> 368.00 </TD> <TD align="right"> 1000.00 </TD> <TD align="right"> 2.43 </TD> <TD align="right"> 11609.00 </TD> <TD align="right"> 76.65 </TD> </TR>
  <TR> <TD> high wind </TD> <TD align="right"> 248.00 </TD> <TD align="right"> 4608617560.00 </TD> <TD align="right"> 1.64 </TD> <TD align="right"> 11857.00 </TD> <TD align="right"> 78.29 </TD> </TR>
  <TR> <TD> avalanche </TD> <TD align="right"> 224.00 </TD> <TD align="right"> 3721800.00 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 12081.00 </TD> <TD align="right"> 79.77 </TD> </TR>
   </TABLE>

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
print(xtable(subset(injury_analysis, injury_prop_sum <= 80)), type = "html", 
    include.rownames = FALSE)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun May 25 20:38:37 2014 -->
<TABLE border=1>
<TR> <TH> event </TH> <TH> injuries </TH> <TH> total_damage </TH> <TH> injuryprop </TH> <TH> injury_sum </TH> <TH> injury_prop_sum </TH>  </TR>
  <TR> <TD> tornado </TD> <TD align="right"> 91346.00 </TD> <TD align="right"> 52040613590.00 </TD> <TD align="right"> 65.00 </TD> <TD align="right"> 91346.00 </TD> <TD align="right"> 65.00 </TD> </TR>
  <TR> <TD> tstm wind </TD> <TD align="right"> 6957.00 </TD> <TD align="right"> 5038965790.00 </TD> <TD align="right"> 4.95 </TD> <TD align="right"> 98303.00 </TD> <TD align="right"> 69.95 </TD> </TR>
  <TR> <TD> flood </TD> <TD align="right"> 6789.00 </TD> <TD align="right"> 27819678250.00 </TD> <TD align="right"> 4.83 </TD> <TD align="right"> 105092.00 </TD> <TD align="right"> 74.78 </TD> </TR>
  <TR> <TD> excessive heat </TD> <TD align="right"> 6525.00 </TD> <TD align="right"> 500155700.00 </TD> <TD align="right"> 4.64 </TD> <TD align="right"> 111617.00 </TD> <TD align="right"> 79.43 </TD> </TR>
   </TABLE>

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
print(xtable(subset(economic_cost, cost_prop_sum <= 80)), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun May 25 20:38:37 2014 -->
<TABLE border=1>
<TR> <TH> event </TH> <TH> property_damage </TH> <TH> crop_damage </TH> <TH> total_damage </TH> <TH> costprop </TH> <TH> cost_sum </TH> <TH> cost_prop_sum </TH>  </TR>
  <TR> <TD> tornado </TD> <TD align="right"> 51625660480.00 </TD> <TD align="right"> 100018.52 </TD> <TD align="right"> 52040613590.00 </TD> <TD align="right"> 27.84 </TD> <TD align="right"> 52040613590.00 </TD> <TD align="right"> 27.84 </TD> </TR>
  <TR> <TD> flood </TD> <TD align="right"> 22157709800.00 </TD> <TD align="right"> 168037.88 </TD> <TD align="right"> 27819678250.00 </TD> <TD align="right"> 14.88 </TD> <TD align="right"> 79860291840.00 </TD> <TD align="right"> 42.73 </TD> </TR>
  <TR> <TD> hail </TD> <TD align="right"> 13927366720.00 </TD> <TD align="right"> 579596.28 </TD> <TD align="right"> 16952904170.00 </TD> <TD align="right"> 9.07 </TD> <TD align="right"> 96813196010.00 </TD> <TD align="right"> 51.80 </TD> </TR>
  <TR> <TD> flash flood </TD> <TD align="right"> 15140811510.00 </TD> <TD align="right"> 179200.46 </TD> <TD align="right"> 16562128610.00 </TD> <TD align="right"> 8.86 </TD> <TD align="right"> 113375324620.00 </TD> <TD align="right"> 60.66 </TD> </TR>
  <TR> <TD> drought </TD> <TD align="right"> 1046106000.00 </TD> <TD align="right"> 33898.62 </TD> <TD align="right"> 13518672000.00 </TD> <TD align="right"> 7.23 </TD> <TD align="right"> 126893996620.00 </TD> <TD align="right"> 67.89 </TD> </TR>
  <TR> <TD> hurricane </TD> <TD align="right"> 6168319010.00 </TD> <TD align="right"> 5339.31 </TD> <TD align="right"> 8910229010.00 </TD> <TD align="right"> 4.77 </TD> <TD align="right"> 135804225630.00 </TD> <TD align="right"> 72.66 </TD> </TR>
  <TR> <TD> tstm wind </TD> <TD align="right"> 4484958440.00 </TD> <TD align="right"> 109202.60 </TD> <TD align="right"> 5038965790.00 </TD> <TD align="right"> 2.70 </TD> <TD align="right"> 140843191420.00 </TD> <TD align="right"> 75.35 </TD> </TR>
  <TR> <TD> hurricane/typhoon </TD> <TD align="right"> 3805840000.00 </TD> <TD align="right"> 4798.48 </TD> <TD align="right"> 4903712800.00 </TD> <TD align="right"> 2.62 </TD> <TD align="right"> 145746904220.00 </TD> <TD align="right"> 77.98 </TD> </TR>
   </TABLE>

__Table 3: Meteorological events responsible for >80% of economic losses calculated as a sum of property and cropping losses reported within the NOAA storm database.__

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
This analysis shows that a small number of key event types as indicated in Table 4 below are responsible for >80% of the mortality and economic losses reported within the NOAA storm database.


```r
print(xtable(top_events), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun May 25 20:38:37 2014 -->
<TABLE border=1>
<TR> <TH> event </TH> <TH> deaths </TH> <TH> total_damage </TH>  </TR>
  <TR> <TD> avalanche </TD> <TD align="right"> 224.00 </TD> <TD align="right"> 3721800.00 </TD> </TR>
  <TR> <TD> drought </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 13518672000.00 </TD> </TR>
  <TR> <TD> excessive heat </TD> <TD align="right"> 1903.00 </TD> <TD align="right"> 500155700.00 </TD> </TR>
  <TR> <TD> flash flood </TD> <TD align="right"> 978.00 </TD> <TD align="right"> 16562128610.00 </TD> </TR>
  <TR> <TD> flood </TD> <TD align="right"> 470.00 </TD> <TD align="right"> 27819678250.00 </TD> </TR>
  <TR> <TD> hail </TD> <TD align="right"> 15.00 </TD> <TD align="right"> 16952904170.00 </TD> </TR>
  <TR> <TD> heat </TD> <TD align="right"> 937.00 </TD> <TD align="right"> 3258500.00 </TD> </TR>
  <TR> <TD> high wind </TD> <TD align="right"> 248.00 </TD> <TD align="right"> 4608617560.00 </TD> </TR>
  <TR> <TD> hurricane </TD> <TD align="right"> 61.00 </TD> <TD align="right"> 8910229010.00 </TD> </TR>
  <TR> <TD> hurricane/typhoon </TD> <TD align="right"> 64.00 </TD> <TD align="right"> 4903712800.00 </TD> </TR>
  <TR> <TD> lightning </TD> <TD align="right"> 816.00 </TD> <TD align="right"> 940751370.00 </TD> </TR>
  <TR> <TD> rip current </TD> <TD align="right"> 368.00 </TD> <TD align="right"> 1000.00 </TD> </TR>
  <TR> <TD> tornado </TD> <TD align="right"> 5633.00 </TD> <TD align="right"> 52040613590.00 </TD> </TR>
  <TR> <TD> tstm wind </TD> <TD align="right"> 504.00 </TD> <TD align="right"> 5038965790.00 </TD> </TR>
   </TABLE>

__Table 4: Meteorological events responsible for >80% of economic losses calculated as a sum of property and cropping losses reported within the NOAA storm database.__

It can also be seen that tornados are the most damaging weather event, accounting for 27.8421% of economic impact, 37.1938% of deaths, and 65.002% of injuries.

As such, these are the events that any municipal manager should be considering first, dependent upon the particular meteorological and geographic circumstances of the municipality.
