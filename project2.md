**Coursera - Reproducible Research - Project 2 -**
**The health and financial impact of weather diasters in the United States**

**Synopsis**
This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size.
The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

The objective of the analysis, is to determine the weather diasters that cause the most fatalities and economic damage.

The first code block loads the relevant R packages, and reads in the '.csv' file.  This assumes that the file has already been downloaded and unzipped.

```r
library(dplyr)
library(knitr)
weather <- read.csv("repdata%2Fdata%2FStormData.csv", sep = ",")
```

The second code block processes the data, to sum the fatalities caused by each type of weather event.
The weather events are then sorted by which cause the highest number of fatalities.
The top 10 are subsetted to later graph.

```r
fatalities <- aggregate(FATALITIES ~ EVTYPE, data = weather, FUN = sum)
fatalities_ordered <- fatalities[order(decreasing = TRUE, fatalities[,2]),]
fatalities_plot <- fatalities_ordered[1:10,]
```

This code block processes the data to assess the highest property damage. The data is labeled with a multiplier, that ranges from a hundred to one billion. A loop is used to check with multiplier corresponds with each element, and calculates the total value of damage.

Just as done for fatalities, the sum of damage caused by each weather event is determined, and ordered by events causing the most value damage.  

```r
##for loop goes through each element of the exponent of the property damage
##when it encounters an exponent, it multiplies the property damage by the multiplier
##this value is stored in a new array to later be added to the data frame
for(i in 1:nrow(weather)){
  if(weather$PROPDMGEXP[i] == "H" | weather$PROPDMGEXP[i] == "h"){
    weather_pdmg[i] <- weather$PROPDMG[i] * 100
  }else if(weather$PROPDMGEXP[i] == "K" | weather$PROPDMGEXP[i] == "k"){
    weather_pdmg[i] <- weather$PROPDMG[i] * 1000
  }else if(weather$PROPDMGEXP[i] == "M" | weather$PROPDMGEXP[i] == "m"){
    weather_pdmg[i] <- weather$PROPDMG[i] *1000000
  }else if(weather$PROPDMGEXP[i] == "B" | weather$PROPDMGEXP[i] == "b"){
    weather_pdmg[i] <- weather$PROPDMG[i] *1000000000
  }else{
    weather_pdmg[i] <- weather$PROPDMG[i]
  }
}
weather$pdmg <- weather_pdmg
pdmg <- aggregate(pdmg ~ EVTYPE, data = weather, FUN = sum)
pdmg_ordered <- pdmg[order(decreasing = TRUE, pdmg[,2]),]
pdmg_10 <- pdmg_ordered[1:10,]
```


This code block repeats the above loop, but for crop damage.

```r
weather_cdmg <- numeric()
##does the same for crop damage, as above
for(i in 1:nrow(weather)){
  if(weather$CROPDMGEXP[i] == "H" | weather$CROPDMGEXP[i] == "h"){
    weather_cdmg[i] <- weather$CROPDMG[i] * 100
  }else if(weather$CROPDMGEXP[i] == "K" | weather$CROPDMGEXP[i] == "k"){
    weather_cdmg[i] <- weather$CROPDMG[i] * 1000
  }else if(weather$CROPDMGEXP[i] == "M" | weather$CROPDMGEXP[i] == "m"){
    weather_cdmg[i] <- weather$CROPDMG[i] *1000000
  }else if(weather$CROPDMGEXP[i] == "B" | weather$CROPDMGEXP[i] == "b"){
    weather_cdmg[i] <- weather$CROPDMG[i] *1000000000
  }else{
    weather_cdmg[i] <- weather$CROPDMG[i]
  }
}
weather$cdmg <- weather_cdmg
cdmg <- aggregate(cdmg ~ EVTYPE, data = weather, FUN = sum)
cdmg_ordered <- cdmg[order(decreasing = TRUE, cdmg[,2]),]
cdmg_10 <- cdmg_ordered[1:10,]
```

**Results**

The first plot shows the 10 weather events that cause the most fatalities.

```r
barplot(fatalities_plot$FATALITIES, las = 3, names.arg = fatalities_plot$EVTYPE, 
        main = "Weater Events with Highest Fatalities", 
        ylab = "Number of fatalities", col = "red", cex.names = 0.8)
```

![plot of chunk Results - Fatalities by Weather Event](figure/Results - Fatalities by Weather Event-1.png)

The next two plots show the 10 weather events that cause the most property and crop damage.

```r
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(pdmg_10$pdmg/(10^9), las = 3, names.arg = pdmg_10$EVTYPE, 
        main = "Events with Highest Property Damages", ylab = "Damage Cost ($ billions)", 
        col = "blue")
barplot(cdmg_10$cdmg/(10^9), las = 3, names.arg = cdmg_10$EVTYPE, 
        main = "Events With Highest Crop Damages", ylab = "Damage Cost ($ billions)", 
        col = "green", ylim = c(0,14))
```

![plot of chunk Results - Property and Crop Damage by Weather Event](figure/Results - Property and Crop Damage by Weather Event-1.png)

**Conclusions**
Highest Fatalities - The weather diaster that contributes to the largest number of fatalities is Tornados, followed by excessive heat and flash flood

The economic impact was split between Property Damage and Crop Damage.The events with the highest property damages are flooding, hurricane/typhoons and tornados.  
The events with the highest crop damages are drought and different types of flooding.
