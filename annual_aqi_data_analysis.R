
#### ---------------------------------------------
###   Air Quality Data Analysis 
#### ---------------------------------------------

## load libraries
rm(list=ls())
library(data.table)
library(readxl)
library(RColorBrewer)
library(classInt)
library(geoR)
library(spBayes)
library(fields)
library(MBA)
library(akima)
library(ggplot2)
library(usmap)

#setwd("~/repos/biostat-696-project")

#### ---------------------------------------------
###   Smoking Data Analysis 
#### ---------------------------------------------

## load in smoking data 
airData2016 <- data.table(read.csv("daily_aqi_by_county_2016.csv"))
## extract month and year from the data 

airData2016$Date <- ymd(airData2016$Date)
airData2016$year <- year(airData2016$Date)

## we will exclude Alaska and Hawaii from our analysis
airData2016 <- airData2016[!State.Name%in%c("National", "Alaska", "Hawaii")]
aggregatedAirData <- airData2016[,list(meanAQI=mean(na.omit(AQI))), by=c("State.Name", "Category", "Defining.Parameter","year")]


aggregatedAirData$fips <- fips(aggregatedAirData$State.Name)


pollute_types <- unique(aggregatedAirData$Defining.Parameter)
plot_list <- list()
## plot the average AQI for each type of pollutant  on a state level
for (k in 1:length(pollute_types)){
  graphData <- aggregatedAirData[Defining.Parameter==pollute_types[k]]
  plot_list[[k]] <- plot_usmap(data =graphData, values = "meanAQI", lines = "black", labels = TRUE) + 
    scale_fill_gradient2(low="green",high="red", midpoint=median(na.omit(graphData$meanAQI)),name="Mean AQI") +
    theme(legend.position = "right") + 
    labs(title=paste0("2016 Air Quality by State for ", pollute_types[k]))
}

## it appears that we are missing data for some of the pollutants (e.g. Nebraska does not have data for NO2)
## however, for ozone, we have data for all 50 states 
## also, it has been established that asthma can be triggered by ozone, so moving forward, we will proceed with ozone only analysis