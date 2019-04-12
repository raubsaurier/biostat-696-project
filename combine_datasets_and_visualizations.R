
#### ---------------------------------------------
###   Script for Analyzing the Covariate Data
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
library(colorRamps)

#setwd("~/repos/biostat-696-project")
# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/biostat-696-project")

#### ---------------------------------------------
###   Smoking Data Analysis 
#### ---------------------------------------------

## load in 2016 air quality data 
airData2016 <- data.table(read.csv("daily_aqi_by_county_2016.csv"))

## extract month and year from the data 
airData2016$Date <- lubridate::ymd(airData2016$Date)
airData2016$year <- lubridate::year(airData2016$Date)

## we will exclude Alaska and Hawaii from our analysis
airData2016 <- airData2016[!State.Name%in%c("National", "Alaska", "Hawaii")]
aggregatedAirData <- airData2016[,list(meanAQI=mean(na.omit(AQI))), by=c("State.Name", "Defining.Parameter","year")]

## get the FIPS for each state (used for plotting)
aggregatedAirData$fips <- fips(aggregatedAirData$State.Name)
## use log scale since California has some very high ozone values 
aggregatedAirData$log_mean_AQI <- log(aggregatedAirData$meanAQI)


pollute_types <- unique(aggregatedAirData$Defining.Parameter)
plot_list <- list()
## plot the average AQI for each type of pollutant  on a state level
for (k in 1:length(pollute_types)){
  graphData <- aggregatedAirData[Defining.Parameter==pollute_types[k]]
  plot_list[[k]] <- plot_usmap(data =graphData, values = "log_mean_AQI", lines = "black") + 
    scale_fill_gradientn(colours=blue2red(5), name="Log Mean AQI") +
    theme(legend.position = "right") + 
    labs(title=paste0("2016 Air Quality by State for ", pollute_types[k]))
}

## it appears that we are missing data for some of the pollutants (e.g. Nebraska does not have data for NO2)
## however, for ozone, we have data for all 50 states 
## also, it has been established that asthma can be triggered by ozone, so moving forward, we will proceed with ozone only analysis

#### ---------------------------------------------
###   2016 % of Adults who report smoking daily 
#### ---------------------------------------------

## load in smoking data 
smoking2016 <- data.table(read_excel("daily_smoking_adults_2016.xlsx"))
## this data is messy -> need to clean it 
colnames(smoking2016) <- c("state", "pct_daily_smokers")
## only keep the continental US states 
smoking2016 <-smoking2016[1:51,]
smoking2016 <- smoking2016[!state%in%c("National", "Alaska", "Hawaii")]

## make a visualization of this data 
smoking2016$fips <- fips(smoking2016$state)

plot_usmap(data =smoking2016, values = "pct_daily_smokers", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="% of Daily Smokers") +
  theme(legend.position = "right") + 
  labs(title="2016 % Of Daily Smokers by State")


#### ---------------------------------------------
###   2016 % of Adults who are Obese 
#### ---------------------------------------------

obesity2016 <- data.table(read.csv("adult_obesity_2016.csv"))
setnames(obesity2016, c("Data_Value", "LocationAbbr","Low_Confidence_Limit","High_Confidence_Limit"), 
         c("pct_obesity", "stateID", "obesity_95_lb", "obesity_95_ub"))

## subset based on the values we want 
aggregatedObesity <-  obesity2016[,c("stateID","pct_obesity", "YearStart", "obesity_95_lb", "obesity_95_ub")]

aggregatedObesity <- aggregatedObesity[!stateID%in%c("US", "AK", "HI")]

## make a visualization of this data 
aggregatedObesity$fips <- fips(aggregatedObesity$stateID)

plot_usmap(data =aggregatedObesity, values = "pct_obesity", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(10), name="Obesity Rate (in %)") +
  theme(legend.position = "right") + 
  labs(title="2016 Adult Obesity Rate by State")
#### ---------------------------------------------
###   Race/Ethnicity Data 
#### ---------------------------------------------


## 2016 population demographic data for US states 
demographicData <- data.table(read_xlsx("race_ethnicity_data.xlsx"))
colnames(demographicData) <- as.character(demographicData[1,])
demographicData <- demographicData[-1,]
#don't need this column, as it is empty anyway 
demographicData$Footnotes <- NULL
## Research shows that African-American children have the highest prevalence of asthma 
demographicData$pct_white <- as.numeric(demographicData$White)/as.numeric(demographicData$Total)
# montana doesn't have data on # of black residents 
demographicData$pct_black <- as.numeric(demographicData$Black)/as.numeric(demographicData$Total)


setnames(demographicData, "Location", "state")
demographicData$fips <- fips(demographicData$state)
# drop Puerto Rico, and "National" data 
demographicData <- demographicData[!is.na(fips)]

finalDemoData <- demographicData[,c("fips", "pct_white", "pct_black")]

#### ---------------------------------------------
###   Merge The Covariate Datasets 
#### ---------------------------------------------

ozoneData <- aggregatedAirData[Defining.Parameter=="Ozone"]
totalData <- merge(aggregatedObesity, smoking2016, by="fips")
totalData <- merge(totalData, ozoneData, by="fips")
totalData <- merge(totalData, finalDemoData, by="fips")


## check for correlations 

## since Montana doesn't have known black population, remove it from the dataset in order to be able to compute correlations

corrData <- totalData[fips!=30]

corrVars <- corrData[,c("pct_obesity", "pct_daily_smokers", "log_mean_AQI", "pct_black", "pct_white")]
cor(corrVars)

## the results show that smoking and obesity rates appear to be positively correlated
## both % black and % white are positively correlated with obesity
# AQI tends to be negatively correlated with smoking rate, % white and obesity rate 
# AQI appears to be positively correlated with % black 

#### ---------------------------------------------
###   one last dataset - we should attach the lat/long coordinates that correspond to the centroids of each state 
#### ---------------------------------------------

centroidData <- data.table(read.csv("us_centroids_for_kriging.csv"))
centroidData$fips <- fips(centroidData$state)

totalData <- merge(totalData,centroidData, by=c("state", "fips"))

#### ---------------------------------------------
## load in the 2016 Child Asthma Prevalence Data 
#### ---------------------------------------------
asthma2016 <- data.table(read.csv("2016 Asthma Prevalence Complete.csv"))
## we notice right away that some states are missing from the 2016 CDC prevalence estimates 
setnames(asthma2016, c("Location", "total_count"), c("state", "asthma_count"))

## get fips: 
asthma2016$fips <- fips(asthma2016$state)

## Merge asthma data with covariates
asthma_data = merge(asthma2016, totalData, by = "fips", all.y=TRUE) ## set all.y = TRUE so that we can krige for the missing states
asthma_data = asthma_data[,c("fips", "state.x", "stateID", "total_population", "asthma_count",
                             "pct_obesity", "obesity_95_lb", "obesity_95_ub", 
                             "pct_daily_smokers", "meanAQI", "log_mean_AQI", "pct_black", "pct_white")]
setnames(asthma_data, c("state.x"), c("state"))
asthma_data$asthma_count = as.numeric(gsub(",", "", asthma_data$asthma_count, fixed = TRUE))

write.csv(asthma_data, "2016Asthma_Final.csv", row.names = FALSE)