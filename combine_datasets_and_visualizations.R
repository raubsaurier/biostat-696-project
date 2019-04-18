
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

#### 4/13/2019 update: create a variable that indicates Ozone vs. other type of pollution in order to avoid confounding in the model 
### 4/17 - include PM2 to include pollen by proxy 
get_pollution_type <- function(pollution){
  x <- "Other"
  if(pollution=="Ozone"){
    x <- "Ozone"
  } else if(pollution=="PM2.5"){ 
    x <- "PM2.5"
    } else { 
      x <- x 
  }
  return(x)
}

aggregatedAirData$pollution_type <- mapply(get_pollution_type, aggregatedAirData$Defining.Parameter)
aggregatedAirData <- aggregatedAirData[,c("year", "fips", "pollution_type", "meanAQI", "State.Name")]
aggregatedAirData <- aggregatedAirData[,list(meanAQI=sum(na.omit(meanAQI))), by=c("State.Name","year", "fips", "pollution_type")]

reshapedData <- reshape(aggregatedAirData, idvar = c("year", "State.Name","fips"), timevar = "pollution_type", 
                        direction="wide")
setnames(reshapedData, "State.Name", "state")


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
###   2016 % of Children who are obese 
#### ---------------------------------------------

obesity2016 <- data.table(read_xlsx("childhood_obesity_2016_2017.xlsx"))

obesity2016 <- obesity2016[!State%in%c("Alaska", "Hawaii")]
## make a visualization of this data 
obesity2016$fips <- fips(obesity2016$State)
setnames(obesity2016, "State", "state")

plot_usmap(data =obesity2016, values = "obesity_rate_2016", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(10), name="Obesity Rate (in %)") +
  theme(legend.position = "right") + 
  labs(title="2016 Child Obesity Rate by State")
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
## use 2017 % of black residents for Montana since 2016 data was not available 
demographicData[state=="Montana"]$pct_black <- 0.01
demographicData$fips <- fips(demographicData$state)
# drop Puerto Rico, and "National" data 
demographicData <- demographicData[!is.na(fips)]

finalDemoData <- demographicData[,c("fips", "state", "pct_white", "pct_black")]

#### ---------------------------------------------
###   Merge The Covariate Datasets 
#### ---------------------------------------------

ozoneData <- copy(reshapedData)
totalData <- merge(obesity2016, smoking2016, by=c("fips", "state"))
totalData <- merge(totalData, ozoneData, by=c("fips", "state"))
totalData <- merge(totalData, finalDemoData, by=c("fips", "state"))


## check for correlations 

## since Montana doesn't have known black population, remove it from the dataset in order to be able to compute correlation

corrVars <- totalData[,c("obesity_rate_2016", "pct_daily_smokers", "meanAQI.Ozone", "pct_black", "pct_white")]
cor(corrVars)

## the results show that smoking and obesity rates appear to be positively correlated
## both % black and % white are positively correlated with obesity
# AQI tends to be negatively correlated with smoking rate and  % white
# AQI appears to be positively correlated with % black and obesity rate 

#### ---------------------------------------------
###   one last dataset - we should attach the lat/long coordinates that correspond to the centroids of each state 
#### ---------------------------------------------

## 4/17 update -=> we don't need to do kriging anymore 
# centroidData <- data.table(read.csv("us_centroids_for_kriging.csv"))
# centroidData$fips <- fips(centroidData$state)
# 
# totalData <- merge(totalData,centroidData, by=c("state", "fips"))

#### ---------------------------------------------
## load in the 2016 Child Asthma Prevalence Data 
#### ---------------------------------------------
asthma2016 <- data.table(read.csv("2016 Asthma Prevalence Complete.csv"))
## we notice right away that some states are missing from the 2016 CDC prevalence estimates 
setnames(asthma2016, c("Location", "total_count", "total_population"), c("state", "asthma_count", "total_child_pop"))
asthma2016$asthma_count = as.numeric(gsub(",", "", asthma2016$asthma_count, fixed = TRUE))
## get fips: 
asthma2016$fips <- fips(asthma2016$state)



plot_usmap(data =asthma2016, values = "asthma_count", lines = "black", labels = TRUE) + 
  scale_fill_gradientn(colours=blue2red(10), name="Asthma Count") +
  theme(legend.position = "right") + 
  labs(title="2016 Child Asthma by State")

#### ---------------------------------------------



## Merge asthma data with covariates
asthma_data = merge(asthma2016, totalData, by = c("state","fips"), all.y=TRUE) ## set all.y = TRUE so that we can krige for the missing states
asthma_data = asthma_data[,c("fips", "state","asthma_count", "obesity_rate_2016", "obesity_rate_2017",
                             "pct_daily_smokers", "meanAQI.Ozone", "meanAQI.Other", "meanAQI.PM2.5", "pct_black", "pct_white")]
#setnames(asthma_data, c("state.x"), c("state"))
asthma_data$asthma_count = as.numeric(gsub(",", "", asthma_data$asthma_count, fixed = TRUE))

#### ---------------------------------------------
## code to add total population to all states! 
#### ---------------------------------------------
# 
population = data.table(read.csv("child_population_NUMERIC.csv"))
population$Data <- as.character(population$Data)
population$Data <- as.numeric(population$Data)


population <- population[Location!="United States"&TimeFrame==2016&Age.group=="Total less than 18"]
# 
# population = population %>%
#   filter(Location != "United States" &
#            TimeFrame == 2016 &
#            Age.group == "Total less than 18" &
#            DataFormat == "Number") %>%
#   select(Location, Data) %>%
#   rename(total_population = Data)
# # 
population <- population[,c("Location", "Data")]
setnames(population,c("Location", "Data"), c("state", "total_population"))

# 
total_asthma = merge(asthma_data, population, by="state")

write.csv(total_asthma , "2016Asthma_Final_w_KrigingParams_PM25.csv",row.names=FALSE)




write.csv(asthma_data, "2016Asthma_Final.csv", row.names = FALSE)