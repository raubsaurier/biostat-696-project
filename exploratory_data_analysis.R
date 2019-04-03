
###############################################################################################
#####
#####    
#####         
#####
###############################################################################################
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
# set working directory to wherever the source files are: 
#setwd("~/repos/biostat-696-project")


## --- --- --- --- --- --- --- --- 
#### Air quality data#### 
## --- --- --- --- --- --- --- --- 

## load data 
airData <- data.table(read.csv("data_174749.csv"))
airData$pollutant_count <- 1

# just plot for one year: 
airData2005 <- airData[Year==2005]
airData2005$state <- tolower(as.character(airData2005$State))

## let's just plot pollution totals for now and ignore what type of pollution it is: 
graphData <- airData2005[,list(Value= sum(na.omit(Value))), by=list(stateFIPS, County, Year, state)]

## --- --- --- --- --- --- --- --- 
#### Asthma data for Kansas by County for 2000-2017 #####
## --- --- --- --- --- --- --- --- 

asthmaData <- data.table(read_excel("kansas_asthma_rate.xlsx"))


# let's do the year 2011 and by county level 
countyData <- asthmaData[LocationType=="County"&TimeFrame==2011]

get_fips <- function(state, county){
  x <- fips(state, county)
  return(x)
}

## get the FIPS number for each county in Kansas 
countyData$fips <- mapply(get_fips, "Kansas", countyData$Location)

# dataset with two variables - FIPs number and the rates 
graphData <- countyData[,c("fips", "Data")]
graphData$Data <- as.numeric(graphData$Data)

## check what the median rate is 
median(graphData$Data)

## plot  on a state level
plot_usmap(include = "KS", data = graphData, values = "Data", lines = "black") + 
  scale_fill_gradient2(low="blue",midpoint=median(graphData$Data), high="red", name="Asthma Rates") +
  theme(legend.position = "right") + 
  labs(title="2011 Asthma Rates per 1,000 Population - Kansas County Level") 


## --- --- --- --- --- --- --- --- 
#### Asthma data for Kentucky by County for 2000-2017 #####

## this is hospitalizaton rates per 10,000 children ages 0-17 
## --- --- --- --- --- --- --- --- 

kyData <- data.table(read_xlsx("kyData.xlsx"))
# let's do the year 2011 and by county level 
countyData <- kyData[LocationType=="County"&TimeFrame=="2010-2012"]

get_fips <- function(state, county){
  x <- fips(state, county)
  return(x)
}

## get the FIPS number for each county in KY
countyData$fips <- mapply(get_fips, "KY", countyData$Location)

# dataset with two variables - FIPs number and the rates 
graphData <- countyData[,c("fips", "Data")]
graphData$Data <- as.numeric(graphData$Data)

## plot  on a state level
plot_usmap(include = "KY", data = graphData, values = "Data", lines = "black") + 
  scale_fill_gradient2(low="green",midpoint=median(na.omit(graphData$Data)), high="red", name="Asthma Hospitalization \n Rates") +
  theme(legend.position = "right") + 
  labs(title="2010-2012 Average Asthma Rates per 1,000 Population - KY County Level") 

# from the map, we see that there are 13 counties that we do not have data for
# we may need to interpolate the data for these 

## Also, we see that Fulton county has a very high rate of child asthma hospitalizations 
#compared to the rest of the state 


