
#### ---------------------------------------------
###   Simple Regression Analysis 
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


## load in pollution data 
airData <- data.table(read.csv("data_174749.csv"))
airData$pollutant_count <- 1

airData2011 <- airData[Year==2011&State=="Kentucky"]

airsubset <- airData[,c("State","countyFIPS","Year", "Value", "Pollutant")]


kyData <- data.table(read_xlsx("kyData.xlsx"))
# let's do the year 2011 and by county level 
countyData <- kyData[LocationType=="County"&TimeFrame=="2010-2012"]
setnames(countyData, "Data", "average_hospitalization_rate")

get_fips <- function(state, county){
  x <- fips(state, county)
  return(x)
}

## get the FIPS number for each county in Kansas 
countyData$fips <- mapply(get_fips, "Kentucky", countyData$Location)
countyData$fips <- as.numeric(countyData$fips)
## combine the datasets 


totalData <- merge(countyData, airsubset, by.x="fips", by.y="countyFIPS")

### fit the linear regression #####


