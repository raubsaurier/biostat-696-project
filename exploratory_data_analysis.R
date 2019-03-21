
###############################################################################################
#####
#####    
#####         
#####
###############################################################################################
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(classInt)
library(geoR)
library(spBayes)
library(fields)
library(MBA)
library(akima)
library(ggplot2)


## load data 


airData <- data.table(read.csv("data_174749.csv"))
airData$pollutant_count <- 1

# just plot for one year: 
airData2005 <- airData[Year==2005]
airData2005$state <- tolower(as.character(airData2005$State))
states <- map_data("state")

graphData <- merge(airData2005, states, by.x="State", by.y="region", allow.cartesian=TRUE)






