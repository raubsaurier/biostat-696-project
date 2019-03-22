
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

## let's just plot pollution totals for now and ignore what type of pollution it is: 
graphData <- airData2005[,list(Value= sum(na.omit(Value))), by=list(stateFIPS, County, Year, state)]

## load in map of US by states from the "maps" package
states <- map_data("state")

graphData <- merge(airData2005, states, by.x="state", by.y="region", allow.cartesian=TRUE)

## plot the pollution values: 

ggplot(data = states) + 
  geom_polygon(data=graphData, aes(x = long, y = lat, fill = Value, group = group), color = "white") + 
  coord_fixed(1.3)


