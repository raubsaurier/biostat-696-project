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

# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/Final Project")

## load data 


airData <- data.table(read.csv("data_174749.csv"))
airData$pollutant_count <- 1

# just plot for one year: 

states <- map_data("state")
  # ggplot2::map_data - turn data from the maps package into a data frame suitable for plotting with ggplot2
  # states is a data frame that has lat/long coordinates of the states
counties <- map_data("county")
  # counties is a data fram that has lat/long coordinates of the counties

airData2005 <- airData[Year==2005]
airData2005$region <- tolower(as.character(airData2005$State))
airData2005$subregion <- tolower(as.character(airData2005$County))

graphData2005 <- merge(airData2005, counties, allow.cartesian=TRUE)
  # merge 2005 data to states so it can be graphed with ggplot2

pdf("2005 concentrations.pdf")
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = graphData2005, aes(x = long, y = lat, group = group, fill = Value)) +
  facet_wrap(~ Pollutant)
dev.off()
  # looks as if Acetaldehyde and Formaldehyde have the most variation in the data

graphData2005$logValue = ifelse(graphData2005$Value != 0, log(graphData2005$Value), log(graphData2005$Value + .0001))
pdf("2005 logconcentrations.pdf")
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = graphData2005, aes(x = long, y = lat, group = group, fill = logValue)) +
  facet_wrap(~ Pollutant)
dev.off()
  # on the log scale butadiene has the most variation in values



airData2011 <- airData[Year==2011]
airData2011$region <- tolower(as.character(airData2011$State))
airData2011$subregion <- tolower(as.character(airData2011$County))

graphData2011 <- merge(airData2011, counties, allow.cartesian=TRUE)
# merge 2005 data to states so it can be graphed with ggplot2

pdf("2011 concentrations.pdf")
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = graphData2011, aes(x = long, y = lat, group = group, fill = Value)) +
  facet_wrap(~ Pollutant)
dev.off()
  # looks as if Acetaldehyde and Formaldehyde have the most variation in the data

graphData2011$logValue = ifelse(graphData2011$Value != 0, log(graphData2011$Value), log(graphData2011$Value + .0001))
pdf("2011 logconcentrations.pdf")
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data = graphData2011, aes(x = long, y = lat, group = group, fill = logValue)) +
  facet_wrap(~ Pollutant)
dev.off()
  # on the log scale butadiene has the most variation
