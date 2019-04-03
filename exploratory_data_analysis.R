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

# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/Final Project")

## --- --- --- --- --- --- --- --- 
#### Air quality data#### 
## --- --- --- --- --- --- --- --- 

## load data 
airData <- data.table(read.csv("data_174749.csv"))
airData$pollutant_count <- 1

# dataset with two variables - FIPs number and the rates 
graphData <- countyData[,c("fips", "Data")]
graphData$Data <- as.numeric(graphData$Data)

## plot  on a state level
plot_usmap(include = "KY", data = graphData, values = "Data", lines = "black") + 
  scale_fill_gradient2(low="green",midpoint=median(na.omit(graphData$Data)), high="red", name="Asthma Hospitalization \n Rates") +
  theme(legend.position = "right") + 
  labs(title="2010-2012 Average Asthma Rates per 1,000 Population - KY County Level") 

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
