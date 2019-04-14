#### ---------------------------------------------
###   Script for Spatial Analysis of 2016 asthma data
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
library(gstat)

## set working directory
# setwd("~/repos/biostat-696-project")
# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/biostat-696-project")

## Read in the data
asthma = read.csv("2016Asthma_Final.csv")

## run initial (non-spatial) GLM
loglinear_model = glm(asthma_count ~ offset(log(total_population)) + pct_obesity + pct_daily_smokers + log_mean_AQI,
                     family = "poisson", data = asthma)
summary(loglinear_model)

## save the residuals 
loglinResids <- residuals(loglinear_model)

## test moran's I: 


logistic_model = glm(cbind(asthma_count, total_population - asthma_count) ~ pct_obesity + pct_daily_smokers + log_mean_AQI,
                     family = "binomial", data = asthma)
summary(logistic_model)


##### Use kriging on centroid lat/long coordinates: 

asthma_with_coords = data.table(read.csv("2016Asthma_Final_w_KrigingParams.csv"))

## convert lat & long for semi variogram: 

## set the radius of the earth
R <- 6371

get_lat_in_km <- function(lat, long, R){
  distance <- R*cos(lat)*cos(long)
  return(distance)
}

get_long_in_km <- function(lat, long, R){
  distance <- R*cos(lat)*sin(long)
  return(distance)
}

## transform the lat/long variables from angles to distances (in km)
asthma_with_coords$x.dist <- mapply(get_lat_in_km, asthma_with_coords$lat, asthma_with_coords$long, R)
asthma_with_coords$y.dist <- mapply(get_long_in_km, asthma_with_coords$lat, asthma_with_coords$long, R)

## run initial (non-spatial) GLM
loglinear_model = glm(asthma_count ~ offset(log(total_population)) + obesity_rate_2016 + pct_daily_smokers + pct_black + log(meanAQI.Ozone)
                      + log(meanAQI.Other),
                      family = "poisson", data = asthma_with_coords)
summary(loglinear_model)

## save the residuals 
loglinResids <- residuals(loglinear_model)
# since we do not have asthma data for some states, make sure to only get the coordinates for the 
x.dist <- asthma_with_coords[!is.na(asthma_with_coords$state)&!is.na(asthma_with_coords$pct_black)]$x.dist/1000
y.dist <- asthma_with_coords[!is.na(asthma_with_coords$state)&!is.na(asthma_with_coords$pct_black)]$y.dist/1000


residData <- data.table(cbind(x.dist, y.dist, loglinResids))

setnames(residData, c("x.dist", "y.dist", "residuals"))

emp.variog <- variogram(loglinResids~1,locations=~x.dist+y.dist, data=residData)
plot(emp.variog, main="Empirical Semi-Variogram of the Log-Linear Residuals")

## the autocorrelation between the points seems to be random 
# this could be due to other variables that are not included in the model



# fit an exponential variogram 
exp.variog <- fit.variogram(emp.variog,vgm(psill=800, "Exp"),fit.method=2)
exp.variog
# make a plot of the empirical semi-variogram with the exponential semi-variogram overlayed 
print(plot(emp.variog,exp.variog, main="Exponential Semi-Variogram"))

plot(vario, vario.fit$var_model, main = "Fitted variogram")






