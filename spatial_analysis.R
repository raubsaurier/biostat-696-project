#### ---------------------------------------------
###   Script for Spatial Analysis of 2016 asthma data
#### ---------------------------------------------

## load libraries
rm(list=ls())

## set working directory
# setwd("~/repos/biostat-696-project")
# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/biostat-696-project")

## Read in the data
asthma = read.csv("2016Asthma_Final.csv")

## run initial (non-spatial) GLM
loglinear_model = glm(asthma_count ~ offset(log(total_population)) + pct_obesity + pct_daily_smokers + log_mean_AQI,
                     family = "poisson", data = asthma)
summary(loglinear_model)

logistic_model = glm(cbind(asthma_count, total_population - asthma_count) ~ pct_obesity + pct_daily_smokers + log_mean_AQI,
                     family = "binomial", data = asthma)
summary(logistic_model)
