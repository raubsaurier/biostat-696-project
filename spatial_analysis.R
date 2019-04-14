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
exp.variog <- fit.variogram(emp.variog,vgm(psill=800, nugget=1500, "Lin"),fit.method=2)
exp.variog
# make a plot of the empirical semi-variogram with the exponential semi-variogram overlayed 
print(plot(emp.variog,exp.variog, main="Exponential Semi-Variogram"))


## specify the initial values for the parameters 
beta.init <- loglinear_model$coefficients# use the estimates of beta from our loglinear GLM model
sigma2.init <- exp.variog$psill[2] # partial sill 
phi.init <- 1/exp.variog$range[2] # range 
tau2.init <- exp.variog$psill[1]  #we want to include a nugget effect
coords <- as.matrix(cbind(x.dist, y.dist)) ##easting and northing coordinates from the data 

#### ---------------------------------------------
### Code for frequentist model w/ spatial effects: 
#### ---------------------------------------------


asthma_subset <- asthma_with_coords[,c("asthma_count", "obesity_rate_2016", 
                                      "pct_daily_smokers", "meanAQI.Ozone", "meanAQI.Other", "pct_black", "x.dist", "y.dist")]

asthma_subset <- asthma_subset[!is.na(asthma_subset$asthma_count)&!is.na(asthma_subset$pct_black)]

## get the covariates 
asthma_count <- asthma_subset$asthma_count
obesity_rate_2016 <- asthma_subset$obesity_rate_2016
pct_daily_smokers <- asthma_subset$pct_daily_smokers
meanAQI.Ozone <- asthma_subset$meanAQI.Ozone
meanAQI.Other <- asthma_subset$meanAQI.Other
pct_black <- asthma_subset$pct_black


br.geo <- as.geodata(asthma_subset,coords.col=c(7,8), data.col = 1, covar.col = c(2:6))

## fit the model 
set.seed(04122019)
br.reml <- likfit(geodata=br.geo, trend= asthma_count ~obesity_rate_2016 + pct_daily_smokers + meanAQI.Ozone + meanAQI.Other + pct_black,
                  cov.model="exponential",ini=c(sigma2.init, phi.init), nugget=tau2.init, lik.met="REML")
br.reml


#### ---------------------------------------------
##### Bayesian hierarchical model w/ spatial effects: 
#### ---------------------------------------------


## set up the model 
n.batch <- 1000
batch.length <- 100
n.samples = n.batch*batch.length
burn.in <- 0.5*n.samples

# for reproducibility
set.seed(20190301)
## fit a Bayesian GLM 
nyBayes <- spGLM(ny_data$log_cases ~  ny_data$Pct.homeowners + ny_data$Pct.over65 + ny_data$Avg.inv.dist.TCEs, weights = ny_data$Population.1980, family="poisson", 
                 coords=coords,starting=list("phi"=phi.init,"sigma.sq"=sigma2.init, "tau.sq"=tau2.init,"beta"=beta.init, "w"=0),
                 tuning=list("phi"=0.0075, "sigma.sq"=0.005, "tau.sq"=0.001, "beta"=c(0.02, 0.02, 0.02, 0.02), "w"=0.0001), #tried several different values of the tuning to make the trace plots look nicer 
                 priors=list("phi.Unif"=c(0.03, 0.9), "sigma.sq.IG"=c(3, 1.5),
                             "tau.sq.IG"=c(3, 1),"beta.Flat"), cov.model="exponential",n.samples=n.samples, verbose=TRUE, n.report=0.2*n.samples)

## Trace plots of the parameters
par(mai=rep(0.5,4))
plot(nyBayes$p.beta.theta.samples)



