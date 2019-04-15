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

#### ---------------------------------------------
##### Use kriging on centroid lat/long coordinates: 
#### ---------------------------------------------

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
exp.variog <- fit.variogram(emp.variog,vgm(psill=800, nugget=1500, "Exp"),fit.method=2)
exp.variog
# make a plot of the empirical semi-variogram with the exponential semi-variogram overlayed 
print(plot(emp.variog,exp.variog, main="Exponential Semi-Variogram"))


## specify the initial values for the parameters 
beta.init <- loglinear_model$coefficients# use the estimates of beta from our loglinear GLM model
sigma2.init <- exp.variog$psill[2] + 0.0001 # since this is 0, add a very small value so that the REML model can run 
phi.init <- 1/exp.variog$range[2] # range 
tau2.init <- exp.variog$psill[1]  #we want to include a nugget effect

#### ---------------------------------------------
### Code for frequentist model w/ spatial effects: 
#### ---------------------------------------------


asthma_model <- asthma_with_coords[,c("total_population","asthma_count", "obesity_rate_2016", 
                                      "pct_daily_smokers", "meanAQI.Ozone", "meanAQI.Other", "pct_black", "lat", "long")]

asthma_subset <- asthma_model[!is.na(asthma_model$asthma_count)]
coords <- as.matrix(cbind(asthma_subset$lat, asthma_subset$long)) 


## get the covariates 
asthma_count <- asthma_subset$asthma_count
obesity_rate_2016 <- asthma_subset$obesity_rate_2016
pct_daily_smokers <- asthma_subset$pct_daily_smokers
meanAQI.Ozone <- asthma_subset$meanAQI.Ozone
meanAQI.Other <- asthma_subset$meanAQI.Other
pct_black <- asthma_subset$pct_black
total_population <- asthma_subset$total_population


br.geo <- as.geodata(asthma_subset,coords.col=c(8,9), data.col = 2, covar.col = c(3,4,5,6,7))

## fit the model 
set.seed(04122019)
br.reml <- likfit(geodata=br.geo, trend= asthma_count ~obesity_rate_2016 + pct_daily_smokers + meanAQI.Ozone + meanAQI.Other + pct_black,
                  cov.model="exponential",ini=c(sigma2.init, phi.init), nugget=tau2.init, lik.met="REML")
br.reml


#### ---------------------------------------------
##### making predictions with the data 
#### ---------------------------------------------
# first we will use the above model to make predictions on the last 20 observations
# get the last 20 observations of the BR price data 
asthma_pred <- asthma_model[is.na(asthma_model$asthma_count)]

# the locations of the observations we want to predict on 
lat0 <- asthma_pred$lat
long0 <-asthma_pred$long

pred_coords <- as.matrix(cbind(lat0, long0))

## get the estimates of phi, sigma, tau from the REML 
sigma2.pred <- br.reml$sigmasq
phi.pred <-br.reml$phi
tau2.pred <- br.reml$tausq


# set up the model for prediction 
set.seed(03012019)
kc.ok.control <- krige.control(type.krige="ok",trend.d =asthma_count ~ obesity_rate_2016 + pct_daily_smokers + meanAQI.Ozone + meanAQI.Other + pct_black, 
                              obj.model = br.reml,
                              trend.l =  asthma_pred$asthma_count ~ asthma_pred$obesity_rate_2016 + asthma_pred$pct_daily_smokers + asthma_pred$meanAQI.Ozone + asthma_pred$meanAQI.Other +asthma_pred$pct_black,
                              cov.model="exponential", 
                              cov.pars=c(sigma2.pred,phi.pred),nugget=tau2.pred)


loc.ok <- matrix(c(lat0,long0), ncol=2)
kc.ok.s0 <- krige.conv(br.geo,locations=loc.ok,krige=kc.ok.control)
pred_asthma_counts <- kc.ok.s0$predict

#### ---------------------------------------------
##### Bayesian hierarchical model w/ spatial effects: 
#### ---------------------------------------------


## set up the model 
n.batch <- 2000
batch.length <- 100
n.samples = n.batch*batch.length
burn.in <- 0.5*n.samples

# for reproducibility
set.seed(20190301)
## fit a Bayesian GLM 
## not sure if I set the weights correctly, feel free to change!! 
asthmaBayes <- spGLM(asthma_count ~obesity_rate_2016 + pct_daily_smokers + meanAQI.Ozone + meanAQI.Other + pct_black
                 , weights = total_population, family="poisson", 
                 coords=coords,starting=list("phi"=phi.init,"sigma.sq"=sigma2.init, "tau.sq"=tau2.init,"beta"=beta.init, "w"=0),
                 tuning=list("phi"=0.000001, "sigma.sq"=0.00005, "tau.sq"=0.0001, beta=c(rep(0.00001, length(beta.init))), "w"=0.0001), #tried several different values of the tuning to make the trace plots look nicer 
                 priors=list("phi.Unif"=c(1, 10), "sigma.sq.IG"=c(2, 1),
                             "tau.sq.IG"=c(3, 1),"beta.Flat"), cov.model="gaussian",n.samples=n.samples, verbose=TRUE, n.report=0.2*n.samples)

## Trace plots of the parameters
par(mai=rep(0.5,4))
plot(asthmaBayes$p.beta.theta.samples)


#### ---------------------------------------------
## Bayesian predictions 
#### ---------------------------------------------

n.pred <- 18
pred_coords <- as.matrix(cbind(asthma_pred$lat, asthma_pred$long)) 
# the variables for the 18 sites 
asthma.predcov <- matrix(cbind(rep(1, n.pred), asthma_pred$obesity_rate_2016,  asthma_pred$pct_daily_smokers, asthma_pred$meanAQI.Ozone, asthma_pred$meanAQI.Other, 
                               asthma_pred$pct_black),nrow=n.pred,ncol=6)

# get the predictions at the 20 selected sites
set.seed(03012019)
bayesian_pred <- spPredict(asthmaBayes, pred.coords=coords, pred.covars=asthma.predcov, start=burn.in, verbose = FALSE, n.report=5000)

## posterior mean of the predictions 
post.pred.mean <- rowMeans(bayesian_pred$p.y.predictive.samples)
post.pred.mean[1:n.pred]

## posterior medians 
post.pred.median <- apply(bayesian_pred$p.y.predictive.samples,1,median)
post.pred.median[1:n.pred]


## 90% posterior predictive intervals 
post.pred.90ci <- apply(bayesian_pred$p.y.predictive.samples,1,quantile,c(0.05,0.95))
post.pred.90ci[,1:n.pred]



