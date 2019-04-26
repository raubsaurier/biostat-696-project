###------------------------------------
## Script for Spatial Data Analysis
## 2016 data
## Subset on 22 Northeast/Midwest states
##--------------------------------------

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
library(CARBayes)
library(maptools)
library(spdep)
library(dplyr)
library(coda)
library(gridExtra)

## set working directory
# setwd("~/repos/biostat-696-project")
# setwd("~/Documents/BIOSTATS 696 - Spatial Data Analysis/biostat-696-project")

## Read in the data
asthma = data.table(read.csv("2016Asthma_Final_w_KrigingParams_PM25.csv"))
asthma$fips = as.numeric(fips(asthma$state))

###-------------------------------------
## Subset on states with data/neighbors
## total of 22 states
###-------------------------------------
asthma[which(asthma$state == "New Hampshire"), "asthma_count"] = 28477
### 4/20 update -> I found 2015 asthma % data for Iowa (wrote about it more in the Overleaf paper)
# assuming the asthma rate stayed constant into the next year, multiply Iowa population by 0.06: 
asthma[which(asthma$state == "Iowa"), "asthma_count"] = 43822
##### Checked the kidscenter data source with the CDC -> for the most part, it seems that the kids center statistic overestimates asthma prevelance 
## (assuming CDC is gold standard) -> usually it is 1% higher 
## NOTE: MUST CHANGE POPULATION COUNTS TO REFLECT CHANGE IN YEAR

states_subset = c("Maine", "New Hampshire", "Vermont", "New York",
                  "Massachusetts", "Connecticut", "Rhode Island",
                  "New Jersey", "Pennsylvania", "Ohio", "Wisconsin",
                  "Indiana", "Kentucky", "Michigan", "Illinois",
                  "Missouri", "Iowa", "Minnesota", "Nebraska", 
                  "Kansas", "Oklahoma")
asthma_sub = asthma %>%
  filter(state %in% states_subset)

plot_usmap(data = asthma_sub[,1:3], values = "asthma_count", lines = "black", labels = TRUE) + 
  scale_fill_gradientn(colours=blue2red(8), name="Asthma Count") +
  theme(legend.position = "right") + 
  labs(title="2016 Asthma Counts")

asthma_percentage = (asthma_sub$asthma_count / asthma_sub$total_population) * 100
plot_usmap(data = cbind(asthma_sub[,1:2], asthma_percentage), values = "asthma_percentage", lines = "black", labels = TRUE) + 
  scale_fill_gradientn(colours=blue2red(8), name="Asthma Rate (in %)") +
  theme(legend.position = "right") + 
  labs(title="2016 Asthma Prevalence Rates by State")

## AQI graph
plot_usmap(data = asthma_sub, values = "meanAQI.Ozone", lines = "black", labels = TRUE) + 
  scale_fill_gradientn(colours=blue2red(8), name="Mean AQI") +
  theme(legend.position = "right") + 
  labs(title="2016 Mean AQI by State")

## Obesity Graphs 
obesity_plot = plot_usmap(data = asthma_sub, values = "obesity_rate_2016", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="Adult Obesity Rate") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Adult Obesity Rate by State")

## Smoking Graph
## AQI graph
smoking_plot = plot_usmap(data = asthma_sub, values = "pct_daily_smokers", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="% of Daily Smokers") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Daily Smokering Rate by State")

grid.arrange(obesity_plot, smoking_plot, ncol = 2)

## PM2.5 graph
pm2.5_plot = plot_usmap(data = asthma_sub, values = "meanAQI.PM2.5", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="PM 2.5 Level") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Average PM 2.5 Levels")

## Ozone graph
ozone_plot = plot_usmap(data = asthma_sub, values = "meanAQI.Ozone", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="Ozone Concentration") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Average Ozone Concentration")

grid.arrange(pm2.5_plot, ozone_plot, ncol = 2)

## % Black Graphs 
race_plot = plot_usmap(data = asthma_sub, values = "pct_black", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="% of Black Residents") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Percentage of\nBlack Residents")

## High School Education Graph
edu_plot = plot_usmap(data = asthma_sub, values = "pct_high_school", lines = "black") + 
  scale_fill_gradientn(colours=blue2red(8), name="% with a High School Degree") +
  theme(legend.key.size = unit(.35, "cm"), legend.position = "right") + 
  labs(title="Percentage with a\nHigh School Degree")

grid.arrange(race_plot, edu_plot, ncol = 2)

####-----------------------------
## test moran's I for overall counts
####-----------------------------
# create spatial polygon of US
US = map("state",fill=TRUE,plot=FALSE)
US.poly = map2SpatialPolygons(US,IDs=sapply(strsplit(US$names,":"),function(x) x[1]),
                              proj4string=CRS("+proj=longlat + datum=wgs84"))
US.poly = US.poly[which(names(US.poly) %in% tolower(states_subset))]
US.nb = poly2nb(US.poly)
US.weights = nb2WB(US.nb)
US.list.w = nb2listw(US.nb)

# test Moran's I
moran.test(asthma_sub$asthma_count / asthma_sub$total_population, 
           listw = US.list.w, randomisation=FALSE)

  # Moran's I value is .0278 with corresponding pvalue = .3122 which is not significant at a 0.05 significance level, 
  # indicating that we fail to reject the null hypothesis and conclude that there is no spatial autocorrelation in the 
  # percentage of children in the population who have asthma between states

###----------------------------------
## test Moran's I for health indicators model
###----------------------------------
health_model = glm(asthma_count ~ offset(log(total_population)) + obesity_rate_2016 + pct_daily_smokers,
                   family = "poisson",
                   data = asthma_sub)
summary(health_model)
  # all variables significant
  # beta_obesity = -0.017, beta_smoking = -0.71
  # there seems to be a decrease in the prevalence of asthma with higher rates of obesity and smoking ????

moran.test(residuals(health_model),
           listw = US.list.w, randomisation = FALSE)
  # Moran's I value is -0.0673 with corresponding pvalue = .5 which is not significant at the 0.05 significance level,
  # indicating that we fail to reject the null hypothesis and conclude that there is no spatial autocorrelation in the
  # residuals of the number of children who get asthma when predicted by obesity rate and percentage of smokers in the population


###-------------------------------------
## Disease mapping for health indicators
###------------------------------------
set.seed(696)

# get weights
adj.US = US.weights$adj
rep.US = rep(1:nrow(asthma_sub),US.weights$num)
W = matrix(0, nrow(asthma_sub), nrow(asthma_sub))
for (i in 1:nrow(asthma_sub)) {
  W[i, adj.US[rep.US == i]] = rep(1, US.weights$num[i])
}

asthma_count = round(asthma_sub$asthma_count / 1000)
total_population = round(asthma_sub$total_population / 1000)


# run Bayesian spatial linear model w/ improper CAR prior for health covariates
health_dismap = S.CARleroux(formula =asthma_count ~ log(offset(total_population)) +
                            asthma_sub$obesity_rate_2016 + asthma_sub$pct_daily_smokers,
                          W=W,
                          family = "poisson",
                          burnin = 50000,
                          n.sample = 200000,
                          thin = 1,
                          rho=1,
                          prior.mean.beta = NULL,
                          prior.var.beta = NULL,
                          prior.nu2 = NULL,
                          prior.tau2 = NULL,
                          verbose = TRUE)



health_dismap$summary.results
  # obesity: non-significant based on the 95% credible interval. Overall, obesity is negatively related to asthma prevalence
  # smoking: non-significant based on the 95% credible interval. Overall, smoking is positively related to asthma prevalence
  # tau^2: measure of spatial variation. The estimate is positive suggesting that we have more events than what we would expect randomly

## function to get traceplots 
healthBetas <- c("intercept","offset","obesity_rate_2016", "pct_daily_smokers") #intercept + covariates 

## for loop to make the traceplots of the betas 
for(i in 1:length(healthBetas)){
  traceplot(health_dismap$samples$beta[,i], main=paste0("Trace of ",healthBetas[i]))
  plot(density(health_dismap$samples$beta[,i]), main=paste0("Density of ",healthBetas[i]))
}

## traceplots of the spatial parameters 
traceplot(health_dismap$samples$tau2, main="Trace of Tau2")
traceplot(health_dismap$samples$phi, main="Trace of Phi")

####### estimated risk #########

###estimated risk from the model 
health_risk <- health_dismap$fitted.values/asthma_sub$total_population
health_round_risk <- round(quantile(health_risk),5)
health_risk_cuts <- cut(health_round_risk,5)

### we get a very very low estimated risk from the health model
plotclr = brewer.pal(5, "Blues")
class = classIntervals(health_risk, 5)
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Estimated Risk from Health Model")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = health_risk_cuts,
       fill = plotclr, cex = .75, ncol = 1, bty = "n")

# posterior median
health_samples = health_dismap$samples$phi
health_median = as.numeric(apply(health_samples, 2, median))


plotclr = brewer.pal(4, "RdBu")[4:1]
class = classIntervals(health_median, 4,
                       style = "fixed",
                       fixedBreaks = c(-.12, -.06, 0, .06, .12))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Health Model\nPosterior Median")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.12, -.06)", "[-.06, 0)", "[0, .06)", "[.06, .12]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states in the Midwest have negative median spatial effects, meaning that those
  # states had less asthma than would be expected by their smoking and obesity levels. 
  # Contrasting, most of the Northeast had more asthma than would be exptected by their smoking
  # and obesity levels.

# posterior 95% lower confidence interval bound
health_lower = as.numeric(apply(health_samples, 2, quantile, .025))

plotclr = brewer.pal(4, "Blues")[4:1]
class = classIntervals(health_lower, 4,
                       style = "fixed",
                       fixedBreaks = c(-.27, -.20, -.14, -.08, 0))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Health Model\nLower Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.27, -.20)", "[-.20, -.14)", "[-.14, -.08)", "[-.08, 0]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # All of the states have a negative lower bound of their spatial random effects

# posterior 95% upper confidence interval bound
health_upper = as.numeric(apply(health_samples, 2, quantile, .975))

plotclr = brewer.pal(4, "Reds")[1:4]
class = classIntervals(health_upper, 4,
                       style = "fixed",
                       fixedBreaks = c(0, .08, .16, .24, .32))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Health Model\nUpper Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[0, .08)", "[.08, .16)", "[.16, .24)", "[.24, .32]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # All of the states have a positive upper bound of their spatial random effects

  # since all of the lower bounds of the spatial random effects were negative and all of the upper bounds
  # of the upper bounds of the spatial random effects were positive, no states have a significant
  # difference in their asthma prevalence than would be expected by smoking and obesity values within that state

###-------------------------------------
## Bayesian model w/ improper CAR for environmental indicators
###------------------------------------

env_dismap = S.CARleroux(formula = asthma_count~ log(offset(total_population)) + 
                           asthma_sub$meanAQI.PM2.5 + asthma_sub$meanAQI.Ozone +asthma_sub$meanAQI.Other,
                         W=W,
                         family = "poisson",
                         burnin = 50000,
                         n.sample = 200000,
                         thin = 1,
                         rho=1,
                         prior.mean.beta = NULL,
                         prior.var.beta = NULL,
                         prior.nu2 = NULL,
                         prior.tau2 = NULL,
                         verbose = TRUE)

### Traceplots for the beta parameters and spatial parameters: 

## function to get traceplots of the parameters 
environ_betas <- c("intercept","offset","meanAQI.PM2.5", "meanAQI.Ozone", "meanAQI.Other") #intercept + covariates 

## for loop to make the traceplots + density plots of the betas 
for(i in 1:length(environ_betas)){
  traceplot(env_dismap$samples$beta[,i], main=paste0("Trace of ",environ_betas[i]))
  plot(density(env_dismap$samples$beta[,i]), main=paste0("Density of ",environ_betas[i]))
}

## traceplots + density plots of the spatial parameters 

traceplot(env_dismap$samples$phi, main="Traceplot of Phi")
plot(density(env_dismap$samples$phi), main="Density of Phi")
## density + trace of tau2
traceplot(env_dismap$samples$tau2,, main="Traceplot of Tau2")
plot(density(env_dismap$samples$tau2), main="Density of Tau2")

env_dismap$summary.results
# PM2.5: non-significant based on the 95% credible interval. Overall, PM2.5 is positively related to asthma prevalence
# ozone: non-significant based on the 95% credible interval. Overall, ozone concentration is positively related to asthma prevalence
# tau^2: measure of spatial variation. The estimate is positive suggesting that we have more events than what we would expect randomly


###estimated risk from the model 
env_risk <- env_dismap$fitted.values/asthma_sub$total_population
env_round_risk <- round(quantile(env_risk),5)
env_risk_cuts <- cut(env_round_risk,5)

### we get a very very low estimated risk from the environmental model
plotclr = brewer.pal(5, "Blues")
class = classIntervals(env_risk, 5)
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Estimated Risk from Environmental Model")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = env_risk_cuts,
       fill = plotclr, cex = .75, ncol = 1, bty = "n")

# posterior median
env_samples = env_dismap$samples$phi
env_median = as.numeric(apply(env_samples, 2, median))

plotclr = brewer.pal(4, "RdBu")[4:1]
class = classIntervals(env_median, 4,
                       style = "fixed",
                       fixedBreaks = c(-.12, -.06, 0, .06, .12))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Environmental Model\nPosterior Median")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.12, -.06)", "[-.06, 0)", "[0, .06)", "[.06, .12]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states in the Midwest have negative median spatial effects, meaning that those
  # states had less asthma than would be expected by their PM2.5 and ozone concentration levels. 
  # Contrasting, the Northeast had more asthma than would be exptected by their PM2.5 and ozone
  # concentration levels.

# posterior 95% lower confidence interval bound
env_lower = as.numeric(apply(env_samples, 2, quantile, .025))

plotclr = brewer.pal(4, "Blues")[4:1]
class = classIntervals(env_lower, 4,
                       style = "fixed",
                       fixedBreaks = c(-.27, -.20, -.14, 0, .005))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Environmental Model\nLower Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.27, -.20)", "[-.20, -.14)", "[-.14, 0)", "[0, .005]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states have a negative lower bound of their spatial random effects
  # except for Massachusetts which has a sligtly positive upper bound

# posterior 95% upper confidence interval bound
env_upper = as.numeric(apply(env_samples, 2, quantile, .975))

plotclr = brewer.pal(4, "Reds")[1:4]
class = classIntervals(env_upper, 4,
                       style = "fixed",
                       fixedBreaks = c(-.005, 0, .15, .22, .31))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Environmental Model\nUpper Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.005, 0)", "[0, .15)", "[.15, .22)", "[.22, .31]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states have a positive upper bound of their spatial random effects
  # Ohio and Illinois have negative upper bounds of thier spatial random effects. 

  # overall most of the states do not have significant spatial random effects, meaning that their
  # asthma prevalence is no different than what we would expect given their PM2.5 and ozone concentration
  # levels. Contrasting, Ohio and Illinois have negative spatial random effects meaning that they have less
  # asthma prevalence than would be expected by their PM2.5 and ozone concentration whereas Massachusetts
  # has a positive spatial random effect and thus has more asthma than would be expected by PM2.5 and
  # ozone concentration. 

###-------------------------------------
## Disease mapping for socioeconomic indicators
###------------------------------------

## I realized that we can't actually run a disease mapping model since we don't have "expected" cases of asthma
# so I changed this to be a regular Bayesian linear model w/ spatial RF and an improper CAR prior
#also wrote about it in the overleaf paper# 

socio_dismap = S.CARleroux(formula = asthma_count ~log(offset(total_population)) + 
                             asthma_sub$pct_black + asthma_sub$pct_high_school,
                         W=W,
                         family = "poisson",
                         burnin = 50000,
                         n.sample = 200000,
                         thin = 1,
                         rho=1,
                         prior.mean.beta = NULL,
                         prior.var.beta = NULL,
                         prior.nu2 = NULL,
                         prior.tau2 = NULL,
                         verbose = TRUE)

## function to get traceplots 
socio_betas <- c("intercept","offset","pct_black", "pct_high_school") #intercept + covariates 
colnames(env_dismap$samples$beta) <- socio_betas


## for loop to make the traceplots of the betas 
for(i in 1:length(socio_betas)){
  traceplot(socio_dismap$samples$beta[,i], main=paste0("Trace of ",socio_betas[i]))
  plot(density(socio_dismap$samples$beta[,i]), main=paste0("Density of ",socio_betas[i]))
}



socio_dismap$summary.results
  # pct_black: non-significant based on the 95% credible interval. Overall, pct_black is positively related to asthma prevalence
  # pct_high_school: non-significant based on the 95% credible interval. Overall, pct_high_school is negatively related to asthma prevalence
  # tau^2: measure of spatial variation. The estimate is positive suggesting that we have more events than what we would expect randomly

### risks from the 
###estimated risk from the model 
SE_risk <- socio_dismap$fitted.values/asthma_sub$total_population
SE_round_risk <-  round(quantile(SE_risk), 5)
SE_risk_cuts <- cut(SE_round_risk,5)

### we get a very very low estimated risk from the environmental model
plotclr = brewer.pal(5, "Blues")
class = classIntervals(SE_risk, 5)
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Estimated Risk from Socio-Economic Model")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = SE_risk_cuts,
       fill = plotclr, cex = .75, ncol = 1, bty = "n")



# posterior median
socio_samples = socio_dismap$samples$phi
socio_median = as.numeric(apply(socio_samples, 2, median))

plotclr = brewer.pal(4, "RdBu")[4:1]
class = classIntervals(socio_median, 4,
                       style = "fixed",
                       fixedBreaks = c(-.12, -.06, 0, .06, .13))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Socioeconomic Model\nPosterior Median")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.12, -.06)", "[-.06, 0)", "[0, .06)", "[.06, .13]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states in the Midwest have negative median spatial effects, meaning that those
  # states had less asthma than would be expected by their PM2.5 and ozone concentration levels. 
  # Contrasting, the Northeast had more asthma than would be exptected by their PM2.5 and ozone
  # concentration levels.

# posterior 95% lower confidence interval bound
socio_lower = as.numeric(apply(socio_samples, 2, quantile, .025))

plotclr = brewer.pal(4, "Blues")[4:1]
class = classIntervals(env_lower, 4,
                       style = "fixed",
                       fixedBreaks = c(-.29, -.21, -.14, 0, .02))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Socioeconomic Model\nLower Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[-.29, -.21)", "[-.21, -.14)", "[-.14, 0)", "[0, .02]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # Most of the states have a negative lower bound of their spatial random effects
  # except for Massachusetts which has a sligtly positive upper bound 

# posterior 95% upper confidence interval bound
socio_upper = as.numeric(apply(socio_samples, 2, quantile, .975))

plotclr = brewer.pal(4, "Reds")[1:4]
class = classIntervals(socio_upper, 4,
                       style = "fixed",
                       fixedBreaks = c(0, .07, .15, .21, .29))
colcode = findColours(class, plotclr)
plot(US.poly, border = "black", axes = TRUE, main = "Spatial Random Effects of Socioeconomic Model\nUpper Bound")
plot(US.poly, col = colcode, add = TRUE)
legend(x = "bottomright", legend = c("[0, .07)", "[.07, .15)", "[.15, .21)", "[.21, .29]"),
       fill = plotclr, cex = .75, ncol = 1, bty = "n")
  # All of the states have a positive upper bound of their spatial random effects

  # All of the states besides Massachuessets have insignificant spatial random effects.
  # Massachuestts has a positive spatial random effect meaning they have more asthma than
  # would be expected by the percentage of the population that is Black and the percentage
  # of the population with a high school degree. 



