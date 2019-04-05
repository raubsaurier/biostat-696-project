
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

#### ---------------------------------------------
###   Smoking Data Analysis 
#### ---------------------------------------------

## load in smoking data 
smokingData <- data.table(read.csv("IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv"))
smokingData <- smokingData[!state%in%c("National", "Alaska", "Hawaii")]
aggregatedSmoking <- smokingData[,list(average_smoking_prevalence=mean(na.omit(total_mean)), 
                                average_lower_bound=mean(na.omit(total_lb)), average_upper_bound=mean(na.omit(total_ub))), by=c("state", "sex", "year")]


smoking2012 <- aggregatedSmoking[year==2012]
graphData <- smoking2012[sex=="Both"]
graphData$fips <- fips(graphData$state)


## plot smoking prevalence on a state level
plot_usmap(data =smoking2012[sex=="Both"], values = "average_smoking_prevalence", lines = "black") + 
  scale_fill_gradient2(low="green",high="red",midpoint = mean(smoking2012[sex=="Both"]$average_smoking_prevalence),
                       breaks=round(unname(quantile(smoking2012[sex=="Both"]$average_smoking_prevalence)),2), name="Prevalence Rates") +
                         theme(legend.position = "right") + 
                         labs(title="2012 Smoking Prevalence by State") 
                       

## female specific smoking prevalence                        
plot_usmap(data =smoking2012[sex=="Females"], values = "average_smoking_prevalence", lines = "black") + 
  scale_fill_gradient2(low="green",high="red",midpoint = mean(smoking2012[sex=="Females"]$average_smoking_prevalence),
                       breaks=round(unname(quantile(smoking2012[sex=="Both"]$average_smoking_prevalence)),2), name="Prevalence Rates") +
  theme(legend.position = "right") + 
  labs(title="2012 Female Smoking Prevalence by State") 




## Male specific smoking prevalence                        
plot_usmap(data =smoking2012[sex=="Males"], values = "average_smoking_prevalence", lines = "black") + 
  scale_fill_gradient2(low="green",high="red",midpoint = mean(smoking2012[sex=="Males"]$average_smoking_prevalence),
                       breaks=round(unname(quantile(smoking2012[sex=="Both"]$average_smoking_prevalence)),2), name="Prevalence Rates") +
  theme(legend.position = "right") + 
  labs(title="2012 Male Smoking Prevalence by State") 

