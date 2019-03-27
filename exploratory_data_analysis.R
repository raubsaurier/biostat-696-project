
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

#### Air quality data#### 

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

## will use this later 
trump.pct <- continentalData$pct.Trump
# dataset with two variables - state and % of trump votes 
trumpData <- continentalData[,c("fips", "pct.Trump")]

# get the quintile breaks for plotting 
trumpData$quintile_breaks <- cut(trumpData$pct.Trump, quantile(trumpData$pct.Trump, c(0, 0.2, 0.4, 0.6, 0.8, 1)))

## plot the quintile-level % of voters for Trump in 2016 on a state level
plot_usmap(data = trumpData, values = "quintile_breaks", lines = "black") + 
  scale_fill_brewer(palette="Reds", name="% of Trump Votes by State \n measured in Quintile") +
  theme(legend.position = "right") + 
  labs(title="2016 Election Data - % of Trump Voters by State") 
## plot the pollution values: 

ggplot(data = states) + 
  geom_polygon(data=graphData, aes(x = long, y = lat, fill = Value, group = group), color = "white") + 
  coord_fixed(1.3)

#### Asthma data for Kansas by County for 2000-2017 #####

asthmaData <- data.table(read_excel("kansas_asthma_rate.xlsx"))

# let's do the year 2011 and by county level 
countyData <- asthmaData[LocationType=="County"&TimeFrame==2011]

get_fips <- function(state, county){
  x <- fips(state, county)
  return(x)
}
countyData$fips <- mapply(get_fips, "Kansas", countyData$Location)

# dataset with two variables - FIPs number and the rates 
graphData <- countyData[,c("fips", "Data")]
graphData$Data <- as.numeric(graphData$Data)

## check what the median rate is 
median(graphData$Data)

## plot  on a state level
plot_usmap(include = "KS", data = graphData, values = "Data", lines = "black") + 
  scale_fill_gradient2(low="blue",midpoint=median(graphData$Data), high="red", name="Asthma Rates") +
  theme(legend.position = "right") + 
  labs(title="2011 Asthma Rates per 1,000 Population - Kansas County Level") 





