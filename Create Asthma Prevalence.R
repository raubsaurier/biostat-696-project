#################################
#
# Create dataset combining asthma prevalence with offset (population under age 18) per state
#
#################################
library(dplyr)

prevalence = read.csv("2016prevalence")

population = read.csv("Child population by age group.csv")

population = population %>%
  filter(Location != "United States" &
           TimeFrame == 2016 &
           Age.group == "Total less than 18" &
           DataFormat == "Number") %>%
  select(Location, Data) %>%
  rename(total_population = Data)

# function to change state abbreviation to state name
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

prevalence = prevalence %>%
  mutate(Location = abb2state(state)) %>%
  select(Location, total_count) %>%
  inner_join(., population)

write.csv(prevalence, "2016 Asthma Prevalence Complete.csv", row.names = FALSE)
