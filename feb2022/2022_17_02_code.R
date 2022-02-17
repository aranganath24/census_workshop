## Install Packages, Load Libraries, and Enter Census API Key (if you don't have a key, request
## one at https://api.census.gov/data/key_signup.html) 

install.packages(c("tidycensus", "tidyverse", "tmap", "sf"))

library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)

census_api_key("KEY GOES HERE")

## Part 1

## Load table of 2010 decennial census variables, and assign it to an object "decennial variables"

decennial_variables<-load_variables(2010,"sf1")
View(decennial_variables)

## Generate a table of population by state, based on the 2010 decennial census

state_population_2010<-get_decennial(geography = "state", 
                                     variables = "P001001", 
                                     geometry=TRUE,
                                     year = 2010)
View(state_population_2010)


#Generate a table of the population distribution across Colorado counties in the year 2010, 
#based on the 2010 decennial census

CO_county_population_2010<-get_decennial(geography = "county", 
                                         state="CO",
                                         variables = "P001001", 
                                         year = 2010)
View(CO_county_population_2010)

## Clean up the table of CO population by county by removing the "variable" column, and renaming
## the "value" column as "Population", which we can do using the tidyverse's "dplyr" package

CO_county_population_2010<-CO_county_population_2010 %>% 
                              mutate(variable=NULL) %>% 
                              rename(population=value)

View(CO_county_population_2010)


# Create a new object containing a dataset of rural population AND overall 
#population by state,based on the 2010 decennial. 
#Also, renames the variables, and arrange the dataset in descending order with 
#respect to the rural population

state_pop_ruralpop_2010<-get_decennial(geography = "state", 
                                       variables = c("P001001", "P002005"),
                                       output="wide",
                                       year = 2010) %>% 
  rename(total_population=P001001, rural_population=P002005) %>% 
  arrange(desc(rural_population))


View(state_pop_ruralpop_2010)


#Using dplyr, generate a new variable in the dataset based on existing variables; 
# in particular, create a variable called "rural_pct" that reflects the rural population 
# as a percentage of the population, and then arranges it in descrend order
# with respect to the new "rural_pct" variable

state_pop_ruralpop_2010<-state_pop_ruralpop_2010 %>% 
                         mutate(rural_pct=(rural_population/total_population)*100) %>% 
                         arrange(desc(rural_pct))


View(state_pop_ruralpop_2010)

# Extracts observations from "state_pop_ruralpop_2010" where rural_pct>40 
# and assigns to a new object named "rural_pct_over40"

rural_pct_over40<-state_pop_ruralpop_2010 %>%
                    filter(rural_pct>40)
















