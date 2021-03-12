library(tidycensus)
library(tidyverse)
library(tmap)

census_api_key("7d3ec56a20c9f554972004e88aceb135e9d422e0")

##view variables
decennial_variables<-load_variables(2010,"sf1")
View(decennial_variables)

##State-level population in 2010

state_population_2010<-get_decennial(geography = "state", 
                                                  variables = "P001001", 
                                                  year = 2010)

##We can adjust the geography and year parameters; let's say we want the population distribution 
##across CO counties in the year 2000:

CO_county_population_2010<-get_decennial(geography = "county", 
                                         state="CO",
                                         variables = "P001001", 
                                         year = 2000)

##Let's clean up the table by removing the "variable" column, and renaming the "value" column as "Population"

CO_county_population_2010<-get_decennial(geography = "county", 
                                         state="CO",
                                         variables = "P001001", 
                                         year = 2000) %>% 
                              mutate(variable=NULL) %>% 
                              rename(population=value)



##It's also possible to get multiple variables in a single table; let's add a field/column containing
##the rural population in each state in 2010:

state_population_rural_2010<-get_decennial(geography = "state", 
                                          variables = c("P001001", "P002005"),
                                          output="wide",
                                          year = 2010) %>% 
                                          rename(total_population=P001001, rural_population=P002005) 

##Let's use dplyr to calculate a new column containing a variable based on our existing variables (i.e. rural population
##share by state)

state_rural_pct_2010<-state_population_rural_2010 %>% mutate(rural_pct=(rural_population/total_population)*100)
View(state_rural_pct_2010)

##visualize here 

##Let's say we want to create a new dataset that only contains states where the rural population exceeded 40%

states_most_rural<-state_rural_pct_2010 %>% filter(rural_pct>40)
View(states_most_rural)

##writing out data

##Student exercise...create a dataset of Colorado counties with a rural population exceeding 20%

co_rural_counties<-get_decennial(geography = "county",
                                 state="CO",
                                 variables = c("P001001", "P002005"),
                                 output="wide",
                                 year = 2010) %>% 
                  mutate(variable=NULL) %>% 
                  rename(total_population=P001001, rural_population=P002005) %>% 
                  mutate(rural_pct=(rural_population/total_population)*100) %>% 
                  filter(rural_pct>20)
                                  

##change over time
##Which state saw the largest decrease in its rural share of the population between 2000 and 2010? 

state_population_rural_2010<-get_decennial(geography = "state", 
                                           variables = c("P001001", "P002005"),
                                           output="wide",
                                           year = 2010) %>% 
                              mutate(rural_pct_2010=(P002005/P001001)*100) %>% 
                              dplyr::rename(population_total_2010=P001001) %>% 
                              dplyr::rename(population_rural_2010=P002005)


state_population_rural_2000<-get_decennial(geography = "state", 
                                           variables = c("P001001", "P002005"),
                                           output="wide",
                                           year = 2000) %>% 
                              mutate(rural_pct_2000=(P002005/P001001)*100) %>% 
                              dplyr::rename(population_total_2000=P001001) %>% 
                              dplyr::rename(population_rural_2000=P002005)

rural_change<-(full_join(state_population_rural_2000, state_population_rural_2010,by="NAME")) %>% 
              select(NAME,population_total_2000,population_rural_2000,rural_pct_2000,population_total_2010,
              population_rural_2010,rural_pct_2010) %>% 
              mutate(rural_change=rural_pct_2010-rural_pct_2000)

d<-rural_change %>%
  ggplot(aes(x = rural_change, y = NAME)) + 
  geom_point()

rural_change %>%
  ggplot(aes(x = rural_change, y = NAME)) + 
  geom_point()+

  
  
  
  
rural_change %>%
  ggplot(aes(x = reorder(NAME,-rural_change), y=rural_change)) + 
  geom_col()+
  coord_flip()
  
basegraph<-rural_change %>%
  ggplot(aes(x = reorder(NAME,rural_change), y=rural_change)) + 
  geom_col()+
  coord_flip()

basegraph+labs(title="Rural Depopulation", x="State Name", y="Pct Change in Rural Population")+
  theme(plot.title=element_text(hjust=0.5))

my_years<-c(2000,2010)
population_rural_2000_2010<-map(
  my_years,
  ~(get_decennial(geography = "state", 
                  variables = c("P001001", "P002005"),
                  output="wide",
                  year =.)) %>% 
    mutate(rural_pct=(P002005/P001001)*100) %>% 
    arrange(NAME)
)

names(population_rural_2000_2010)<-my_years

joined_ds<-st_join(population_rural_2000_2010[["2000"]],population_rural_2000_2010[["2010"]],by="NAME") %>% 
           mutate(pct_change=rural_pct.y-rural_pct.x) %>% 
           select(geometry,NAME,pct_change)
joined_ds

state_population_2010<-get_decennial(geography = "state", 
                                     variables = "P001001", 
                                     geometry=TRUE,
                                     shift_geo = TRUE,
                                     year = 2010)

rural_depop_tomap<-full_join(state_population_2010,joined_ds,by="NAME")

map4<-tm_shape(rural_depop_tomap)+
  tm_polygons(col="pct_change", n=6,style="jenks",palette="BuGn", midpoint=TRUE)


basegraph+theme(plot.title="Rural Depopulation"(hjust=0.5))






geom

ggplot2::reorder

co_rural_counties<-get_decennial(geography = "county",
                                 state="CO",
                                 variables = c("P001001", "P002005"),
                                 output="wide",
                                 year = 2010) %>% 
  mutate(variable=NULL) %>% 
  rename(total_population=P001001, rural_population=P002005) %>% 
  mutate(rural_pct=(rural_population/total_population)*100)

##see appendix

##data manipulation: find the county with the oldest median age in each state

median_age_county<- get_decennial(geography = "county", 
                             variables = "P013001", 
                             year = 2010) 
                             


median_age_county<-median_age_county %>% mutate(variable=NULL) %>% 
                                         rename(median_age=value)

oldest_median_age_county<-median_age_county %>% separate(NAME,c("County","State"),sep=",") %>% 
                                         group_by(State) %>% 
                                         arrange(desc(median_age)) %>% 
                                         slice(1)

oldest_median_age_county<-oldest_median_age_county %>% rename(median_age_high=median_age)

View(oldest_median_age_county)

##data manipulation: find the county with the youngest median age in each state (student exercise)

youngest_median_age_county<-median_age_county %>% 
                            separate(NAME,c("County","State"),sep=",") %>% 
                            group_by(State) %>% 
                            arrange(median_age) %>% 
                            slice(1) %>% 
                            rename(median_age_low=median_age)

##data manipulation: create table that contains difference between median age
##of county with highest median age, and median age of county with lowest median age,
##for each state (i.e. which state has the largest gap between oldest county and youngest county?)

combined_dataset<-full_join(oldest_median_age_county,youngest_median_age_county,by="State") 

county_median_age_range_by_state<-combined_dataset %>% 
                                  select(State,median_age_high,median_age_low) %>% 
                                  mutate(age_range_counties=median_age_high-median_age_low)


##visualization

##map


##ACS


##piping in other data

##appendix

multiyear<-map(
  my_years,
  ~get_decennial(geography = "state", 
                 variables = c("P001001", "P002005"),
                 output="wide",
                 year =.)
)

names(multiyear)<-my_years
multiyear

clean<-map(
  multiyear,
  ~(rename)




https://247wallst.com/special-report/2018/07/09/the-oldest-and-youngest-county-in-each-state-2/2/

http://lenkiefer.com/2016/05/22/population-growth-housing-supply-and-house-prices/

  https://www.businessinsider.com/youngest-and-oldest-counties-in-the-us-map-2016-6