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
                                     shift_geo = TRUE,
                                     year = 2010)
View(state_population_2010)


## Generate a table of the population distribution across Colorado counties in the year 2010, 
## based on the 2010 decennial census

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

## Create a new object containing a dataset of rural population AND overall population by state,
## based on the 2010 decennial. Also, rename the variables, and arrange the dataset in descending
## order with respect to the rural population

state_pop_ruralpop_2010<-get_decennial(geography = "state", 
                                       variables = c("P001001", "P002005"),
                                       output="wide",
                                       year = 2010) %>% 
                          rename(total_population=P001001, rural_population=P002005) %>% 
                          arrange(desc(rural_population))


View(state_pop_ruralpop_2010)

##Using dplyr, generate a new variable in the dataset based on existing variables; in particular,
##create a variable called "rural_pct" that reflects the rural population as a percentage of the
##population, and then 

state_pop_ruralpop_2010<- state_pop_ruralpop_2010 %>% 
                          mutate(rural_pct=(rural_population/total_population)*100) %>% 
                          arrange(desc(rural_pct))


View(state_pop_ruralpop_2010)

##Filtering datasets based on specified criteria; create a new object containing a state-
##dataset based on the 2010 decennial, with information on total and rural population (as 
## well as the percentage of the rural population with respect to the overall state population), 
##but that only includes states whose rural population share exceeds 40%

rural_pct_over40<-state_pop_ruralpop_2010 %>%
                  filter(rural_pct>40)

View(rural_pct_over40)

### Student Exercise 1: Create a dataset of Colorado counties whose rural population percentage
###(with respect to the overall county population) exceeded 50% (based on the 2010 decennial
### census). Sort the dataset in descending order with respect to the rural percentage variable.

# Part 2

# What is the percentage point difference in the rural share of the state population, from 2000
# to 2010? In other words, by how many percentage points did the rural share of state populations
# increase or decrease in the decade from 2000 to 2010?

## To answer this question, we first need to gather data on state-level overall and rural 
## population,from both the 2000 and 2010 census:

population_rural_2000_2010<-get_decennial(geography = "state", 
                                         variables = c("P001001", "P002005"),
                                         output="wide",
                                         year = c(2000,2010)) 

## That didn't work, because the "get_decennial" function in tidycensus can handle
## handle multiple variable inputs, but not multiple year inputs. To efficiently gather data
## from both the 2000 and 2010 decennials, we can use the tidyverse's "map" function, which
## is used to carry out operations that require iteration (the map function is essentially a 
## functional alternative to a for-loop). 

## generate a vector of year variables to iterate over
my_years<-c(2000,2010)

# generate a list containing datasets with information on state-level population (P001001),
# state-level rural population (P002005), and a newly created variable (rural_pct) that divides
# P001001 by P002005 and multiplies by 100, using the years in the "my_years" vector (these years
# will cycle through the "year" parameter when the function runs).
population_rural_2000_2010<-map(
  my_years,
  ~(get_decennial(geography = "state", 
                  variables = c("P001001", "P002005"),
                  output="wide",
                  year =.)) %>% 
    mutate(rural_pct=(P002005/P001001)*100) %>% 
    arrange(NAME)
)

## inspect the resulting list
population_rural_2000_2010

## assign names, based on the years in the "my_years" vector, to the list elements
names(population_rural_2000_2010)<-my_years

## view the final list
population_rural_2000_2010

## Calculate the state-level percentage point difference in the rural population between 2000 
## and 2010. 

## join the 2010 and 2000 datasets to create a single dataset 

rural_change<-full_join(population_rural_2000_2010[["2000"]],
                        population_rural_2000_2010[["2010"]],by="NAME")

View(rural_change)

## Amend the joined dataset by creating a new variable (rural_pctpt_change) that subtracts
## the state-level percentage of rural residents in 2010 from that in 2000

rural_change<-rural_change %>% 
              mutate(rural_pctpt_change=rural_pct.y-rural_pct.x) %>% 
              select(NAME,rural_pctpt_change)

View(rural_change)

## Visualize the change captured by the "rural_pctpt_change" variable in a chart, using the
## ggplot package 

rural_change_viz<-rural_change %>%
                  ggplot(aes(x = reorder(NAME,rural_pctpt_change), y=rural_pctpt_change)) + 
                  geom_col()+
                  coord_flip()+
                  labs(title="Rural Depopulation by State (2000-2010)", x="State Name", 
                  y="% Point Change in Rural Population from 2000 to 2010")+
                  theme(plot.title=element_text(hjust=0.5)) 

## Display the change captured by the "rural_pctpt_change" variable on a map

### Join table containing the "rural_pctpt_change" variable (i.e. the dataset assigned to
### the "rural_change" object) to the "state_population_2010" object we created above, since
## this object already has geometries assigned to it

rural_change_tomap<-full_join(state_population_2010,rural_change,by="NAME") %>% 
                    relocate(NAME)

### Create an object to hold our map
foundational_map<-tm_shape(rural_change_tomap)+
                  tm_polygons(col="rural_pctpt_change", n=6,style="jenks",
                              palette="BuGn", midpoint=TRUE, contrast=1)
### View the map
foundational_map

### change the static map into an interactive web map (it'll take a bit of tinkering 
### to re-shift Alaska and Hawaii, but this will give you an idea of what's possible)

tmap_mode("view")
foundational_map

### go back to a static plot

tmap_mode("plot")
foundational_map

### Add additional formatting to map

revised_map<-tm_shape(rural_change_tomap)+
             tm_polygons(col="rural_pctpt_change", breaks=c(-6,-4,-2, 0, 1, 2), 
                         palette="YlGnBu", midpoint=TRUE, contrast=1)+
             tm_layout(frame=FALSE, main.title="Percentage Point Change\nin Rural Population, By State (2000-2010)",  
             main.title.position="left", legend.outside=TRUE)+
             tm_credits("Source: Decennial Census")

revised_map


# Student Exercise 2

## Exercise 2a: Create a map (one static, and one dynamic), that shows county-level variation
## in the median age across the state of Colorado in 2010. 

## Exercise 2b: Create a visualization (using the ggplot package) of state-level variation in 
## the median age across the United States. 


#Load a table of variables for the 2015-2019 American Community Survey into memory, and assign it 
#to an object. 

ACS_5_2019<-load_variables(2019,"acs5")
View(ACS_5_2019)

## Use the tidycensus "get_acs" function to issue a call to the Census API to retrieve a 
## county-level dataset of median income, and assign it to an object called "median_income_2019";
## Within that function call, we'll also rename the column containing the median income estimate
## as "median_income", and arrange the dataset in descending order with respect to median
## income. 

median_income_2019<-get_acs(geography="county",
                       variables="B19013_001",
                       year=2019) %>% 
                    rename(median_income=estimate) %>% 
                    arrange(desc(median_income))

View(median_income_2019)

## Generate a table that contains the highest median-income county for each state in 2019 and assign it 
## to an object called "highest_income_counties_2019":

highest_income_counties_2019<-median_income_2019 %>% 
                              separate(NAME,c("County","State"),sep=",") %>% 
                              group_by(State) %>% 
                              arrange(desc(median_income)) %>% 
                              slice(1) %>% 
                              unite(NAME, c("County","State"), remove=FALSE, sep=",")


View(highest_income_counties_2019)

## Use ggplot to visualize the "highest_income_counties_2019" dataset, and incorporate the 
## margins of error for the estimates into the visualization:

highest_income_counties_viz<-highest_income_counties_2019 %>% 
                             ggplot(aes(x=median_income,y=reorder(NAME, median_income)))+
                             geom_errorbarh(aes(xmin = median_income - moe, xmax = median_income + moe)) +
                             geom_point(color = "red", size = 3)+
                             labs(title="County with Highest Median Income, by State",
                             y="",
                             x="Median Income Estimate from 5-year ACS\n(bars indicate margin of error)")+ 
                             theme(plot.title=element_text(hjust=0.5))

highest_income_counties_viz

#Practice Exercise 3a: Generate a visualization of median income by county in Colorado, using the
#2014-2018 ACS; include error bars to represent the MOE corresponding to each of the estimates

#Practice Exercise 3b: Generate an interactive nationwide map of median income by state
# in 2016 using the American Community Survey:

# Explore the correlation between county median income and the percentage of the county over-25
# population without at least a bachelor's degree (based on the 2018 5-year ACS)

ACS_5_2018<-load_variables(2018,"acs5")
View(ACS_5_2018)

##Generate a vector of variables to feed into the "get_acs" function
education_vars<-c(Bachelors="B15003_022", Masters="B15003_023", Professional="B15003_024", 
                  Doctorate="B15003_025") 

##Generate a dataset of educational attainment, with a reference population of the over-25 population,
##for each county
education_acs_2018<-get_acs(geography="county",
                            year=2018,
                            variables=education_vars,
                            summary_var="B15003_001")
View(education_acs_2018)

##Generate a dataset of the percentage of each county's over-25 population that does not possess
## at least a BA
pct_less_than_BA<-education_acs_2018 %>% 
                  group_by(GEOID) %>% 
                  mutate(no_degree_pct=((summary_est-sum(estimate))/(summary_est)*100)) %>% 
                  summarize(mean(no_degree_pct)) %>% 
                  rename(no_degree_pct="mean(no_degree_pct)")

View(pct_less_than_BA)

##Generate a dataset of median income based on the 2018 ACS, and join the "pct_less_than_BA"
##object from the previous step to this median income dataset to create a new dataset that
## is assigned to the object "median_income_nodegree

median_income_2018<-get_acs(geography="county",
                       variables="B19013_001",
                       year=2018) %>% 
                    rename(median_income=estimate) %>% 
                    arrange(desc(median_income))

median_income_nodegree<-full_join(median_income_2018 ,pct_less_than_BA,by="GEOID")

View(median_income_nodegree)


## Use the dataset created above (assigned to the "median_income_nodegree" object) to create
## a scatterplot of the correlation between county share of 25+ population without bachelors
## and median income

medianincome_nodegree_corr_viz<-
  median_income_nodegree %>% 
  ggplot()+
  geom_point(aes(x=no_degree_pct,y=median_income))+
  geom_smooth(aes(x=no_degree_pct,y=median_income),method="lm")+
  labs(title="Scatterplot of 25+ Share of Population Without BA vs Median Income,\nby County (2014-2018 ACS)",
       y="Estimated County Median Income",
       x="Estimated Percentage of Under-25 Population Without at least Bachelors Degree")+
  theme(plot.title=element_text(hjust=0.5))

medianincome_nodegree_corr_viz


##Calculate the correlation coefficient 
noba_medincome_corr<-cor.test(median_income_nodegree$median_income, 
                              median_income_nodegree$no_degree_pct)

#Calculate multiple state-level plots of the above relationship using facets

median_income_nodegree_facets<-median_income_nodegree %>% 
                               separate(NAME,c("County","State"),sep=",")

medianincome_nodegree_bystate_viz<-median_income_nodegree_facets %>% 
                                   ggplot()+
                                   geom_point(aes(x=no_degree_pct,y=median_income))+
                                   geom_smooth(aes(x=no_degree_pct,y=median_income),method="lm")+
                                   labs(title="Scatterplot of 25+ Share of Population Without BA vs Median Income,\nby County (2014-2018 ACS)",
                                        y="Estimated County Median Income",
                                        x="Estimated Percentage of Under-25 Population Without at least Bachelors Degree")+
                                   facet_wrap(~State)

medianincome_nodegree_bystate_viz


#Scatterplot of county median income against Covid cases per 100,000 people for Colorado
#counties (https://data-cdphe.opendata.arcgis.com/datasets/222c9d85e93540dba523939cfb718d76_0/data?geometry=-112.500%2C37.517%2C-98.602%2C40.504)

##Set working directory, read in CSV file of CO county-level COVID cases per 100,000, and clean
##CSV 
co_covid<-read_csv("co_covid.csv") %>% 
          mutate(GEOID=as.character(GEOID))

View(co_covid)

##join CSV of Covid cases/100000 to our earlier dataset of county-level median income (stored
## in the "median_income_2019" object)
co_covid_medianincome<-inner_join(median_income_2019, co_covid,by="GEOID")

View(co_covid_medianincome)

##generate a scatterplot of county median income vs Covid case rates in Colorado

co_covid_medianincome_viz<-co_covid_medianincome %>% 
                           ggplot()+
                           geom_point(aes(x=median_income, y=County_Rate_Per_100_000))+
                           geom_smooth(aes(x=median_income,y=County_Rate_Per_100_000),method="lm")+
                           labs(title="Scatterplot of County Median Income\nvs\nCounty Covid+ Rate Per 100,000",
                                x="Estimated County Median Income",
                                y="County Covid+ Rate Per 100,000",
                                caption = "Source: 2015-2019 ACS\nColorado Department of Public Health and Environment (CDPHE)")+
                           theme(plot.title=element_text(hjust=0.5))

co_covid_medianincome_viz

## calculate the correlation coefficient

income_cases_corr<-cor.test(co_covid_medianincome$median_income, co_covid_medianincome$County_Rate_Per_100_000)
income_cases_corr

#Practice Exercise 4: Calculate the percentage of Colorado counties' overall populations that are
#non-white and non-Hispanic (based on the 2015-2019 ACS). Create a scatterplot of CO counties'
#non-white, non-Hispanic population shares against county-level Covid+ rates per 100,000. 











