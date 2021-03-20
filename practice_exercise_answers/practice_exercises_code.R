## Practice Exercise 1: Create a dataset of Colorado counties whose rural population percentage
## (with respect to the overall county population) exceeded 50% (based on the 2010 decennial
##  census). Sort the dataset in descending order with respect to the rural percentage variable.

rural_pct_over50_CO2010<-get_decennial(geography = "county",
                                       state="CO",
                                       variables = c("P001001", "P002005"),
                                       output="wide",
                                       year = 2010) %>% 
                          mutate(variable=NULL) %>% 
                          rename(total_population=P001001, rural_population=P002005) %>% 
                          mutate(rural_pct=(rural_population/total_population)*100) %>% 
                          filter(rural_pct>50) %>% 
                          arrange(desc(rural_pct))

View(rural_pct_over50_CO2010)

## Practice Exercise 2a: Create a map (one static, and one dynamic), that shows county-level 
## variation in the median age across the state of Colorado in 2010 

### Call the data from the API using tidycensus

median_age_CO<- get_decennial(geography = "county",
                              state="CO",
                              variables = "P013001", 
                              year = 2010,
                              geometry = TRUE) %>% 
                  rename(median_age=value) %>% 
                  relocate(NAME)

### Make a static map

median_age_CO_map<-tm_shape(median_age_CO)+
                   tm_polygons(col="median_age",breaks=c(30,35,40,45,50),
                               palette="YlGnBu",midpoint=TRUE)+
                    tm_layout(frame=FALSE, main.title="Median Age by County,\nColorado",  
                    main.title.position="left", legend.outside=TRUE)

median_age_CO_map

### Make a dynamic map

tmap_mode("view")
median_age_CO_map

## Practice Exercise 2b

median_age_CO_visualization<-
  median_age_CO %>%
  ggplot(aes(x = median_age, y = reorder(NAME, median_age))) + 
  geom_point()+
  labs(title="Median Age by County, CO", x="Median Age", y="County Name")+
  theme(plot.title=element_text(hjust=0.5))

median_age_CO_visualization

### 2b clean up the Y axis labels

median_age_CO_cleaned<-median_age_CO %>% 
  mutate(County_Name=str_remove_all(NAME,"Colorado|,|County"))

median_age_CO_cleaned_visualization<-
  median_age_CO_cleaned %>%
  ggplot(aes(x = median_age, y = reorder(County_Name, median_age))) + 
  geom_point()+
  labs(title="Median Age by County, CO", x="Median Age", y="County")+
  theme(plot.title=element_text(hjust=0.5))

median_age_CO_cleaned_visualization

#Practice Exercise 3a: Generate a visualization of median income by county in Colorado, using the
#2014-2018 ACS; include error bars to represent the MOE corresponding to each of the estimates

median_income_CO_2018<-get_acs(geography="county",
                               state="CO",
                               variables="B19013_001",
                               year=2018) %>% 
                        rename(median_income=estimate) %>% 
                        arrange(desc(median_income))

median_income_counties_CO_2018_viz<-median_income_CO_2018 %>% 
                                      mutate(County_Name=str_remove_all(NAME,"Colorado|,|County")) %>% 
                                      ggplot(aes(x=median_income,y=reorder(County_Name, median_income)))+
                                      geom_errorbarh(aes(xmin = median_income - moe, xmax = median_income + moe)) +
                                      geom_point(color = "blue", size = 3)+
                                      labs(title="Median Income in Colorado, by County (2018)",
                                      y="",
                                      x="Median Income Estimate from 5 year ACS\n(Bars indicate margin of error)")+
                                      theme(plot.title=element_text(hjust=0.5))
median_income_counties_CO_2018_viz                       


# Practice Exercise 3b: Generate an interactive nationwide map of median income by state
# in 2016 using the American Community Survey:


median_income_2016_states<-get_acs(geography="state",
                                   variables="B19013_001",
                                   year=2016,
                                   geometry=TRUE) %>% 
                           rename(median_income=estimate) %>% 
                           arrange(desc(median_income)) %>% 
                           relocate(NAME)

median_income_map<-tm_shape(median_income_2016_states)+
                    tm_polygons(col="median_income", n=6, style="fisher", palette="YlOrBr",
                                contrast=1)

tmap_mode("view")

median_income_map

#Practice Exercise 4

View(ACS_5_2019)

acs_2019_nonwhite_COcounties<-get_acs(geography="county",
                                      state="CO",
                                      variables="B03002_003",
                                      summary_var = "B03002_001",
                                      year=2019) %>% 
                              mutate(white_pct=(estimate/summary_est)*100) %>% 
                              mutate(nonwhite_pct=100-white_pct)


co_covid_race<-inner_join(acs_2019_nonwhite_COcounties, co_covid,by="GEOID")


co_covid_race_viz<-co_covid_race %>% 
                    ggplot()+
                    geom_point(aes(x=nonwhite_pct,y=County_Rate_Per_100_000))+
                    geom_smooth(aes(x=nonwhite_pct,y=County_Rate_Per_100_000),method="lm")+
                    labs(title="Scatterplot of County Median Income\nvs\nCounty Covid+ Rate Per 100,000",
                    x="Non-White Percentage of County Population",
                    y="County Covid+ Rate Per 100,000",
                  caption = "Source: 2015-2019 ACS\nColorado Department of Public Health and Environment (CDPHE)")+
                  theme(plot.title=element_text(hjust=0.5))

co_covid_race_viz




