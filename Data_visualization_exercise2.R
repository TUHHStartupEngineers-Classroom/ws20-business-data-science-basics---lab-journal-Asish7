library(tidyverse)
library(ggthemes)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_tbl <- covid_data_tbl %>%
  rename(cumulative_cases = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`, continent=continentExp, country=countriesAndTerritories)

##### challenge 2
world <- map_data("world")

covid_data_tbl_reshaped <- covid_data_tbl%>%
  filter(year==2020)


covid_data_tbl_reshaped  <- covid_data_tbl_reshaped  %>% 
  mutate(country = case_when(
    country == "United_Kingdom" ~ "UK",
    country == "United_States_of_America" ~ "USA",
    country == "Czechia" ~ "Czech Republic",
    country == "Bonaire, Saint Eustatius and Saba" ~ "Bonaire",
    TRUE ~ country
    
  ))

covid_data_tbl_reshaped <- covid_data_tbl_reshaped %>%
  group_by(country)%>%
  summarise(total_death = sum(deaths),population = mean(popData2019))


covid_data_tbl_reshaped  <- covid_data_tbl_reshaped  %>%
  mutate(country_new = str_replace_all(country, "_", " ") , Mortality_Rate = (total_death / population) * 100)

world <- dplyr::left_join(world, covid_data_tbl_reshaped , by=c("region" = "country_new"))


world %>%
  ggplot(aes(map_id = region)) +
  geom_map(aes(fill = Mortality_Rate), map = world, color = "white") +
  expand_limits(x = world$long, y = world$lat)+
  theme_minimal() 
