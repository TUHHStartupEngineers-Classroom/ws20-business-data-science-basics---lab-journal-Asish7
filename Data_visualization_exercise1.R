library(tidyverse)
library(ggthemes)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_tbl <- covid_data_tbl %>%
  rename(cumulative_cases = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`, continent=continentExp, country=countriesAndTerritories)


covid_data_tbl_reshaped <- covid_data_tbl %>%
  
  select(month,country ,continent, cases, deaths, year) %>%
  filter((country == "United_States_of_America" | 
           country == "Germany" | 
           country == "United_Kingdom" | 
           country == "France" | 
           country == "Spain") & year == "2020")%>%
  # Group by category and summarize
  group_by(country, month, year) %>%
  summarise(total_case = sum(cases)) %>%
  ungroup()%>%
  mutate(month=lubridate::month(month,label = TRUE,abbr = TRUE))


cumulative_case_tbl <- covid_data_tbl_reshaped %>% mutate(cum_sum=cumsum(total_case))


cumulative_case_tbl %>%
  
  ggplot(aes(month, cum_sum, group=country, color = country)) +
  
  geom_line(size=.5) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €"))+

  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 01/12/2020, USA has more cases than any other country",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country"
  )+  
  theme_minimal() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))
  



