library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(rvest)


#Getting the Urls

url <- "https://www.radon-bikes.de/"
#xopen(url)

url_home <- read_html(url)
url_home

bike_family_tbl<- url_home %>%
  html_nodes(css = ".js-dropdown > a")%>%
  html_text()%>%
  discard(.p = ~stringr::str_detect(.x,"DE|SERVICE|WEAR|LIFE")) %>%
  unique()%>%
  enframe(name = "position", value = "family_name")

bike_family_tbl

# 1.2 COLLECT PRODUCT CATEGORIES ----


bike_category_tbl <- url_home %>%
  html_nodes(css = ".megamenu__item> a") %>%
  html_attr('href')%>% 
  enframe(name = "position", value = "subdirectory")%>%
  mutate(
    url = glue("https://www.radon-bikes.de{subdirectory}bikegrid")
  )

bike_category_tbl


bike_category_url <- bike_category_tbl$url[1]
#xopen(bike_category_url)

html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  html_nodes(css = ".m-bikegrid__item > a") %>%
  html_attr("href") %>%
  enframe(name = "position", value = "category_url")%>%
  mutate(
  url = glue("https://www.radon-bikes.de{category_url}")
      )
bike_url_tbl 


# 2.1.2 Extract the descriptions 

bike_price_tbl <- html_bike_category %>%
  html_nodes('.m-bikegrid__price.currency_eur .m-bikegrid__price--active')%>%
  html_text()%>%
  enframe(name = "position", value = "price")

#bike_price_tbl

bike_title_tbl <- html_bike_category %>%
  html_nodes('.m-bikegrid__info .a-heading--small')%>%
  html_text()%>%
  enframe(name = "position", value = "title")

#bike_title_tbl


bike_tbl1 <- merge(x = bike_title_tbl, y = bike_price_tbl, by = "position")
bike_tbl_merged <- merge(x = bike_tbl1, y = bike_url_tbl, by = "position")




bike_tbl_merged <- bike_tbl_merged %>% 
  select(-position,-category_url)


bike_tbl_merged
