# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(ggplot2)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path="G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("G:/Data Science Project-R/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")



# 3.0 Examining Data ----


# 4.0 Joining Data ----

left_join(orderlines_tbl, bikes_tbl,by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))



# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ",")%>%
  mutate(total.price = price * quantity)%>%
  select(-...1, -gender,-ends_with(".id"))%>%
  

rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))



# 6.0 Business Insights ----

# Step 1 - Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state,order_date, total_price) %>%
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize



sales_by_state_tbl %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "red") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by State",
    subtitle = "Upward Trend",
    x = "State", # Override defaults for x and y
    y = "Revenue"
  )



# Step 1 - Manipulate
sales_by_state_tbl_1 <- bike_orderlines_wrangled_tbl %>%
  select(state,order_date, total_price) %>%
  mutate(year = year(order_date))%>%
  group_by(year,state) %>% 
  summarize(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))




# Step 2 - Visualize facet


sales_by_state_tbl_1 %>%
  ggplot(aes(x =year , y = sales, fill = state)) +
  geom_col(fill = "red")+
  facet_wrap(~ state) +
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
 

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and State",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

