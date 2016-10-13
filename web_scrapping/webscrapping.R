library(httr)
library(xml2)
library(rvest)
#library(dplyr)
#library(tidyr)

#r <- GET("http://google.com", verbose (data_in = TRUE))
#http_status(r)
#content(r)
#Shttr::BROWSE(url)

url = "https://www.tripadvisor.com/Tourism-g150782-Monterrey_Northern_Mexico-Vacations.html"

#hotels <-
#  url %>% read_html() %>% html_nodes(xpath = "//a[@class='hotel name']") %>% html_text()

hotel_names <-
  url %>% read_html() %>% html_nodes("#HTL_FAVS .name") %>% html_text()

hotel_links <-
  url %>% read_html() %>% html_nodes("#HTL_FAVS .name") %>% html_attr('href')

hotel_ratings <-
  url %>% read_html() %>% html_nodes("#HTL_FAVS .sprite-ratings") %>% html_attr('alt')

df <- data.frame(hotel_names, hotel_ratings, hotel_links)

View(df)
