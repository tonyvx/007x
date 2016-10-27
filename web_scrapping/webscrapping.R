library(httr)
library(xml2)
library(rvest)


readDestination <- function()
{ 
  n <- readline(prompt="Destination - ")
  if (n.na(n)){
    n <- readDestination()
  }
  return(n)
}

print(readDestination())
#library(dplyr)
#library(tidyr)

#r <- GET("http://google.com", verbose (data_in = TRUE))
#http_status(r)
#content(r)
#Shttr::BROWSE(url)

url = "https://www.tripadvisor.com/Search?geo=1&redirect&q="

#hotels <-
#  url %>% read_html() %>% html_nodes(xpath = "//a[@class='hotel name']") %>% html_text()
search <- hotel_names <-
  paste0(url,'Monterrey,Mexico') %>% read_html() %>% html_nodes(".title span") %>% html_attr("href")
hotel_names <-
  url+'Monterrey,Mexico' %>% read_html() %>% html_nodes("#HTL_FAVS .name") %>% html_text()

hotel_links <-
  url %>% read_html() %>% html_nodes("#HTL_FAVS .name") %>% html_attr('href')

hotel_ratings <-
  url %>% read_html() %>% html_nodes("#HTL_FAVS .sprite-ratings") %>% html_attr('alt')

df <- data.frame(hotel_names, hotel_ratings, hotel_links)

View(df)
