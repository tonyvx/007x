library(httr)
library(xml2)
library(rvest)


readDestination <- function()
{
  n <- readline(prompt="Destination - ")
  print
  if (n == ""){
    n <- readDestination()
  }
  return(n)
}

destination <-readDestination()
#destination = "Kochi,+India"

search_url1 = paste0("https://www.tripadvisor.com/Search?geo=1&redirect&q=",
                     destination)

travel_guide_url <-
  search_url1 %>% read_html() %>% html_nodes(".sublink:nth-child(6) a") %>% html_attr('href') %>% head(1)

title_names <-
  paste0("https://www.tripadvisor.com", travel_guide_url) %>%
  read_html() %>%
  html_nodes("#FILTERED_LIST .al_border") %>%
  html_nodes(".property_title a") %>%
  html_text()

title_links <-
  paste0("https://www.tripadvisor.com", travel_guide_url) %>%
  read_html() %>%
  html_nodes("#FILTERED_LIST .al_border") %>%
  html_nodes(".property_title a") %>%
  html_attr('href')

title_ratings <-
  paste0("https://www.tripadvisor.com", travel_guide_url) %>%
  read_html()  %>%
  html_nodes(".sprite-ratings") %>%
  html_attr('alt')

df <- data.frame(title_names,
                 #title_ratings,
                 title_links)

View(df)
