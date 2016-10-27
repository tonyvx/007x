library(httr)
library(xml2)
library(rvest)


readDestination <- function()
{
  n <- readline(prompt = "Destination - ")
  print
  if (n == "") {
    n <- readDestination()
  }
  return(n)
}

fetch <- function(x) {

  for (i in x) {
    print(xml_find_one(i,'.//div[@class=class="photo_booking non_generic"]'))
   # name <- i %>% xml_find_one( 'property_title') %>% xml_text()
    #print(name)
  }
  # name <- x %>% html_nodes(".property_title a") %>% html_text()
  # href <-
  #   x %>% html_nodes(".property_title a") %>% html_attr('href')
  # rating <- x %>% html_nodes(".sprite-ratings") %>% html_attr('alt')
  # #print(name, href, rating)
}

destination <- readDestination()

search_url1 = paste0("https://www.tripadvisor.com/Search?geo=1&redirect&q=",
                     destination)

travel_guide_url <-
  search_url1 %>% read_html() %>% html_nodes(".sublink:nth-child(6) a") %>% html_attr('href') %>% head(1)

data1 <- paste0("https://www.tripadvisor.com", travel_guide_url) %>%
  read_html() %>%
  html_nodes("#FILTERED_LIST .al_border")  %>% fetch()

