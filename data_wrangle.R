library("dplyr")
library("tidyr")
df <-
  read.csv("./refine.csv",
           header = TRUE,
           stringsAsFactors = FALSE)


refined_data = df %>%
  mutate(company = if_else(grepl('^[vV]', company), 'van houten', company)) %>%
  mutate(company = if_else(grepl('^[aA]', company), 'akzo', company)) %>%
  mutate(company = if_else(grepl('^[fpP]', company),  'philips', company)) %>%
  mutate(company = if_else(grepl('^[uU]', company), 'unilever', company)) %>%
  separate(Product.code...number,
           # split into code & number using - as seprator
           into = c("code", "number"),
           sep = '-') %>%
  mutate(code = if_else(code == 'p', 'Smartphone', code)) %>% # use mutate and if_else change code into categories
  mutate(code = if_else(code == 'v', 'TV', code)) %>%
  mutate(code = if_else(code == 'x', 'Laptop', code)) %>%
  mutate(code = if_else(code == 'q', 'Tablet', code)) %>%
  mutate(full_address = paste(address, ",", city, ",", country))

refined_data %>% select(company, name, code, number, address, city, country, full_address) %>% arrange(company) %>% View()
