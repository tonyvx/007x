library("dplyr")
library("tidyr")
df <-read.csv("./refine_original.csv",
           header = TRUE,
           stringsAsFactors = FALSE)


refined_data = df %>% #Fix company Name
  mutate(company = if_else(grepl('^[vV]', company), 'van houten', company)) %>%
  mutate(company = if_else(grepl('^[aA]', company), 'akzo', company)) %>%
  mutate(company = if_else(grepl('^[fpP]', company),  'philips', company)) %>%
  mutate(company = if_else(grepl('^[uU]', company), 'unilever', company)) %>%
  # split into code & number using - as seprator
  separate(Product.code...number,
           into = c("code", "number"),
           sep = '-') %>%
  # replace code p,v,x,q with Smartphone, TV,Laptop, Tablet
  mutate(code = if_else(code == 'p', 'Smartphone', code)) %>% # use mutate and if_else change code into categories
  mutate(code = if_else(code == 'v', 'TV', code)) %>%
  mutate(code = if_else(code == 'x', 'Laptop', code)) %>%
  mutate(code = if_else(code == 'q', 'Tablet', code)) %>%
  # Add a new column full address using address, city, country
  mutate(full_address = paste0(address, ",", city, ",", country)) %>%
  #Add columns ompany_philips, company_akzo, company_van_houten and company_unilever
  mutate(company_philips=if_else(company=='philips',1,0)) %>%
  mutate(company_akzo=if_else(company=='akzo',1,0)) %>%
  mutate(company_van_houten=if_else(company=='van houten',1,0)) %>%
  mutate(company_unilever=if_else(company=='unilever',1,0)) %>%
  #product category: product_smartphone, product_tv, product_laptop and product_tablet
  mutate(product_smartphone=if_else(code=='Smartphone',1,0)) %>%
  mutate(product_tv=if_else(code=='TV',1,0)) %>%
  mutate(product_laptop=if_else(code=='Laptop',1,0)) %>%
  mutate(product_tablet=if_else(code=='Tablet',1,0)) 

refined_data %>% arrange(company,code) %>% write.csv("./refine_clean.csv")
