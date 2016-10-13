library("dplyr")
df <- read.csv("./titanic_original.csv",
               header = TRUE,
               stringsAsFactors = FALSE)
#1Fix Port of embarkation - missing cells set to S
refined <-
  df %>% mutate(embarked = if_else(embarked == '', 'S', embarked)) %>%
  #2Fix Age - using the mean or median of the rest of the values
  mutate(age  = if_else(is.na(age)  , median(age, na.rm = TRUE), age)) %>%
  #3Fix boat fill it with NA if blank
  mutate(boat = if_else(boat == '', 'NA', boat)) %>%
  #4Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
  mutate(has_cabin_number = if_else(cabin == '', 0, 1)) 

write.csv(refined, './titanic_clean.csv')

refined %>% View()




