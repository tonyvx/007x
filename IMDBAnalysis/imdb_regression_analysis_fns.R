library(ggplot2)
library(dplyr)
library(tidyr)

#####funtions######

#Loading dataset from csv
loadDF <- function() {
  movies <- read.csv("./movie_metadata.csv")
  
  #Add columns for use for plotting
  
  movies <- movies %>%
    mutate(gross_in_million = gross / 1000000)
  
  movies <- movies %>%
    mutate(budget_in_million = budget / 1000000)
  
  movies <- movies %>%
    mutate(movie_age = 2016 - title_year)
  
  movies <- movies %>%
    mutate(
      movie_age_class = case_when(
        movies$title_year > 2010 ~ ">2010",
        movies$title_year > 2000 ~ ">2000",
        movies$title_year > 1990 ~ ">1990",
        movies$title_year > 1980 ~ ">1980",
        movies$title_year > 1970 ~ ">1970",
        TRUE ~ "Old"
      )
    )
  
  movies <- movies %>%
    mutate(
      genres_class = case_when(
        grepl("Horror", movies$genres) ~ "Horror",
        grepl("Action", movies$genres) ~ "Action",
        grepl("Adventure", movies$genres) ~ "Action",
        grepl("Thriller", movies$genres) ~ "Thriller",
        grepl("Western", movies$genres) ~ "Thriller",
        grepl("Crime", movies$genres) ~ "Thriller",
        grepl("Romance", movies$genres) ~ "Family",
        grepl("Sci-Fi", movies$genres) ~ "Family",
        grepl("Drama", movies$genres) ~ "Family",
        grepl("Fantasy", movies$genres) ~ "Family",
        grepl("Animation", movies$genres) ~ "Family",
        grepl("Family", movies$genres) ~ "Family",
        grepl("Comedy", movies$genres) ~ "Comedy",
        grepl("Documentary", movies$genres) ~ "Other",
        grepl("History", movies$genres) ~ "Other",
        grepl("Music", movies$genres) ~ "Other",
        TRUE ~ as.character(movies$genres)
      )
    )
  
  return(movies)
}

convertToIntVector <- function(selectedFactors) {
  colIndex = c()
  for (i in selectedFactors)
    colIndex <- c(colIndex, as.integer(i))
  return(colIndex)
}

## Regression Analysis 
analysis <- function(lmodel, data) {
  sse = sum(lmodel$residuals ^ 2, na.rm = TRUE)
  rmse = sqrt(sse / nrow(data))
  summary_data = summary(lmodel)
  my_list <- c(
    "R-Squared" = summary_data$r.squared,
    "Adjusted R-Squared" = summary_data$adj.r.squared,
    "Sum of squared Errors" = sum(lmodel$residuals ^ 2, na.rm = TRUE) ,
    "Root Mean Squared error" = rmse
  )
  my_list
}
analysisPredict <- function(predicted, train, test, test_data) {
  sse = sum((predicted - test) ^ 2, na.rm = TRUE)
  sst = sum((mean(train, na.rm = TRUE) - test) ^ 2, na.rm = TRUE)
  R2 = 1 - sse / sst
  rmse = sqrt(sse / nrow(test_data))
  
  my_list <- c(
    "R-Squared" = R2,
    "Adjusted R-Squared" = "NA",
    "Sum of squared Errors" = sse ,
    "Root Mean Squared error" = rmse
  )
  my_list
}
createTrainData <- function(movie_data) {
  return(movie_data %>% filter(movie_age_class == '>2000'))
}
createTestData <- function(movie_data) {
  return(movie_data %>% filter(movie_age_class == '>2010'))
}

createLinearModel <- function(selectedFactors, df) {
  #[5] "director_facebook_likes"
  #[6] "actor_3_facebook_likes"
  #[8] "actor_1_facebook_likes"
  #[14] "cast_total_facebook_likes"
  #[25] "actor_2_facebook_likes"
  #[28] "movie_facebook_likes"
  #[29] "gross_in_million"
  #[30] "budget_in_million"
  selectedFactors <- c(convertToIntVector(selectedFactors), 29, 30)
  lmodel <- lm(gross_in_million ~ ., data = df %>% select(selectedFactors))
  return(lmodel)
}

#IMDB Score vs Gross in million
imdb_gross_plot <- function(movies) {
  p = movies %>%
    mutate(rating = factor(round(imdb_score))) %>%
    na.omit() %>%
    ggplot(aes(x = gross_in_million, y = ..density.., fill = rating)) +
    geom_histogram(binwidth = 5)
  return(p)
}
#movie likes vs ross in million
movie_like_gross_plot <- function(movies) {
  p = movies %>%
    mutate(`Movie likes in 10000s` = factor(round(movie_facebook_likes /
                                                    10000))) %>%
    na.omit() %>%
    ggplot(aes(x = gross_in_million, y = ..density.., fill = `Movie likes in 10000s`)) +
    geom_histogram(binwidth =
                     5)
  return(p)
}