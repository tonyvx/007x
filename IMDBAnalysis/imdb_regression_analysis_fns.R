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
  return(my_list)
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
  return(my_list)
}
createTrainData <- function(movie_data) {
  return(movie_data %>% filter(movie_age_class == '>2000'))
}
createTestData <- function(movie_data) {
  return(movie_data %>% filter(movie_age_class == '>2010'))
}

dfSubsetForlm <- function(selectedFactors, df) {
  selectedFactors <-
    c(selectedFactors, "gross_in_million", "budget_in_million")
  data_lm = subset(df , select = selectedFactors) %>% na.omit()
  return(data_lm)
}
createLinearModel <- function(data_lm) {
  lmodel <- lm(gross_in_million ~ ., data = data_lm)
  return(lmodel)
}

#IMDB Score vs Gross in million
imdb_gross_plot <- function(movies) {
  p = movies %>%
    mutate(rating = factor(round(imdb_score))) %>%
    na.omit() %>%
    ggplot(aes(x = gross_in_million, y = ..density.., fill = rating)) +
    ggtitle("IMDB rating vs gross_in_million")+
    geom_histogram(binwidth = 5, position = "dodge")
  return(p)
}
#movie likes vs ross in million
movie_like_gross_plot <- function(movies) {
  p = movies %>%
    mutate(`Movie likes in 10000s` = factor(round(movie_facebook_likes /
                                                    10000))) %>%
    na.omit() %>%
    ggplot(aes(x = gross_in_million, y = ..density.., fill = `Movie likes in 10000s`)) +
    
    ggtitle("Movies facebook like vs gross_in_million")+
     geom_histogram(binwidth =
                     5, position = "dodge") + scale_colour_brewer()
  return(p)
}


gross_vs_budget_likes_plot <- function(movies, param_like) {
  columns <- c("gross_in_million", "budget_in_million", param_like)
  print(columns)
  df <-
    movies %>% filter(country == "USA") %>% subset(select = columns)
  names(df)[3] <- "fb_like"
  
  df <- df %>% mutate(fb_like = log10(fb_like))
 
  
  plot <- df %>%
    na.omit() %>%
    ggplot(aes(x = budget_in_million,
               y = gross_in_million,
               col = fb_like)) +
    
    geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) +
    geom_smooth() +
    guides(col = guide_legend(title = param_like)) +
    ggtitle("gross_in_million vs budget_in_million")
  
  return(plot)
}