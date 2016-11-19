#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

#Loading dataset from csv
movies <- read.csv("./movie_metadata.csv")

#Add columns for use for plotting

movies <- movies %>% mutate(gross_in_million = gross / 1000000)
movies <- movies %>% mutate(budget_in_million = budget / 1000000)
movies <- movies %>% mutate(movie_age = 2016 - title_year)
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
movies <-
  movies %>% filter(country %in% c("USA", "UK") &
                      movie_age_class %in% c('>2010', '>2000', '>1990'))

#1 Gross by critic review and imdb score
movies %>%  select(gross_in_million,
                   num_critic_for_reviews,
                   movie_age_class,
                   imdb_score,
                   country) %>% na.omit() %>% 
  ggplot(aes(imdb_score, gross_in_million, col = num_critic_for_reviews)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) +
  facet_grid(movie_age_class ~ country)

#2 Gross by user review and imdb score
movies %>%  select(gross_in_million,
                   num_user_for_reviews,
                   imdb_score,
                   movie_age_class ,
                   country) %>% na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, color = num_user_for_reviews)) +
  geom_jitter(alpha = 0.9) +
  facet_grid(movie_age_class ~ country)

#3 Gross by duration and imdb score
movies %>%  select(gross_in_million,
                   duration,
                   imdb_score,
                   movie_age_class ,
                   country) %>% na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, color = duration)) +
  geom_point(alpha = 0.6) +
  facet_grid(movie_age_class ~ country)

#4 Gross by budget and imdb score
movies %>% select(gross_in_million,
                  budget_in_million,
                  imdb_score,
                  movie_age_class ,
                  country) %>% na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, color = budget_in_million)) +
  geom_point(alpha = 0.6) +
  facet_grid(movie_age_class ~ country)

#5 Gross by movie-age and imdb score
movies %>% select(gross_in_million, movie_age_class,title_year, imdb_score,country) %>% na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, col = movie_age_class)) +
  geom_point(alpha = 0.6) +
  facet_grid(movie_age_class ~ country)

#6 Gross by Genres and imdb score
movies  %>% select(gross_in_million,
                   movie_age_class,
                   imdb_score,
                   genres_class,
                   country) %>% na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, col = genres_class)) +
  geom_jitter(alpha = 0.6) +
  scale_color_brewer(palette = "Set1")
  facet_grid(  movie_age_class~country)

#7 Genres_class count by gross_in_million 
movies  %>% select(gross_in_million,
                   movie_age_class,
                   imdb_score,
                   genres_class,
                   country) %>% na.omit() %>%
  ggplot(aes(gross_in_million,col = genres_class)) +
  geom_freqpoly() +
  facet_grid(  movie_age_class~country)

movies %>%
  select(gross_in_million,
         movie_age_class,
         imdb_score,
         genres_class,
         country) %>%
  na.omit() %>%
  ggplot(aes(imdb_score, gross_in_million, col = genres_class)) +
  scale_y_log10() +
  geom_jitter(alpha = 0.9, width = .02) +
  facet_grid(movie_age_class ~ country)

movies %>% select(gross_in_million,
                  movie_age,
                  imdb_score,
                  genres_class) %>% na.omit() %>%  ggpairs()