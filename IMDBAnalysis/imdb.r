#libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#Loading dataset from csv
movies <- read.csv("./movie_metadata.csv")

#Add columns for use for plotting
movies <- movies %>% mutate(`Action` = grepl("Action", genres))
movies <-
  movies %>% mutate(`Adventure` = grepl("Adventure", genres))
movies <- movies %>% mutate(`Fantasy` = grepl("Fantasy", genres))
movies <- movies %>% mutate(`Sci-Fi` = grepl("Sci-Fi", genres))
movies <- movies %>% mutate(`Thriller` = grepl("Thriller", genres))
movies <-
  movies %>% mutate(`Documentary` = grepl("Documentary", genres))
movies <- movies %>% mutate(`Romance` = grepl("Romance", genres))
movies <-
  movies %>% mutate(`Animation` = grepl("Animation", genres))
movies <- movies %>% mutate(`Comedy` = grepl("Comedy", genres))
movies <- movies %>% mutate(`Family` = grepl("Family", genres))
movies <- movies %>% mutate(`Musical` = grepl("Musical", genres))
movies <- movies %>% mutate(`Mystery` = grepl("Mystery", genres))
movies <- movies %>% mutate(`Western` = grepl("Western", genres))
movies <- movies %>% mutate(`Drama` = grepl("Drama", genres))
movies <- movies %>% mutate(`History` = grepl("History", genres))
movies <- movies %>% mutate(`Sport` = grepl("Sport", genres))
movies <- movies %>% mutate(`Crime` = grepl("Crime", genres))
movies <- movies %>% mutate(`Horror` = grepl("Horror", genres))
movies <- movies %>% mutate(`War` = grepl("War", genres))
movies <-
  movies %>% mutate(`Biography` = grepl("Biography", genres))
movies <- movies %>% mutate(`Music` = grepl("Music", genres))
movies <-
  movies %>% mutate(`Game-Show` = grepl("Game-Show", genres))
movies <-
  movies %>% mutate(`Reality-TV` = grepl("Reality-TV", genres))
movies <- movies %>% mutate(`News` = grepl("News", genres))
movies <- movies %>% mutate(`Short` = grepl("Short", genres))
movies <-
  movies %>% mutate(`Film-Noir` = grepl("Film-Noir`", genres))

movies <- movies %>% mutate(gross_in_million = gross / 1000000)
movies <- movies %>% mutate(budget_in_million = budget / 1000000)
movies <- movies %>% mutate(movie_age = 2016 - title_year)
movies <- movies %>%
  mutate(movie_age_class = if_else(movie_age < 3, "New",
                                   if_else(
                                     movie_age < 10, "2000s",
                                     if_else(movie_age < 20, "1990s", "old")
                                   )))

movies <- movies %>%
  mutate(
    genres_class = case_when(
      grepl("Action", movies$genres) ~ "Action",
      grepl("Adventure", movies$genres) ~ "Action",
      grepl("Thriller", movies$genres) ~ "Thriller",
      grepl("Western", movies$genres) ~ "Thriller",
      grepl("Romance", movies$genres) ~ "Family",
      grepl("Sci-Fi", movies$genres) ~ "Family",
      grepl("Drama", movies$genres) ~ "Family",
      grepl("Fantasy", movies$genres) ~ "Family",
      grepl("Animation", movies$genres) ~ "Family",
      grepl("Comedy", movies$genres) ~ "Comedy",
      grepl("Crime", movies$genres) ~ "Thriller",
      grepl("Romance", movies$genres) ~ "Romance",
      grepl("Family", movies$genres) ~ "Family",
      grepl("Fantasy", movies$genres) ~ "Fanatasy",
      grepl("Documentary", movies$genres) ~ "Documentary",
      grepl("History", movies$genres) ~ "Documentary",
      grepl("Animarion", movies$genres) ~ "Family",
      grepl("Horror", movies$genres) ~ "Horror",
      grepl("Music", movies$genres) ~ "Musical",
      TRUE ~ as.character(movies$genres)
    )
  )

#1 Gross by critic review and imdb score
movies %>%  select(gross_in_million, num_critic_for_reviews, imdb_score) %>%
  ggplot(aes(imdb_score, gross_in_million, color = num_critic_for_reviews)) +
  geom_point(alpha = 0.6)

#2 Gross by user review and imdb score
movies %>%  select(gross_in_million, num_user_for_reviews, imdb_score) %>%
  ggplot(aes(imdb_score, gross_in_million, color = num_user_for_reviews)) +
  geom_point(alpha = 0.6)

#3 Gross by duration and imdb score
movies %>%  select(gross_in_million, duration, imdb_score) %>%
  ggplot(aes(imdb_score, gross_in_million, color = duration)) +
  geom_point(alpha = 0.6)

#4 Gross by budget and imdb score
movies %>%  select(gross_in_million, budget_in_million, imdb_score) %>%
  ggplot(aes(imdb_score, gross_in_million, color = budget_in_million)) +
  geom_point(alpha = 0.6)

#5 Gross by movie-age and imdb score
movies %>% filter(movie_age < 10 &
                    movie_age > 5) %>% select(gross_in_million, movie_age, imdb_score) %>%
  ggplot(aes(imdb_score, gross_in_million, col = movie_age)) +
  geom_point(alpha = 0.6)

#6 Gross by Genres(Action vs Romance) and imdb score
movies %>% select(gross_in_million,
                  movie_age,
                  imdb_score,
                  genres_class) %>%
  ggplot(aes(imdb_score, gross_in_million, col = genres_class)) + geom_point(alpha =
                                                                               0.6)
