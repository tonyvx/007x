library("ggplot2")
library("dplyr")
library("RColorBrewer")

plot_data <-
  df %>% group_by(country, title_year) %>% summarize(
    imdb_score = round(mean(imdb_score), 2),
    a_facebook_likes = round(quantile(na.rm = TRUE, actor_1_facebook_likes, c(.90))),
    m_facebook_likes = round(quantile(na.rm = TRUE, movie_facebook_likes, c(.90))),
    d_facebook_likes = round(quantile(na.rm = TRUE, director_facebook_likes, c(.90))),
    
    movie_cnt = n()
  ) %>% arrange(desc(title_year), desc(imdb_score)) %>%
  filter(
    title_year >= params$min_year ,
    title_year <= params$max_year ,
    imdb_score >= params$imdb_score,
    movie_cnt > params$movie_count
  )

plot_data %>% ggplot(aes(x = title_year,
                         y = imdb_score,
                         col = movie_cnt)) +
  geom_point() +
  geom_smooth(method = "loess",
              alpha = 0.6,
              size = 1) +
  facet_grid(. ~ country)

plot_data %>% ggplot(aes(x = title_year,
                         y = imdb_score,
                         col = m_facebook_likes)) +
  geom_point() +
  geom_smooth(method = "loess",
              alpha = 0.6,
              size = 1)  +
  labs(list(title = "IMDB Score by Facebook Likes", x = "Facebook Likes", y = "IMDB Score")) +
  theme(plot.background = element_rect(fill = "#CFDBD3", color = "black", size =
                                         3)) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    strip.background = element_blank()
  ) +
  
  facet_grid(country ~ .)
