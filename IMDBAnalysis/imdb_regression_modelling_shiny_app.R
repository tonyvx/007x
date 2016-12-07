#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(
    "IMDB Analysis - Predict Gross based on budget and Facebook popularity of the cast"
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "countryInput",
        "Country",
        choices = c("USA", "UK", "Canada", "Germany"),
        selected = "USA"
      ),
      selectInput(
        "facebookLikesInput",
        "Facebook popularity of cast",
        choices = c(
          "Facebook likes for the director" = 5,
          "Facebook likes for the main actor" = 8,
          "Facebook likes for the 1st supporting actor" = 25,
          "Facebook likes for the 2nd supporting actor" = 6
        ),
        selected = 5,
        multiple = TRUE
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("gross by imdb score", plotOutput("gross_by_imdb_score"))
      ,
      tabPanel("gross by movie likes", plotOutput("gross_by_movie_like"))
      ,
      tabPanel("Training (title_year > 2000)", verbatimTextOutput("train"))
      ,
      tabPanel("Testing (title_year > 2010)", verbatimTextOutput("test"))
    ))
  )
)
#####funtions######
analysis <- function(lmodel, data) {
  sse = sum(lmodel$residuals ^ 2, na.rm = TRUE)
  rmse = sqrt(sse / nrow(data))
  summary_data = summary(lmodel)
  my_list <- list(
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
  
  my_list <- list(
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
  selectedFactors <- c(selectedFactors, 29, 30)
  colIndex = c()
  for (i in selectedFactors)
    colIndex <- c(colIndex, as.integer(i))
  lmodel <-
    lm(gross_in_million ~ ., data = df %>% select(colIndex))
  return(lmodel)
}
#Loading dataset from csv
loadDF <- function() {
  movies <- read.csv("./movie_metadata.csv")
  
  #Add columns for use for plotting
  
  movies <- movies %>% mutate(gross_in_million = gross / 1000000)
  movies <-
    movies %>% mutate(budget_in_million = budget / 1000000)
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
  return (movies)
}


#IMDB Score vs Gross in million
imdb_gross_plot <- function(movies) {
  p = movies %>% mutate(rating = factor(round(imdb_score))) %>% ggplot(aes(x =
                                                                             gross_in_million, fill = rating)) + geom_histogram(binwidth = 10)
  return(p)
}
#movie likes vs ross in million
movie_like_gross_plot <- function(movies) {
  p = movies %>% mutate(`Movie likes in 10000s` = factor(round(movie_facebook_likes /
                                                                 10000))) %>% ggplot(aes(x = gross_in_million, fill = `Movie likes in 10000s`)) + geom_histogram(binwidth =
                                                                                                                                                                   10)
  return(p)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Gross by critic review and imdb score
  movies = loadDF()
  
  output$gross_by_imdb_score <- renderPlot({
    print(imdb_gross_plot(movies %>% filter(country == input$countryInput)))
  })
  output$gross_by_movie_like <- renderPlot({
    print(movie_like_gross_plot(movies %>% filter(country == input$countryInput)))
  })
  output$train <- renderPrint({
    train <-
      createTrainData(movies %>% filter(country == input$countryInput))
    lmodel <- createLinearModel(input$facebookLikesInput, train)
    trainAnalysis <- analysis(lmodel, train)
    print(trainAnalysis)
  })
  
  output$test <- renderPrint({
    train <-
      createTrainData(movies %>% filter(country == input$countryInput))
    lmodel <- createLinearModel(input$facebookLikesInput, train)
    trainAnalysis <- analysis(lmodel, train)
    test <-
      createTestData(movies %>% filter(country == input$countryInput))
    
    predictTest = predict(lmodel, newdata = test)
    testAnalysis <- analysisPredict(predictTest,
                                    train$gross_in_million,
                                    test$gross_in_million,
                                    test)
    print(testAnalysis)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
