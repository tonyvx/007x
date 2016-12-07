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
library(dplyr)
source("imdb_regression_analysis_fns.R")

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
      sliderInput(
        "gross",
        "gross in million:",
        min = 0,
        max = 1000,
        value = 500
      ),
      #[5] "director_facebook_likes"
      #[6] "actor_3_facebook_likes"
      #[8] "actor_1_facebook_likes"
      #[14] "cast_total_facebook_likes"
      #[25] "actor_2_facebook_likes"
      #[28] "movie_facebook_likes"
      #[29] "gross_in_million"
      #[30] "budget_in_million"
      selectInput(
        "facebookLikesInput",
        "Facebook popularity of cast",
        choices = c(
          "Likes for the director" = 5,
          "Likes for the main actor" = 8,
          "Likes for the 1st supporting actor" = 25,
          "Likes for the 2nd supporting actor" = 6,
          "Likes for all cast" = 14
        ),
        selected = 5,
        multiple = TRUE
      )
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("gross by imdb score", plotOutput("gross_by_imdb_score")),
      tabPanel("gross by movie likes", plotOutput("gross_by_movie_like")),
      tabPanel("Training (title_year > 2000)", verbatimTextOutput("train")),
      tabPanel("Testing (title_year > 2010)", verbatimTextOutput("test"))
    ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Gross by critic review and imdb score
  movies = loadDF()
  
  output$gross_by_imdb_score <- renderPlot({
    print(imdb_gross_plot(movies %>% 
                            filter( country == input$countryInput , gross_in_million > input$gross
      )
    ))
  })
  output$gross_by_movie_like <- renderPlot({
    print(movie_like_gross_plot(movies %>% 
                                  filter( country == input$countryInput ,  gross_in_million > input$gross
      )
    ))
  })
  output$train <- renderPrint({
    train <- createTrainData(movies %>% filter(country == input$countryInput))
    lmodel <- createLinearModel(input$facebookLikesInput, train)
    trainAnalysis <- analysis(lmodel, train)
    print(trainAnalysis)
  })
  
  output$test <- renderPrint({
    train <- createTrainData(movies %>% filter(country == input$countryInput))
    lmodel <- createLinearModel(input$facebookLikesInput, train)
    trainAnalysis <- analysis(lmodel, train)
    test <- createTestData(movies %>% filter(country == input$countryInput))
    
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
