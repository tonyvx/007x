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
  
  
  tabsetPanel(
    tabPanel(
      "Plot",
      fluidRow(column(
        6,
        sliderInput(
          inputId = "gross",
          label = "gross in million:",
          min = 0,
          max = 1000,
          value = c(0, 1000),
          dragRange = TRUE
        )
      ),
      column(
        6,
        selectInput(
          "countryInput",
          "Country",
          choices = c("USA", "UK", "Canada", "Germany"),
          selected = "USA"
        )
      )),
      fluidRow(
        column(6, verbatimTextOutput("gross_by_imdb_score_msg")),
        column(6, verbatimTextOutput("gross_by_movie_like_msg"))
      ),
      fluidRow(column(6, plotOutput(
        "gross_by_imdb_score"
      )),
      column(6, plotOutput(
        "gross_by_movie_like"
      ))),
      fluidRow(column(
        12,
        selectInput(
          "facebookLikesPlotInput",
          "Facebook popularity of cast",
          choices = c(
            "Likes for the director" = "director_facebook_likes",
            "Likes for the main actor" = "actor_1_facebook_likes",
            "Likes for the 1st supporting actor" = "actor_2_facebook_likes",
            "Likes for the 2nd supporting actor" = "actor_3_facebook_likes",
            "Likes for all cast" = "cast_total_facebook_likes"
          ),
          selected = "director_facebook_likes",
          multiple = FALSE
        )
      ),
      column(
        12, plotOutput("gross_vs_budget_likes_plot1")
      ))
    ),
    
    tabPanel(
      "Linear Regression Analysis",
      fluidRow(
        column(
          12,
          selectInput(
            "facebookLikesInput",
            "Facebook popularity of cast",
            choices = c(
              "Likes for the director" = "director_facebook_likes",
              "Likes for the main actor" = "actor_1_facebook_likes",
              "Likes for the 1st supporting actor" = "actor_2_facebook_likes",
              "Likes for the 2nd supporting actor" = "actor_3_facebook_likes",
              "Likes for all cast" = "cast_total_facebook_likes"
            ),
            selected = "director_facebook_likes",
            multiple = TRUE
          )
        ),
        column(6,
               verbatimTextOutput("train")),
        column(6,
               verbatimTextOutput("test"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Gross by critic review and imdb score
  movies = loadDF()
  
  output$gross_by_imdb_score <- renderPlot({
    print(imdb_gross_plot(
      movies %>%
        filter(
          country == input$countryInput ,
          gross_in_million > input$gross[1],
          gross_in_million < input$gross[2]
        )
    ))
  })
  output$gross_vs_budget_likes_plot1 <- renderPlot({
    print(input$countryInput)
    print(gross_vs_budget_likes_plot(
      movies %>%
        filter(country == input$countryInput),
      input$facebookLikesPlotInput
    ))
  })
  output$gross_by_movie_like <- renderPlot({
    print(movie_like_gross_plot(
      movies %>%
        filter(
          country == input$countryInput ,
          gross_in_million > input$gross[1],
          gross_in_million < input$gross[2]
        )
    ))
  })
  output$train <- renderPrint({
    train <-
      createTrainData(movies %>% filter(country == input$countryInput))
    data_lm = dfSubsetForlm(input$facebookLikesInput, train)
    print("Training (title_year > 2000)")
    print("_____________________________________")
    print(summary(data_lm))
    print("_____________________________________")
    print("Correlation of data used for linear model")
    print("_________________________________________")
    print(cor(data_lm))
    
    lmodel <- createLinearModel(data_lm)
    
    print("Linear model")
    print("_________________________________________")
    print(lmodel)
    trainAnalysis <- analysis(lmodel, train)
    print("_____________________________________")
    print("Linear model = Analysis")
    print("_________________________________________")
    print(trainAnalysis)
    print("_____________________________________")
  })
  output$gross_by_imdb_score_msg <- renderPrint({
    print("gross_by_imdb_score_msg")
  })
  
  output$gross_by_movie_like_msg <- renderPrint({
    print("gross_by_movie_like_msg")
  })
  output$test <- renderPrint({
    train <-
      createTrainData(movies %>% filter(country == input$countryInput))
    data_lm = dfSubsetForlm(input$facebookLikesInput, train)
    lmodel <- createLinearModel(data_lm)
   #trainAnalysis <- analysis(lmodel, train)
    test <-
      createTestData(movies %>% filter(country == input$countryInput))
    print(summary(dfSubsetForlm(input$facebookLikesInput, test)))
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
