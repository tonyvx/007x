






# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("hr_efficiency.R")

shinyUI(fluidPage(
  # Application title
  titlePanel(
    "Why are our best and most experienced employees leaving prematurely? "
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(fluidRow(column(
      width = 12,
      selectInput("plot", "Plot:",
                  getPlotTypes())
    )),
    fluidRow(column(
      width = 12,
      verbatimTextOutput("descr")
    ))),
    
    # Show a plot of the generated distribution
    mainPanel(fluidRow(column(
      width = 12, plotOutput("distPlot")
    )),
    fluidRow(column(
      width = 12,
      verbatimTextOutput("data_descr")
    )))
  )
))
