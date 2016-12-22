
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("hr_efficiency.R")

shinyServer(function(input, output) {
  hr <- hrDF()
  
  output$distPlot <- renderPlot({
    print(getSelectedPlot(input$plot, hr))
  })
  
  output$data_descr <- renderPrint({
    print(dataDescription(hr))
  })
  
  output$descr <- renderPrint({
    print(description)
  })
  
})
