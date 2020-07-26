#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("Fase2.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    res <-grass(tiemposRT = input$tiempo1,tiempoSAL =  input$tiempoSAL,tiempoSO =  input$tiempoSO,tiempoAS =  input$tiempoAS)
    print(res[1])
    output$veces <- renderText({
        res[0]
    })
    
    output$tiempo <- renderText({
        res[1]
    })
    
})

 
