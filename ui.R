#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",

    titlePanel("Grass"),
    textOutput("veces"),
    textOutput("tiempo"),
    
    plotOutput('plotter'),
    
    hr(),   
    
    fluidRow(
        column(4,
               numericInput("tiempo1", "Tiempo RT:", 0.153, min = 0.1, max = 1, step = 0.01)
        ),
        
        column(4,
               numericInput("tiempoSAL", "Tiempo SAL:", 6.213, min = 0.1, max = 10, step = 0.1)
        ),
        column(4,
                 numericInput("tiempoSO", "Tiempo SO:", 2.556, min = 0.1, max = 5, step = 0.01)
        ),
        column(4,
                 numericInput("tiempoAS", "Tiempo AS:", 0.124, min = 0.1, max = 1, step = 0.01)
        )
)))
 
