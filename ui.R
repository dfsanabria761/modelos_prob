#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
install.packages("plotly")
library(plotly)
library(ggplot2)
# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Analisis Grass"),
    dashboardSidebar(
        numericInput("tiempo1", "Tiempo RT:", 0.153, min = 0.1, max = 10, step = 0.01),
        numericInput("tiempoSAL", "Tiempo SAL:", 6.213, min = 0.1, max = 10, step = 0.01),
        numericInput("tiempoSO", "Tiempo SO:", 2.556, min = 0.1, max = 10, step = 0.01),
        numericInput("tiempoAS", "Tiempo AS:", 0.124, min = 0.1, max = 10, step = 0.01),
        tags$p("Por favor ingrese la informacion con la mayor precision posible, esto es importante para mostrar graficas y contenido adecuados. Nos disculpamos por la ausencia de tildes en el aplicativo, es estrictamente por shiny")
        
    ),
    
    dashboardBody(
            fluidRow(
                valueBoxOutput("veces"),
                valueBoxOutput("tiempo"),
                valueBoxOutput("tiempoMax")
                
            ),
            fluidRow(
                box(title = "Comparativa RT", solidHeader = T,
                    width = 3, collapsible = T,
                    plotlyOutput("sensibilidadRT")),
                box(title = "Comparativa SAL", solidHeader = T,
                    width = 3, collapsible = T,
                    plotlyOutput("sensibilidadSAL")),
                box(title = "Comparativa SO", solidHeader = T,
                    width = 3, collapsible = T,
                    plotlyOutput("sensibilidadSO")),
                box(title = "Comparativa AS", solidHeader = T,
                    width = 3, collapsible = T,
                    plotlyOutput("sensibilidadAS"))
            ),
            
            fluidRow(
                box(title = "Comparativa General", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("sensibilidadGeneral"))
            )
    )
)
 
