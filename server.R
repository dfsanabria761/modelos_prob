#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

source("Fase2.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    veces <- reactive({
        grass(tiemposRT = input$tiempo1,tiempoSAL =  input$tiempoSAL,tiempoSO =  input$tiempoSO,tiempoAS =  input$tiempoAS)["veces"]
    })
    
    tiempoBase <- reactive({
        grass(tiemposRT = input$tiempo1,tiempoSAL =  input$tiempoSAL,tiempoSO =  input$tiempoSO,tiempoAS =  input$tiempoAS)["tiempoBase"]
    })
    
    tiempoMaximo <- reactive({
        grass(tiemposRT = input$tiempo1,tiempoSAL =  input$tiempoSAL,tiempoSO =  input$tiempoSO,tiempoAS =  input$tiempoAS)["tiempoMaximo"]
    })
    
    utilidad <- reactive({
        grass(tiemposRT = input$tiempo1,tiempoSAL =  input$tiempoSAL,tiempoSO =  input$tiempoSO,tiempoAS =  input$tiempoAS)["utilidad"]
    })
    
    output$veces <- renderValueBox({
        valueBox(paste0(veces(), " Dias a la semana"), 
                 "Que no se ocupa al maximo el camion", icon = icon("fas fa-cannabis"), color = "olive")
    })
    
    output$tiempo <- renderValueBox({
        valueBox(paste0(tiempoBase(), " Horas que demora"), 
                 "Lote en llegar al cliente final", icon = icon("fas fa-cannabis"), color = "green")
    })
    
    output$tiempoMax <- renderValueBox({
        valueBox(paste0(if(tiempoMaximo()> 100 | tiempoMaximo()<0){"Infinitos"}else {tiempoMaximo()}, " Anios laborales"), 
                 "Que tarda el centro de acopio en llegar a maxima capacidad", icon = icon("fas fa-cannabis"), color = "olive")
    })
    
    output$sensibilidadRT <- renderPlotly({
        data <- data.frame("Utilidad" = c("Original", "Alterada"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], grass(input$tiempo1,6.213,2.556,0.124)["utilidad"]))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="lightgreen")
    })
    output$sensibilidadSAL <- renderPlotly({
        data <- data.frame("Utilidad" = c("Original", "Alterada"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], grass(0.153,input$tiempoSAL,2.556,0.124)["utilidad"]))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="darkgreen")
    })
    output$sensibilidadSO <- renderPlotly({
        data <- data.frame("Utilidad" = c("Original", "Alterada"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], grass(0.153,6.213,input$tiempoSO,0.124)["utilidad"]))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="lightgreen")
    })
    output$sensibilidadAS <- renderPlotly({
        data <- data.frame("Utilidad" = c("Original", "Alterada"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], grass(0.153,6.213,2.556,input$tiempoAS)["utilidad"]))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="darkgreen")
    })
    
    output$sensibilidadGeneral <- renderPlotly({
        data <- data.frame("Utilidad" = c("Default", "Modificaciones"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], utilidad()))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="darkgreen")+
            coord_flip()
    })
    
})
