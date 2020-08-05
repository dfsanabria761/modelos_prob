#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rriskDistributions)
install.packages("readxl")
library(readxl)
library(plotly)
library(ggplot2)
source("Fase1.R")

source("Fase2.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$plantasSembradas <- renderPlot({
            miu = 0.5
            alpha = (1/26)
            
            inFile <- input$file1
            if(!is.null(inFile)){
                #Ingresos (COP/semana) por planta sembrada
                ingresosIn = input$ingresosInv
                ingresosCa = input$ingresosCA
                nsemanas  = 52
                seleccion = as(input$select, "numeric")
                class(seleccion)
                #Costos (COP/semana) por mantenimiento de una hectarea
                costosIn = input$costosInv * 1000000
                costosCa = input$costosCA * 1000000
                if(seleccion %% 2 == 0) {
                    ingresos = ingresosIn
                    costos = costosIn
                } else{
                    ingresos = ingresosCa
                    costos = costosCa
                }
                datos <- read_excel(inFile$datapath, sheet = seleccion+2)
                tPlantaciones = datos$`Semanas entre siembras`
                fitDist <- fit.cont(tPlantaciones)
                lambda <- fitDist$fittedParams
                grass_1(lambda, miu, alpha, nombre = "Plantaciones", ingresos =  ingresosCa, costos = costosCa, t = 52, color = "dark green" )
                
            }
        })
    
    
    
    
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
        valueBox(paste0(veces(), " Días a la semana"), 
                 "Que no se ocupa al máximo el camion", icon = icon("fas fa-cannabis"), color = "olive")
    })
    
    output$tiempo <- renderValueBox({
        valueBox(paste0(tiempoBase(), " Horas que demora"), 
                 "Lote en llegar al cliente final", icon = icon("fas fa-cannabis"), color = "green")
    })
    
    output$tiempoMax <- renderValueBox({
        valueBox(paste0(if(tiempoMaximo()> 100 | tiempoMaximo()<0){"Infinitos"}else {tiempoMaximo()}, " Años laborales"), 
                 "Que tarda el centro de acopio en llegar a máxima capacidad", icon = icon("fas fa-cannabis"), color = "olive")
    })
    
    output$sensibilidadGeneral <- renderPlotly({
        data <- data.frame("Utilidad" = c("Default", "Modificaciones"), "Valor" = c(grass(0.153,6.213,2.556,0.124)["utilidad"], utilidad()))
        ggplot(data = data, aes(x=Utilidad, y = Valor)) +
            geom_bar(stat="identity", color="green", fill="darkgreen")+
            coord_flip()
    })
    
})
