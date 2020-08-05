#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
#install.packages("shinyWidgets")
#install.packages("plotly")
library("shinyWidgets")
library(plotly)
library(ggplot2)
# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Grass"),
    dashboardSidebar(
        sidebarMenu(
          menuItem("Zona, Siembra y Utilidad", tabName = "siembra"),
          menuItem("Procesamiento", tabName = "procesamiento"),
          menuItem("Política Óptima", tabName = "politicaOptima")
        ),
        tags$p("Por favor ingrese la información con la mayor precisión posible, esto es importante para mostrar gráficas y contenido adecuados")
    ),
    
    dashboardBody(
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #141204;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #141204;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #262A10;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #141204;
        }
        .skin-blue .main-body {
                              background-color: #141204;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #C9E3AC;
                              }
                              '
                                  ))),
        tabItems(
            tabItem("siembra",
                column(6,
                       fileInput("file1", "Ingrese el archivo en CSV",
                                 multiple = FALSE,
                                 accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                            ".xlsx")),
                       fluidRow(
                           column(6,
                                  knobInput(
                                      inputId = "ingresosCA",
                                      label = "Ingresos por Campo Abierto:",
                                      value = 6875,
                                      min = 0,
                                      max = 10000,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                            ),
                           column(6,
                                  knobInput(
                                      inputId = "ingresosInv",
                                      label = "Ingresos por Invernadero:",
                                      value = 32.08,
                                      min = 0,
                                      max = 100,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           )
                       ),
                       fluidRow(
                           column(6,
                                  knobInput(
                                      inputId = "costosCA",
                                      label = "Costos por Campo Abierto:",
                                      value = 25.53,
                                      min = 0,
                                      max = 100,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           ),
                           column(6,
                                  knobInput(
                                      inputId = "costosInv",
                                      label = "Costos por Invernadero:",
                                      value = 1400,
                                      min = 0,
                                      max = 10000,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           )
                       )
                ),
                column(6,
                       fluidRow(
                           selectInput("select", h3("Seleccione Zona"), 
                                       choices = list("Cundinamarca C.A." = 1, 
                                                      "Cundinamarca Inv." = 2,
                                                      "Antioquia C.A." = 3,
                                                      "Antioquia Inv." = 4, 
                                                      "Valle del Cauca C.A." = 5,
                                                      "Valle del Cauca Inv." = 6,
                                                      "Cauca C.A." = 7, 
                                                      "Cauca Inv." = 8,
                                                      "Tolima C.A." = 9,
                                                      "Tolima Inv." = 10), selected = 1),
                           switchInput(inputId = "switch", value = TRUE, onLabel = "Plantas Sembradas", offLabel = "Utilidad")
                       ),
                       plotOutput("plantasSembradas"),
                       plotOutput("utilidadPlantas"),
                )
            ),
            tabItem("procesamiento",
                fluidRow(
                    valueBoxOutput("veces"),
                    valueBoxOutput("tiempo"),
                    valueBoxOutput("tiempoMax")
                    
                ),
                column(5,
                       fluidRow(
                           knobInput(
                               inputId = "tasaEntrada",
                               label = "Tasa de entrada al centro de acopio (Lotes/h laboral):",
                               value = 10,
                               min = 0,
                               max = 1000,
                               step = 1,
                               displayPrevious = TRUE,
                               lineCap = "round",
                               fgColor = "#262A10",
                               inputColor = "#262A10"
                           )
                       ),
                       fluidRow(
                           column(6,
                                  knobInput(
                                      inputId = "tiempo1",
                                      label = "Tiempo RT:",
                                      value = 0.153,
                                      min = 0,
                                      max = 10,
                                      step = 0.01,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           ),
                           column(6,
                                  knobInput(
                                      inputId = "tiempoSAL",
                                      label = "Tiempo SAL:",
                                      value = 6.213,
                                      min = 0,
                                      max = 10,
                                      step = 0.01,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           )
                       ),
                       fluidRow(
                           column(6,
                                  knobInput(
                                      inputId = "tiempoSO",
                                      label = "Tiempo SO:",
                                      value = 2.556,
                                      min = 0,
                                      max = 10,
                                      step = 0.01,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           ),
                           column(6,
                                  knobInput(
                                      inputId = "tiempoAS",
                                      label = "Tiempo AS:",
                                      value = 0.124,
                                      min = 0,
                                      max = 10,
                                      step = 0.01,
                                      displayPrevious = TRUE,
                                      lineCap = "round",
                                      fgColor = "#90BE6D",
                                      inputColor = "#90BE6D"
                                  )
                           )
                       )
                ),
                column(7,
                    fluidRow(
                        box(title = "Comparativa General", solidHeader = T,
                            width = 12, collapsible = T,
                            plotlyOutput("sensibilidadGeneral"))
                    )
                )
            ),
            tabItem("politicaOptima",
                    fluidRow(
                        valueBoxOutput("cosecha"),
                        valueBoxOutput("politica")
                    ),
                    column(5,
                           fluidRow(
                               selectInput("precipitaciones", h3("Seleccione Clasificación"), 
                                           choices = list("Lluvia con granizo de gran tamaño" = 351, 
                                                          "Lluvia torrencial y granizo" = 300,
                                                          "Lluvia torrencial" = 175,
                                                          "Lluvia muy fuerte" = 70, 
                                                          "Lluvia fuerte" = 28,
                                                          "Lluvia Moderada" = 11.25,
                                                          "Lluvia Ligera" = 4, 
                                                          "Lluvia débil" = 1.88,
                                                          "Lluvia muy débil" = 0.7,
                                                          "Nada o traza de precipitación" = 0.2), selected = 1)
                           ),
                           fluidRow(
                               selectInput("precipitaciones", h3("Seleccione Estado del cultivo"), 
                                           choices = list("Estado 1" = 1, 
                                                          "Estado 2" = 2,
                                                          "Estado 3" = 3,
                                                          "Estado 4" = 4, 
                                                          "Estado 5" = 5), selected = 1)
                           )
                    ),
                    column(7,
                           plotOutput("mapaCalor")
                    )
            )
        )    
    )
)
 
