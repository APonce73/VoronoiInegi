library(shiny)
library(leaflet)
library(knitr)
#library(plotly)
library(tidyverse)
#library(ggplot2)
#library(shinythemes)
library(shinydashboard)



#runApp(system.file("shiny/", package = "googleVis"))
#library(ggplot2movies)

shinyUI(navbarPage(
  title = "Tianguis de México", id = "nav",

 #Ventana 1
    tabPanel("Mapa", 
        #     div(img(src = "CONABIO_LOGO_13.JPG", width = "200"), 
        #         img(src = "CodiceFlorentino.png", width = "500"), style = "text-align: center;"),
            # h4("Proyecto Global de Maíces"),
          #   div(class = "outer",
           # Define UI for slider demo application
           #bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
           
          dashboardBody(
            tags$style(type = "text/css", "#mymap1 {height: calc(100vh - 80px) !important;}"),
            leafletOutput('mymap1', width = "100%", height = "100%")
          ),
                        
                      #   h4("$$$"),
            
           
            absolutePanel(top = 50, right = 40,
                          
                          #Raza Primaria
                #          selectInput(inputId = "Raza_primaria",
                #                      label = h6("Raza Primaria:"),
                #                      c("All", levels(TableL$Raza_primaria))),
                          #c("All", unique(as.character(TableL$Raza_primaria)))),
                          
                          #Por Complejo Racial
                 #         selectInput(inputId = "Complejo_racial",
                #                      label = h6("Grupo Racial:"),
                #                      c("All", levels(TableL$Complejo_racial))),
                          
                          #Por Estado
                          selectInput(inputId = "NOM_ENT",
                                      label = h6("Estado:"),
                                      c("All", levels(FinalTT$NOM_ENT)))
                          
                          
                         # h6("Descargar los datos seleccionados"),
                        #  h6("en la visualización del mapa"),
                         # downloadButton('downloadData', 'Descargar (csv)')
                          
                          #     checkboxGroupInput("Periodo_Colecta", label = h6("Período de Colecta:"),
                          #                        choice = levels(TableL$Periodo_Colecta), selected = levels(TableL$Periodo_Colecta)),
                          
                           ),
            
            absolutePanel(bottom = 10, left = 20,
                          h4("Conabio:"),
                          tags$a(href = "http://www.biodiversidad.gob.mx/genes/proyectoMaices.html", "Proyecto Maices"),
                          br(),
                          #h4("Descarga de la base de datos"),
                          #tags$a(href = "http://www.biodiversidad.gob.mx/genes/pdf/proyecto/Anexo13_Base%20de%20datos/BaseMaicesNativos.xlsx", "DataBase"),
                          #br()
                          h4("Github:"),
                          tags$a(href = "https://github.com/APonce73/Conabio-PGMaices", "Conabio-Maíces"),
                          h5("comentarios:"),
                          h5("aponce@conabio.gob.mx")
                          #uiOutput("fens")
                          )
           # )
           ) #parentesis del tabpanel

            
         
))
