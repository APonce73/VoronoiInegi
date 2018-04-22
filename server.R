library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(knitr)
library(vcd)
library(grid)
library(plotly)
library(ggplot2)
library(googleVis)
library(igraph)

# Define server logic for slider examples
shinyServer(function(input, output, session) {
  
  
  #P el mapa en leaflet
  output$mymap1 <- renderLeaflet(
    {
  
      
      leaflet() %>% 
        #  addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(data = vor,
                    stroke = T, color = "green", weight = 2,
                    fill = F, fillOpacity = 0.0,
                    smoothFactor = 0.5
                    #popup = sprintf("Total In/Out: %s",
                    #              as.character(vor@data$tot))
        ) %>%
        #Para los tianguis
        addCircleMarkers(data = TianguisFF, 
                         ~lng, ~lat, 
                         popup = paste(sep = " ","Municipio:",TianguisFF$NOM_MUN,
                                       "<br/>","Localidad:",TianguisFF$NOM_LOC,
                                       "<br/>","Tipo:",TianguisFF$Tianguis),
                         radius = ~ifelse(Tianguis == "Tianguis", 7, 6),
                         color = ~pal(Tianguis),
                         stroke = FALSE, fillOpacity = 0.5) %>%
        
        #Para los sitios con maíz
        addCircleMarkers(data = TianguisHH, 
                         ~lng, ~lat, 
                         popup = paste(sep = " ","Municipio:",TianguisHH$NOM_MUN,
                                       "<br/>","Localidad:",TianguisHH$NOM_LOC,
                                       "<br/>","Tipo:",TianguisHH$Tianguis),
                         radius = ~ifelse(Tianguis == "Tianguis", 4, 6),
                         color = ~pal(Tianguis),
                         stroke = FALSE, fillOpacity = 0.5,
                         clusterOptions = markerClusterOptions(showCoverageOnHover = T, 
                                                               spiderfyOnMaxZoom = T,
                                                               zoomToBoundsOnClick = T,
                                                               spiderfyDistanceMultiplier = 2))
      
      
  
      
      
      
  #      TTT <- c(brewer.pal(8,"Dark2"))
    
    
     #TTT <- colorNumeric(c(1:64), levels(TableL$Raza_primaria))
  #  Goldberg <- points()
    
  #  Trip2 <- points2()
  #  TT <- paste(Goldberg$Raza_primaria)
  #  leaflet(data = Goldberg) %>%
      #clearShapes() %>%
   #   addTiles() %>%
      #clearBounds() %>%  
  #    addCircleMarkers(Goldberg$longitude, Goldberg$latitude, 
  #                     weight = 8, radius = 5, stroke = F, fillOpacity = 0.9, color = sample(TTT,1),
  #                     clusterOptions = markerClusterOptions(showCoverageOnHover = T, 
  #                                                           spiderfyOnMaxZoom = T,
  #                                                           zoomToBoundsOnClick = T,
  #                                                           spiderfyDistanceMultiplier = 2), 
   #                    popup = paste(sep = " ",
  #                                   "Complejo Racial:",Goldberg$Complejo_racial,"<br/>",
  #                                   "Raza Maiz:",Goldberg$Raza_primaria,"<br/>", 
  #                                   "Municipio:",Goldberg$Municipio, "<br/>",
  #                                   "Localidad:",Goldberg$Localidad)) %>%
      
     
        
      #addMeasure(primaryLengthUnit = "kilometers", primaryAreaUnit = "hectares",activeColor = '#FF00FF') %>%
      #addProviderTiles("Esri.WorldTopoMap")
      
      
      
  #    addProviderTiles("CartoDB.Positron")
    # addLayersControl(
    #    overlayGroups = names(Teo1),
    #    options = layersControlOptions(collapsed = FALSE))
    
  })
  
#  observeEvent({# update the map markers and view on map clicks
#    proxy2 <- leafletProxy("mymap1")
#    #proxy2 %>% clearControls()
#    # Trip2 <- points2()
#    if (input$tripsacum) {
#      proxy2 %>%
#        addCircleMarkers(Trip1$long, Trip1$lat, weight = 3, radius = 1, 
#                         color = '#FA5', opacity = 1, stroke = T,
#                         popup = paste(sep = " ",
#                                       "Raza Maiz:",Trip1$Taxa,"<br/>", 
#                                       "Municipio:",Trip1$Municipio))  
#    } 
#  })
  
  observe({
    proxy1 <- leafletProxy("mymap1")
    #proxy1 %>% clearControls()
    # Teo2 <- points1()
    if (input$tripsacum) {
      Trip2 <- Trip1
      proxy1 %>%
        #Teo1 == Teocintle
        addCircleMarkers(Trip2$long, Trip2$lat, weight = 3, radius = 1, color = '#FA5', 
                         opacity = 1,
                         popup = paste(sep = " ",
                                       "Taxa:",Trip2$Taxa,"<br/>", 
                                       #"Municipio:",Trip2$Municipio, "<br/>",
                                       "Municipio:",Trip2$Municipio))
      
    }
    
  })
  
  observe({
    proxy2 <- leafletProxy("mymap1")
    #proxy1 %>% clearControls()
   # Teo2 <- points1()
    if (input$teocintle) {
      Teo2 <- Teo1
      proxy2 %>%
        #Teo1 == Teocintle
      addCircleMarkers(Teo2$long, Teo2$lat, weight = 3, radius = 1, color = '#9D7', 
                       opacity = 1,
                       popup = paste(sep = " ",
                                      "Taxa:",Teo2$Taxa,"<br/>", 
                                     "Municipio:",Teo2$Municipio, "<br/>",
                                     "Localidad:",Teo2$Localidad))
    } 
    #else {
    #  updateSelectInput(session, input$teocintle, selected = "")
    #  proxy2 %>% 
    #   removeMarker("long")
    #}
  })
  
  
  
})
