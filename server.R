library(shiny)
library(leaflet)
library(RColorBrewer)
library(knitr)
library(vcd)
library(grid)
library(plotly)
#library(googleVis)
library(igraph)
library(tidyverse)

# Define server logic for slider examples
shinyServer(function(input, output, session) {
  
  
  #Ventana 1
  #### For the map in leaflet
  points <- reactive({
    #input$update
    #TableL <- TableL()
    req(input$NOM_ENT)
    
    if (input$NOM_ENT != "All") {
      FinalTT <- FinalTT[FinalTT$NOM_ENT %in% input$NOM_ENT,]
    }else FinalTT <- FinalTT
    
  })
  
 
    
  #head(TianguisFF)
  #names(TianguisFF)
  #summary(TianguisFF)
  #vtess <- deldir(Tianguis_1[,6:7])
  
  #class(vtess)
  #summary(vtess)
  #dim(as.data.frame(vtess$summary$dir.area))
  
  #summary(vtess$delsgs)
  #summary(vtess$dirsgs)
  #summary(vtess$ind.orig)
  
  
  
  
  #P el mapa en leaflet
  output$mymap1 <- renderLeaflet(
    {
      Goldberg <- points()
      #Goldberg$Variable1 <- as.factor(Goldberg$Variable1)
      
      Tianguis_1 <- Goldberg %>%
        #filter(Variable1 != "NE") %>%
        filter(Variable1 == "Tianguis")
      
      Mercados_1 <- Goldberg %>%
       # filter(Variable1 != "NE") %>%
        filter(Variable1 != "Tianguis")
      
      
      vor_pts <- SpatialPointsDataFrame(cbind(Tianguis_1$lng,
                                              Tianguis_1$lat),
                                        Tianguis_1, match.ID = TRUE)
      
      vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)
      
      pal <- colorFactor(c("navy", "red", "black"), domain = c("Tianguis", "Maiz", "Sin_Maiz"))
      
      
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
        addCircleMarkers(data = Tianguis_1, 
                         ~lng, ~lat, 
                         popup = paste(sep = " ","Municipio:",Tianguis_1$NOM_MUN,
                                       "<br/>","Localidad:",Tianguis_1$NOM_LOC,
                                       "<br/>","Tipo:",Tianguis_1$Variable1),
                         radius = ~ifelse(Variable1 == "Tianguis", 7, 6),
                         color = ~pal(Variable1),
                         stroke = FALSE, fillOpacity = 0.5) %>%
        
        #Para los sitios con maíz
        addCircleMarkers(data = Mercados_1, 
                         ~lng, ~lat, 
                         popup = paste(sep = " ","Municipio:",Mercados_1$NOM_MUN,
                                       "<br/>","Localidad:",Mercados_1$NOM_LOC,
                                       "<br/>","Tipo:",Mercados_1$Variable1),
                         radius = ~ifelse(Variable1 == "Maiz", 4, 4),
                         color = ~pal(Variable1),
                         stroke = FALSE, fillOpacity = 0.5
                    #    clusterOptions = markerClusterOptions(showCoverageOnHover = T, 
                    #                                           spiderfyOnMaxZoom = T,
                    #                                           zoomToBoundsOnClick = T,
                    #                                           spiderfyDistanceMultiplier = 2)
                         )
      
      
  
      
      
      
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
  
 
  
  
  
})
