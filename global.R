library(shiny)
library(leaflet)
library(deldir)
library(tidyverse)
library(rgdal)
#library(mxmaps)
#library(plotly)
library(latticeExtra)
#library(ggforce)
library(plyr)
library(dplyr)

getwd()
#setwd('~/Dropbox/GitHub/VoronoiInegi/')


########################
########################

#Para poner el voronoi en leaflet
#Código de https://rud.is/b/2015/07/26/making-staticinteractive-voronoi-map-layers-in-ggplotleaflet/


SPointsDF_to_voronoi_SPolysDF <- function(sp) {
  
  if (is.null(sp) || is.na(sp))
  {
    return()
  }
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2]))
  
  lapply(1:(length(vor_desc)), function(i) {
    
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    
    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID = i)
    
  }) -> vor_polygons
  
  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data
  
  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),
                                  'polygons'),
                             slot, 'ID')
  
  SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                           data = sp_dat)
}

########################
########################

#Cargar mapa de Mexico

Mex <- read.delim("data/Z_NACIONAL.csv"  , header = T, sep = ",")
dim(Mex)
head(Mex)[1:14]
names(Mex)
names(Mex)[7:8] <- c("lng", "lat")
names(Mex)[7:8]

#Aqui escojo el estado
#target1 <- c("Puebla", "Tlaxcala", "Mexico", "Distrito Federal")

Mex2 <- Mex %>%
  select(CVE_EDO, CVE_MUN, CVE_LOC, lng, lat, NOM_MUN, NOM_LOC, NOM_ENT) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Veracruz de Ignacio de la Llave" = "Veracruz"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Coahuila de Zaragoza" = "Coahuila"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Michoacán de Ocampo" = "Michoacan"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("México" = "Mexico"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Querétaro" = "Queretaro"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Nuevo León" = "Nuevo Leon"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("San Luis Potosí" = "San Luis Potosi"))) %>%
  mutate(NOM_ENT = plyr::revalue(NOM_ENT,c("Yucatán" = "Yucatan"))) %>%
  #filter(NOM_ENT %in% target1 ) %>%
  #filter(Altitud < 5000) %>%
  #select(Raza_primaria, Longitud, Latitud, Altitud) %>%
  na.omit() %>%
  distinct() %>%
  as.data.frame()
Mex2$CVE_EDO <- sprintf("%02d", Mex2$CVE_EDO)
Mex2$CVE_MUN <- sprintf("%03d", Mex2$CVE_MUN)
Mex2$CVE_LOC <- sprintf("%04d", Mex2$CVE_LOC)

Mex2$CVE_EDO <- as.factor(Mex2$CVE_EDO)
Mex2$CVE_MUN <- as.factor(Mex2$CVE_MUN)
Mex2$CVE_LOC <- as.factor(Mex2$CVE_LOC)

summary(Mex2)

Mex2 <- Mex2 %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = ""))

#dim(Mex2)
#head(Mex2)

dir()

Edo01 <- read.delim("data/RESLOC2014 - 01 Aguascalientes.csv"  , header = T, sep = ",")
Edo02 <- read.delim("data/RESLOC2014 - 02 Baja California.csv"  , header = T, sep = ",")
Edo03 <- read.delim("data/RESLOC2014 - 03 Baja California Sur.csv"  , header = T, sep = ",")
Edo04 <- read.delim("data/RESLOC2014 - 04 Campeche.csv"  , header = T, sep = ",")
Edo05 <- read.delim("data/RESLOC2014 - 05 Coahuila.csv"  , header = T, sep = ",")
Edo06 <- read.delim("data/RESLOC2014 - 06 Colima.csv"  , header = T, sep = ",")
Edo07 <- read.delim("data/RESLOC2014 - 07 Chiapas.csv"  , header = T, sep = ",")
Edo08 <- read.delim("data/RESLOC2014 - 08 Chihuahua.csv"  , header = T, sep = ",")
Edo09 <- read.delim("data/RESLOC2014 - 09 Distrito Federal.csv"  , header = T, sep = ",")
Edo10 <- read.delim("data/RESLOC2014 - 10 Durango.csv"  , header = T, sep = ",")
Edo11 <- read.delim("data/RESLOC2014 - 11 Guanajuato.csv"  , header = T, sep = ",")
Edo12 <- read.delim("data/RESLOC2014 - 12 Guerrero.csv"  , header = T, sep = ",")
Edo13 <- read.delim("data/RESLOC2014 - 13 Hidalgo.csv"  , header = T, sep = ",")
Edo14 <- read.delim("data/RESLOC2014 - 14 Jalisco.csv"  , header = T, sep = ",")
Edo15 <- read.delim("data/RESLOC2014 - 15 Mexico.csv"  , header = T, sep = ",")
Edo16 <- read.delim("data/RESLOC2014 - 16 Michoacan.csv"  , header = T, sep = ",")
Edo17 <- read.delim("data/RESLOC2014 - 17 Morelos.csv"  , header = T, sep = ",")
Edo18 <- read.delim("data/RESLOC2014 - 18 Nayarit.csv"  , header = T, sep = ",")
Edo19 <- read.delim("data/RESLOC2014 - 19 Nuevo Leon.csv"  , header = T, sep = ",")
Edo20 <- read.delim("data/RESLOC2014 - 20 Oaxaca.csv"  , header = T, sep = ",")
Edo21 <- read.delim("data/RESLOC2014 - 21 Puebla.csv"  , header = T, sep = ",")
Edo22 <- read.delim("data/RESLOC2014 - 22 Queretaro.csv"  , header = T, sep = ",")
Edo23 <- read.delim("data/RESLOC2014 - 23 Quintana Roo.csv"  , header = T, sep = ",")
Edo24 <- read.delim("data/RESLOC2014 - 24 San Luis Potosi.csv"  , header = T, sep = ",")
Edo25 <- read.delim("data/RESLOC2014 - 25 Sinaloa.csv"  , header = T, sep = ",")
Edo26 <- read.delim("data/RESLOC2014 - 26 Sonora.csv"  , header = T, sep = ",")
Edo27 <- read.delim("data/RESLOC2014 - 27 Tabasco.csv"  , header = T, sep = ",")
Edo28 <- read.delim("data/RESLOC2014 - 28 Tamaulipas.csv"  , header = T, sep = ",")
Edo29 <- read.delim("data/RESLOC2014 - 29 Tlaxcala.csv"  , header = T, sep = ",")
Edo30 <- read.delim("data/RESLOC2014 - 30 Veracruz.csv"  , header = T, sep = ",")
Edo31 <- read.delim("data/RESLOC2014 - 31 Yucatan.csv"  , header = T, sep = ",")
Edo32 <- read.delim("data/RESLOC2014 - 32 Zacatecas.csv"  , header = T, sep = ",")


head(Edo01)

EdoTT <- Edo18 %>%
  select(Clave.de.localidad, Nombre.de.la.localidad, Abasto.de.maíz, Disponibilidad.de.tianguis.o.mercado.sobre.ruedas) %>%
  #filter(Disponibilidad.de.tianguis.o.mercado.sobre.ruedas == "Dispone") %>%
  distinct()
summary(EdoTT)

#EdoTT1 <- EdoTT %>%
  #filter(Abasto.de.maíz == "Dispone") %>%
#  filter(Abasto.de.maíz == "No dispone") %>%
#  na.omit()

#summary(EdoTT1)


#Aqui cambio el estado
Edo <- rbind(Edo01,Edo02,Edo03,Edo04,Edo05,Edo06,Edo07,Edo08,Edo09,Edo10,
                 Edo11,Edo12,Edo13,Edo14,Edo15,Edo16,Edo17,Edo18,Edo19,Edo20,
                 Edo21,Edo22,Edo23,Edo24,Edo25,Edo26,Edo27,Edo28,Edo29,Edo30,
                 Edo31,Edo32)
summary(Edo)
names(Edo)

summary(Edo$Nombre.de.la.entidad)

head(Edo)
names(Edo)

#Para seleccionar los mercados
Edo100 <- Edo %>%
  select(Clave.de.entidad.federativa,
         Clave.de.municipio.o.delegación,
         Clave.de.localidad,
         Nombre.de.la.entidad,
         Disponibilidad.de.tianguis.o.mercado.sobre.ruedas) %>%
  filter(Disponibilidad.de.tianguis.o.mercado.sobre.ruedas == "Dispone" ) %>%
  mutate(Disponibilidad.de.tianguis.o.mercado.sobre.ruedas = revalue(Disponibilidad.de.tianguis.o.mercado.sobre.ruedas,c("Dispone" = "Tianguis"))) %>%
  na.omit() %>%
  distinct()

Edo100  
summary(Edo100)
dim(Edo100)
names(Edo100) <- c("CVE_EDO", "CVE_MUN", "CVE_LOC","Nombre","Variable1")
names(Edo100)
head(Edo100)
Edo100$CVE_EDO <- sprintf("%02d", Edo100$CVE_EDO)
Edo100$CVE_MUN <- sprintf("%03d", Edo100$CVE_MUN)
Edo100$CVE_LOC <- sprintf("%04d", Edo100$CVE_LOC)

Edo100$CVE_EDO <- as.factor(Edo100$CVE_EDO)
Edo100$CVE_MUN <- as.factor(Edo100$CVE_MUN)
Edo100$CVE_LOC <- as.factor(Edo100$CVE_LOC)


head(Edo100)
Edo100 <- Edo100 %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = "")) %>%
  select(Variable1, FullCode)

head(Mex2)
head(Edo100)

TianguisFF <- inner_join(Edo100,Mex2, by = "FullCode")
dim(TianguisFF)
head(TianguisFF)
#########

#Paara seleccioanr los sitios con abasto de maíz y sin abasto

names(Edo)
summary(Edo)
dim(Edo)
EdoHH <- Edo %>%
  select(Clave.de.entidad.federativa,
         Clave.de.municipio.o.delegación,
         Clave.de.localidad,
         Nombre.de.la.entidad,
         Abasto.de.maíz) %>%
  mutate(Abasto.de.maíz = revalue(Abasto.de.maíz,c("Dispone" = "Maiz"))) %>%
  mutate(Abasto.de.maíz = revalue(Abasto.de.maíz,c("No dispone" = "Sin_Maiz"))) %>%
  #filter(Abasto.de.maíz == "Dispone" ) %>%
  na.omit() %>%
  distinct()
head(EdoHH)
summary(EdoHH)
dim(EdoHH)

EdoHH  

dim(EdoHH)
names(EdoHH) <- c("CVE_EDO", "CVE_MUN", "CVE_LOC","Nombre","Variable1")
names(EdoHH)
head(EdoHH)
EdoHH$CVE_EDO <- sprintf("%02d", EdoHH$CVE_EDO)
EdoHH$CVE_MUN <- sprintf("%03d", EdoHH$CVE_MUN)
EdoHH$CVE_LOC <- sprintf("%04d", EdoHH$CVE_LOC)

EdoHH$CVE_EDO <- as.factor(EdoHH$CVE_EDO)
EdoHH$CVE_MUN <- as.factor(EdoHH$CVE_MUN)
EdoHH$CVE_LOC <- as.factor(EdoHH$CVE_LOC)


head(EdoHH)
EdoHH <- EdoHH %>%
  mutate(FullCode = paste(CVE_EDO,CVE_MUN,CVE_LOC, sep = "")) %>%
  select(Variable1, FullCode)

  
  #factor(Variable1)

class(EdoHH)
str(EdoHH)
head(Mex2)
head(EdoHH)

TianguisHH <- inner_join(EdoHH,Mex2, by = "FullCode")


#dim(TianguisHH)
#head(TianguisHH)

#dim(TianguisFF)
#dim(TianguisHH)

#str(TianguisFF)
#str(TianguisHH)


FinalTT <- rbind(TianguisHH, TianguisFF) %>% 
  distinct()

#head(FinalTT)
#tail(FinalTT)
#str(TianguisHH)
#str(FinalTT)

#Para hacer el voronoir



#Tipo
#pal <- colorFactor(c("navy", "red", "black"), domain = c("Tianguis", "Maiz", "Sin_Maiz"))
#pal <- colorFactor(c("navy", "red"), domain = c("No dispone", "Dispone"))


#leaflet(data = FinalTT) %>% 
#  addTiles() %>%
#  addCircleMarkers(~lng, ~lat, 
#                   popup = paste(sep = " ","Municipio:",FinalTT$NOM_MUN,
#                                 "<br/>","Localidad:",FinalTT$NOM_LOC,
#                                 "<br/>","Tipo:",FinalTT$Tianguis),
#                   radius = ~ifelse(Tianguis == "Tianguis", 10, 6),
#                   color = ~pal(Tianguis),
#                   stroke = FALSE, fillOpacity = 0.5) %>%
#  #Select the kind of map: http://leaflet-extras.github.io/leaflet-providers/preview/
#  addProviderTiles("OpenStreetMap.DE")
#addProviderTiles("Thunderforest.Pioneer")



#head(TianguisFF[,6:7])

#vor_pts <- SpatialPointsDataFrame(cbind(TianguisFF$lng,
#                                        TianguisFF$lat),
#                                  TianguisFF, match.ID = TRUE)
#
#vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)


#vor

#FinalTT1 <- FinalTT %>%
#  filter(Tianguis != "Sin_Maiz")
  
#quakes.df <- split(FinalTT, FinalTT$Tianguis)
#head(quakes.df)
#names(quakes.df)
#quakes.df$Maiz


    #  overlayGroups = c("Tianguis"),
  #  options = layersControlOptions(collapsed = FALSE))



#leaflet() %>% 
#  #  addTiles() %>%
#  addProviderTiles("OpenStreetMap.DE") %>%
#  
#  addPolygons(data = vor,
#              stroke = T, color = "green", weight = 2,
#              fill = F, fillOpacity = 0.0,
#              smoothFactor = 0.5
              #popup = sprintf("Total In/Out: %s",
              #              as.character(vor@data$tot))
#  ) %>%
  #Para los tianguis
 # addCircleMarkers(data = TianguisFF, 
#                   ~lng, ~lat, 
#                   popup = paste(sep = " ","Municipio:",TianguisFF$NOM_MUN,
#                                 "<br/>","Localidad:",TianguisFF$NOM_LOC,
#                                 "<br/>","Tipo:",TianguisFF$Tianguis),
#                   radius = ~ifelse(Tianguis == "Tianguis", 7, 6),
#                   color = ~pal(Tianguis),
#                   stroke = FALSE, fillOpacity = 0.5) %>%
                   
  #Para los sitios con maíz
#  addCircleMarkers(data = TianguisHH, 
#                   ~lng, ~lat, 
#                   popup = paste(sep = " ","Municipio:",TianguisHH$NOM_MUN,
#                                 "<br/>","Localidad:",TianguisHH$NOM_LOC,
#                                 "<br/>","Tipo:",TianguisHH$Tianguis),
#                   radius = ~ifelse(Tianguis == "Tianguis", 4, 6),
#                   color = ~pal(Tianguis),
#                   stroke = FALSE, fillOpacity = 0.5,
#                   clusterOptions = markerClusterOptions(showCoverageOnHover = T, 
#                                                         spiderfyOnMaxZoom = T,
#                                                         zoomToBoundsOnClick = T,
#                                                         spiderfyDistanceMultiplier = 2))
  





#caixa <- function(x){
#  print("mutilicado por 12")
#  a <- x * 12
#  print(a)
#  print("mutilicado por 11")
#  b <- x * 11
#  print(b)
#  print("mutilicado por 6")
#  b <- x * 6
#  print(b)
#}

#caixa(12)
