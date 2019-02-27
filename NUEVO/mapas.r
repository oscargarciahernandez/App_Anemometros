# the github version of ggmap, which recently pulled in a small fix I had
# for a bug 
#devtools::install_github("dkahle/ggmap")

if (!exists("ERA5_df")) {
  source(here::here("/NUEVO/ERA5.R"))
}

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(OpenStreetMap)
library(rJava)

Coordenadas_anemos=data.frame(a=as.numeric(),b=as.numeric())
colnames(Coordenadas_anemos)=c("lat","lon")
Coordenadas_anemos[1,]=c(43.179389,-2.488504)
#Coordenadas=list(Coordenadas_anemos,Coordendas_era)   #Por si algun dia queremos tener todas las coordenadas en un solo sitio



#Vamos a representar marcar en el mapa el anemo y os n puntos mas cercanos a este
n=max(c(Coordenadas_era$lat),Coordenadas_anemos$lat)    #De todos los puntos a representar, cual esta mas al norte?
s=min(c(Coordenadas_era$lat),Coordenadas_anemos$lat)    #Mas al sur?
e=max(c(Coordenadas_era$lon),Coordenadas_anemos$lon)    #Mas al este?
w=min(c(Coordenadas_era$lon),Coordenadas_anemos$lon)    #Mas al oeste?

ul <- c(n,w)  #Upper Left
lr <- c(s,e)  #Lower Right
par(mfrow=c(2,3))
url <- "https://a.tiles.mapbox.com/v4/mapquest.streets-mb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwcXVlc3QiLCJhIjoiY2Q2N2RlMmNhY2NiZTRkMzlmZjJmZDk0NWU0ZGJlNTMifQ.mPRiEubbajc6a5y9ISgydg"
#nm contiene todos los ppsibles atributos de type del comando openmap
#"osm" es el mapa estandar de OpenStreetMap, "stamen-terrain" es mas limpio y se ve la orografia muy bien, "bing" son fotos aereas, sin etiquetas.
#https://www.r-bloggers.com/the-openstreetmap-package-opens-up/
nm = c("osm", "maptoolkit-topo",
       "waze", "mapquest", "mapquest-aerial",
       "bing", "stamen-toner", "stamen-terrain",
       "stamen-watercolor", "osm-german", "osm-wanderreitkarte",
       "mapbox", "esri", "esri-topo",
       "nps", "apple-iphoto", "skobbler",
       "opencyclemap", "osm-transport",
       "osm-public-transport", "osm-bbike", "osm-bbike-german")
map <- openmap(ul,lr, minNumTiles=20,type="stamen-terrain",zoom=NULL)  #minNumTiles es proporcional a la resolucion del mapa. Muy pequeño: no se ve una mierda. Muy grande: da error
graphics.off()
plot(map)
#Esto abre una interfaz de Java que te permite navegar por el mapa del mundo
#Sirve para ver como quedarian los mapas y para conseguir coordendas, nada mas
launchMapHelper()
#Intento de marcar puntos en el mapa
points(x=0,y=0,col="blue",lwd=98)
points(-2.5,43.15,lwd=1000000)

