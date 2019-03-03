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
library(rgdal)

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
#https://www.r-bloggers.com/the-openstreetmap-package-opens-up/
#Dejo solo en nm los que funcionan por el estado, y escribo aqui la descripcion de los tipos de mapa que podrian ser interesantes
  #osm: mapa estandar OpenStreetMap
  #mapquest: mapa topografico, soso pero preciso
  #bing: fotos aereas sin etiquetas ni nada
  #stamen-toner: mapas poco precisos en blanco y negro, ninguna utilidad cientifica, pero puede quedar chulo, muy industrial
  #stamen-terrain: mapa con los relieves marcados con sombras, de un vistazo se ve bien la forma del terreno
  #stamen-watercolor: tipo acuarela, solo valor artistico, mola si haces mucho zoom en una ciudad
  #esri-topo: parecido a mapquest
nm = c("osm", "mapquest",
       "bing", "stamen-toner", "stamen-terrain",
       "stamen-watercolor", "osm-german", "osm-wanderreitkarte",
       "esri", "esri-topo",
       "opencyclemap", "osm-transport",
       "osm-public-transport", "osm-bbike", "osm-bbike-german")
map <- openmap(ul,lr, minNumTiles=2,type="apple-iphoto",zoom=NULL)  #minNumTiles es proporcional a la resolucion del mapa. Muy pequeño: no se ve una mierda. Muy grande: da error
graphics.off()
plot(map)
#Esto abre una interfaz de Java que te permite navegar por el mapa del mundo
#Sirve para ver como quedarian los mapas y para conseguir coordendas, nada mas
launchMapHelper()
#Marcar puntos anemos en rojo
Coordenadas_anemos_spt=Coordenadas_anemos     #Vamos a crear un objeto espacial (_spt) sin sobrescribir el orginal (el dataframe)
coordinates(Coordenadas_anemos_spt)<-~lon+lat
proj4string(Coordenadas_anemos_spt)<-CRS("+init=epsg:4326")
points(spTransform(Coordenadas_anemos_spt,osm()),lwd=5,col="red")
#Marcar puntos era en morado
Coordenadas_era_spt=Coordenadas_era     #Vamos a crear un objeto espacial (_spt) sin sobrescribir el orginal (el dataframe)
coordinates(Coordenadas_era_spt)<-~lon+lat
proj4string(Coordenadas_era_spt)<-CRS("+init=epsg:4326")
points(spTransform(Coordenadas_era_spt,osm()),lwd=5,col="purple")

#Si vemos que puntos tiene plot, nos hacemos una idea de como funciona esto
axis(side = 1)
axis(side = 2)
