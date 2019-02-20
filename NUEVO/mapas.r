# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))

# some standard map packages.
install.packages(c("maps", "mapdata"))

# the github version of ggmap, which recently pulled in a small fix I had
# for a bug 
devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(OpenStreetMap)
library(rJava)

#Coordenadas que queremos representar----
Coordenadas=list()
Coordenadas_anemos=data.frame(c(NA),c(NA))
colnames(Coordenadas_anemos)=c("lat","lon")
Coordenadas_anemos[1,]=c(43.179389,2.488504)


#View(table(paste0(ERA5_df$lat," - ",ERA5_df$lon)))  #Ver todas las coordenadas
#View(table(ERA5_df$lat))                            #Ver todas las latitudes
#View(table(ERA5_df$lon))                            #Ver todas las latitudes
#Si nos fijamos en la longitud de estas tablas, veremos que los puntos de ERA5 que tenemos en ERA5_df
#forman una matriz perfecta, sin huecos (31*41=1271). Es decir, los puntos son todas las conbinaciones
#posibles de todas las latitdes y longitudes que aparecen.

#Encontrar los n puntos de ERA5 mas cercanos al anemo
#Si el cacho de grid de ERA5 que usamos tuviese algun hueco, este metodo podria
#dar coordenadas que no aparecen en ERA5_df, ya que busca la lon y la lat mas
#cercanas, y supone que las dos forman una coordenada valida
n=4
Coordenadas_era=data.frame(a=as.numeric(),b=as.numeric())
colnames(Coordenadas_era)=c("lat","lon")
latitudes=table(ERA5_df$lat) %>% rownames %>% as.numeric
longitudes=table(ERA5_df$lon) %>% rownames %>% as.numeric
Coordenadas_era[1:4,1]=latitudes[((latitudes-Coordenadas_anemos$lat[1]) %>% abs %>% order)[1:n]]
Coordenadas_era[1:4,2]=longitudes[((longitudes-Coordenadas_anemos$lon[1]) %>% abs %>% order)[1:n]]

#EIbar
#Longitude
lon=-2.466667
#Latitude
lat=43.183333

ul <- c(43.2,-2.52)  #Upper Left
lr <- c(43.15,-2.4)  #Lower Right
par(mfrow=c(2,3))
url <- "https://a.tiles.mapbox.com/v4/mapquest.streets-mb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwcXVlc3QiLCJhIjoiY2Q2N2RlMmNhY2NiZTRkMzlmZjJmZDk0NWU0ZGJlNTMifQ.mPRiEubbajc6a5y9ISgydg"
map <- openmap(ul,lr, minNumTiles=6,type="osm",zoom=NULL)
plot(map)


