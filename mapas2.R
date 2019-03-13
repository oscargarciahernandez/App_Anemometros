library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(OpenStreetMap)
library(rJava)
library(rgdal)


# Importar datos necesarios -----------------------------------------------

  
#Importar ERA5_df
if (!exists("ERA5_df")) {
  source(here::here("/NUEVO/ERA5.R"))
}

#Importar TABLA DE REGISTRO
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}


#Sacar todas las coordenadas ERA5 y guardarlas en un Rdata
if (file.exists(here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))) { 
  load(here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))}else{
  Coordenadas_era<- unique(ERA5_df[,c("lon","lat")])
  save(Coordenadas_era, file=here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))
  }
  


# Seleccionar coordenadas -------------------------------------------------


#Sacar coordenadas anemos de la tabla de registro
Coordenadas_anemos<- as.data.frame(cbind(as.numeric(sub(",",".",as.character(t_reg$LON))),
                                         as.numeric(sub(",",".",as.character(t_reg$LAT)))))
colnames(Coordenadas_anemos)=c("lon","lat")

#Seleccionar anemo
Coord_anemo<- Coordenadas_anemos[1,]


#Ordenarlos puntos del ERA de cercanos a lejanos
Coord_era<- Coordenadas_era[order((Coordenadas_era$lon-Coord_anemo$lon)^2+(Coordenadas_era$lat-Coord_anemo$lat)^2),]


#Coger los n mas cercanos
n=9
Coord_era=Coord_era[1:n,]


#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(c(Coord_era$lat),Coord_anemo$lat)    
s=min(c(Coord_era$lat),Coord_anemo$lat)    
e=max(c(Coord_era$lon),Coord_anemo$lon)    
w=min(c(Coord_era$lon),Coord_anemo$lon)    

incr<- 0.03

if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}
  
  

ul <- c(n,w)  #Upper Left
lr <- c(s,e)  #Lower Right








# Mapeo -------------------------------------------------------------------


#minNumTiles mayor, mayor resolución 

map <- openmap(ul,lr, minNumTiles=40,
               type='bing',
               zoom=NULL)  

graphics.off()

#Cambiamos las coordenadas del mapa
#para trabajar con lon, lat
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


pmap<-autoplot(map.latlon)+
  geom_point(data = Coord_era, aes(lon,lat), size=3, colour = "white", alpha=0.7)+
  geom_point(data = Coord_anemo, aes(lon,lat),size=3, colour="red",alpha=0.7)+
  coord_fixed()



pmap2<- pmap + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

prueba<- ERA5_df[which(ERA5_df$lon%in%Coord_era$lon & ERA5_df$lat%in%Coord_era$lat),]
source(here::here('windrose_sin_nada.R'))

p_ros<- prueba%>%group_by(., lon,lat)%>% do(subplots= plot.windrose(., spd = "uv_wind",dir="uv_dwi",dirres = 22.5,spdres = 2,spdseq = c(0,1,2,3,5,7,10,15), palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = lon-0.07,      # change from 1 to other 
                                           y = lat-0.07,      # values if necessary,
                                           xmax = lon+0.07,   # depending on the map's
                                           ymax = lat+0.07))) # resolution.

pmap2+p_ros$subgrobs
  
ggsave(paste0(path_here,"mapaconpuntos_zoom",zoom_in,".tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  
  
  