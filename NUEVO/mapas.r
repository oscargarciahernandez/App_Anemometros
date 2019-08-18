library(here)
source(here::here("NUEVO/Libraries.R"))


# Importar  ----------------------------------------------------------------


#Importar ERA5_df
if (!exists("ERA5_df")) {
  source(here::here("/NUEVO/ERA5.R"))
}

#Importar TABLA DE REGISTRO
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}


#Sacar todas las coordenadas ERA5 y guardarlas en un Rdata
if (file.exists(here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))) { 
  Coordenadas_era<- readRDS(here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))}else{
    Coordenadas_era<- unique(ERA5_df[,c("lon","lat")])
    saveRDS(Coordenadas_era, file=here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))
  }


# Seleccionar coordenadas -------------------------------------------------


#Sacar coordenadas anemos de la tabla de registro
Coordenadas_anemos<- as.data.frame(cbind(as.numeric(sub(",",".",as.character(t_reg$lon))),
                                         as.numeric(sub(",",".",as.character(t_reg$lat)))))
colnames(Coordenadas_anemos)=c("lon","lat")

#Seleccionar anemo
Coord_anemo<- Coordenadas_anemos[1,]


#Ordenarlos puntos del ERA de cercanos a lejanos
Coord_era<- Coordenadas_era[order((Coordenadas_era$lon-Coord_anemo$lon)^2+(Coordenadas_era$lat-Coord_anemo$lat)^2),]


#Coger los n mas cercanos
n=1
Coord_era=Coord_era[1:n,]


#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(c(Coord_era$lat),Coord_anemo$lat)    
s=min(c(Coord_era$lat),Coord_anemo$lat)    
e=max(c(Coord_era$lon),Coord_anemo$lon)    
w=min(c(Coord_era$lon),Coord_anemo$lon)    


#Fijamos incremento para hacer más grande el mapa

incr<- 0.0215


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right




# Descargar y guardar mapas -----------------------------------------------

#Se puede seleccionar el tipo de mapa a descargar
# si no pones nada descarga todos los mapas disponibles
#Se puede cambiar la resolución, pero esta por defecto en 
# 40 numtiles
download_maps(ul,lr, res=40, maptyp = c("esri-topo","nps","bing"))



# Plotear mapas -----------------------------------------------------------

#Buscamos las carpetas que contienen los mapas
map_folder<- find_mapfolder()

#Aquí seleccionamos la carpeta que queremos plotear

dir.path<- map_folder[5]



#plotear y guardar los ploteos con los puntos
map_files<- list.files(dir.path, full.names = TRUE) %>% .[str_detect(., ".Rdata")]
nombre<- str_split(map_files,"/") %>% lapply(., function(x) return(x[length(x)])) %>% str_remove(., ".Rdata")

for (i in 1: length(map_files)) {
  
  load(file = map_files[i])
  ggmap1<- map_wpoints(map.latlon = map.latlon, 
                       Coord_era = Coord_era, 
                       Coord_anemo = Coord_anemo)
  ggmap1
  
  ggsave(paste0(dir.path,'/',nombre[i],".png"),
         device = "png", dpi=1200,
         width =7, height =7, 
         units = 'in')
  
  
}




#La anchura, la paleta de colores y la opcidad todavía estan por afinar
#♠Mapas2.R está preparado para plotear las rosas cambiando estos parámetros y poder comparar
#Se crean mogollón de carpetas con las imagenes de diferentes formatos. 

#ploteo mapas con rosas de los vientos
ERA5_cutdata<- ERA5_df[which(ERA5_df$lon%in%Coord_era$lon & ERA5_df$lat%in%Coord_era$lat),]

map_files<- list.files(dir.path, full.names = TRUE) %>% .[str_detect(., ".Rdata")]

nombre<- str_split(map_files,"/") %>% lapply(., function(x) return(x[length(x)])) %>% str_remove(., ".Rdata")

p_ros<- WR_parameters(data = ERA5_cutdata, 
                      anchura = 0.06, 
                      paleta = "YlGnBu",
                      opacidad = 0.5)



for (i in 1: length(map_files)) {
  if(exists("map.latlon")){rm(map.latlon)}
  
  load(file = map_files[i])
  pmap2<-autoplot(map.latlon)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),legend.position="none",
                                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                     panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  dev.off()
  
  pmap2+p_ros$subgrobs
  
  ggsave(paste0(dir.path,'/',nombre[i],"_WR.png"),
         device = "png", dpi=1200,
         width =7, height =7, 
         units = 'in')
  
  
}



# MAPAS PARA INFORME CLIENTES ---------------------------------------------
library(rasterVis)
library(elevatr)

DATOS<- here::here("NUEVO/Data_calibracion/0B76C7C0FD4F/ERA5_2019-06-01.RDS") %>% readRDS()

LON_ANEMOMETRO= unique(DATOS$lon)
LAT_ANEMOMETRO= unique(DATOS$lat)

n=LAT_ANEMOMETRO  
s=LAT_ANEMOMETRO    
e=LON_ANEMOMETRO
w=LON_ANEMOMETRO


#Fijamos incremento para hacer más grande el mapa

incr<- 0.1


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right




# Descargar y guardar mapas -----------------------------------------------

#Se puede seleccionar el tipo de mapa a descargar
# si no pones nada descarga todos los mapas disponibles
#Se puede cambiar la resolución, pero esta por defecto en 
# 40 numtiles
PATH_TO_DOWNLOAD_MAPS<-  here::here('NUEVO/Mapas/0B76C7C0FD4F/')
download_maps(ul,lr, res=40, 
              maptyp = c("esri-topo","nps","bing"), 
              PATH_TO_DOWNLOAD = PATH_TO_DOWNLOAD_MAPS)

MAP_FOLDERS<- find_mapfolder(PATH_TO_DOWNLOAD_MAPS)

MAPS_RDS<- list.files(MAP_FOLDERS[1], full.names = TRUE)



for (i in 1: length(MAPS_RDS)) {
  nombre<- str_split(MAPS_RDS[1], '/') %>% .[[1]]%>% .[length(.)] %>% str_replace(., '.RDS', '')
  map.latlon<- readRDS(file = MAPS_RDS[i])
  ggmap1<- map_anemo_point(map.latlon = map.latlon,
                       Coord_anemo =data.frame(lon= LON_ANEMOMETRO, lat= LAT_ANEMOMETRO))
  ggmap1
  
  ggsave(paste0(MAP_FOLDERS[1],'/',nombre[i],".png"),
         device = "png", dpi=300,
         width =7, height =7, 
         units = 'in')
  
  
}




LON_ANEMOMETRO= unique(DATOS$lon)
LAT_ANEMOMETRO= unique(DATOS$lat)




#Fijamos incremento para hacer más grande el mapa
for( i in c(0.1,0.05,0.01,0.005,0.0025)){
  incr<- i
  zoom_map<- 12# VALOR ENTRE 0 Y 14
  
  n=LAT_ANEMOMETRO  + incr
  s=LAT_ANEMOMETRO  - incr
  e=LON_ANEMOMETRO + incr
  w=LON_ANEMOMETRO - incr
  
  
  ul <- c(n,w) #Upper Left
  lr <-c(s,e)#Lower Right
  
  
  lon_location<- seq(lr[1],ul[1], by= 0.001  ) 
  lat_location<- seq(ul[2],lr[2], by = 0.001 )
  data_loc<- expand.grid(lat_location,lon_location)
  colnames(data_loc)<- c("x", "y")
  
  spdf <- SpatialPointsDataFrame(coords = data_loc, data = data_loc,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  FILE_NAME<- paste0(PATH_TO_DOWNLOAD_MAPS, '/map_raster_incr',incr,'zoom_', zoom_map,'.RDS')
  if(file.exists(FILE_NAME)){
    map_raster<- readRDS(FILE_NAME)
  }else{
    map_raster<- get_elev_raster(spdf, z=zoom_map)
    saveRDS(map_raster, FILE_NAME)
  }
  
  levelplot(map_raster, par.settings= viridisTheme()) + 
    layer(panel.points(LON_ANEMOMETRO ,
                       LAT_ANEMOMETRO, pch=21, cex=1, colour='white', fill= 'red'))
  
  
  
}


