library(here)
source(here::here("NUEVO/Libraries.R"))


# importar  ----------------------------------------------------------------


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


#Fijamos incremento para hacer más grande el mapa
incr<- 0.06

if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- c(n,w)  #Upper Left
lr <- c(s,e)  #Lower Right




# Descargar y guardar mapas -----------------------------------------------

#Se puede seleccionar el tipo de mapa a descargar
# si no pones nada descarga todos los mapas disponibles
#Se puede cambiar la resolución, pero esta por defecto en 
# 40 numtiles
download_maps(ul,lr)



# Plotear mapas -----------------------------------------------------------

#Buscamos las carpetas que contienen los mapas
map_folder<- find_mapfolder()

#Aquí seleccionamos la carpeta que queremos plotear
dir.path<- map_folder[2]


map_files<- list.files(dir.path, full.names = TRUE)
nombre<- str_remove(list.files(dir.path), ".Rdata")

for (i in 1: length(map_files)) {
  
  load(file = map_files[i])
  ggmap1<- map_wpoints(map.latlon = map.latlon, 
                       Coord_era = Coord_era, 
                       Coord_anemo = Coord_anemo)
  ggmap1
  
  ggsave(paste0(dir.path,'/',nombre[i],".tiff"),
         device = "tiff", dpi=1200,
         width =7, height =7, 
         units = 'in')
  
  
}
