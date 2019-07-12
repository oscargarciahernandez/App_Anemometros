library(here)
source(here::here('NUEVO/Libraries.R'))


# TFG JON ANDER -----------------------------------------------------------


COSAS_JONANDER<- FALSE
if(COSAS_JONANDER){
  
  x<- here::here('NUEVO/Data_ERA5/') %>% list.files(full.names = T)
  lista_jony<- list()
  for (i in 1:40) {
    y<- x[i] %>% readRDS()
    h<- y %>% filter(lon<= -2.7, lon>= -3.3) %>% 
      filter(lat<= 43.2, lat>= 42.9)
    lista_jony[[i]]<- h
  }
  
  
  tabla_jony[, c("wind","dwi","Dir_dwi")]<- NULL
  
  write.table(tabla_jony, '/media/oscar/Lexar/ERA5_79_19.csv',sep = ";",
              dec = ".",col.names = TRUE, row.names = FALSE)
  
  
  
  
  
  
  library(rnaturalearth)
  library(rnaturalearthdata)
  world <- ne_countries(country = "spain", scale = "small", returnclass = "sf")
  
  ggplot(data = world) +
    geom_sf()+
    geom_point(data= h, aes(x= lon, y= lat), color= "red", size=0.5)+
    theme_light()
}





# ELEVATER ----------------------------------------------------------------


library(elevatr)
library(rasterVis)
library(rgl)

#DESCARGAR DATOS DE ELEVACION USANDO elevatr
#PLOTEAR DATOS CON RasterVis

#LO HE PENSADO PARA QUE FUNCIONE SELECCIONANDO n,s,e y w 
# DEL MISMO MODO QUE HACIAMOS PARA DESCARGAR LOS TILES CON 
# OPENSTREETMAP

#IMPORTAMOS DATOS
DATOS_ANEMOS<- here::here('NUEVO/Data_calibracion/0B38DAE79059/ERA5_2019-03-01.RDS') %>% readRDS()


#HACEMOS TABLA DE PUNTOS DE ERA Y ANEMO 
long<- DATOS_ANEMOS$ERAlon %>% unique()
lat<- DATOS_ANEMOS$ERAlat %>% unique()

grid_ERA<- expand.grid(long, lat)
grid_ERA[, c("lon_A","lat_A")]<- c(rep(DATOS_ANEMOS$lon %>% unique(),nrow(grid_ERA)),
                                   rep(DATOS_ANEMOS$lat %>% unique(),nrow(grid_ERA)))

#AÑADIMOS DISTANCIAS A LA TABLA
vector_dist<- vector()
for (i in 1:nrow(grid_ERA)) {
  vector_dist[i]<- distm(c(grid_ERA$lon[i], grid_ERA$lat[i]),
                         c(grid_ERA$lon_A[i], grid_ERA$lat_A[i]))
  
}
grid_ERA$dist<- vector_dist
colnames(grid_ERA)<- c("lon", "lat", "lon_A", "lat_A", "dist")



#ESCOJEMOS LOS PUNTOS PARA DESCARGAR EL MAPA DE RELIEVE
PUNTO_MAS_CERCANO<- TRUE
if(PUNTO_MAS_CERCANO){
  P_selected<- grid_ERA[which.min(grid_ERA$dist),]
}else{
    P_selected<- grid_ERA
  }
n=max(P_selected$lat)    
s=min(P_selected$lat)    
e=max(P_selected$lon)    
w=min(P_selected$lon) 

#AÑADIMIOS UN INCREMENTO Y UN ZOOM
# EL ZOOM VA DESDE 1 HASTA 14...
# OJITO, MÁS ZOOM, IMPLICA MÁS RESOLUCIÓN Y POR TANTO MAYOR PESO
incr<- 0.007
zoom_map<- 14

#Tengo que investigar más al respecto, pero creo que tiene relacion 
# directa con la resolución
# yo cuando pido un archivo tengo que pedirle el grid que quiero
# esta variable añade puntos al grid...
#Lo que no se es si añadiendo mas puntos se consigue mejorar la 
# resolución del raster
Point_number<- 50


if(n > 0){n<- n + incr}else{n<- n + incr}
if(s > 0){s<- s - incr}else{s<- s- incr}
if(e > 0){e<- e + incr}else{e<- e + incr}
if(w > 0){w<- w - incr}else{w<- w- incr}



ul <- round(c(n,w),digits = 8)  #Upper Left
lr <- round(c(s,e), digits = 8)  #Lower Right


path_raster<- here::here('NUEVO/Mapas/Rasters/')
if(!dir.exists(path_raster)){dir.create(path_raster, 
                                        recursive = TRUE)}

#PONGO ESE IF DENTRO DEL BUCLE PORQUE TARDA LA VIDA
# Y SEGURAMENTE DESCARGARÉ LOS RASTERS DE VARIAS VECES...VARIOS DIAS

file_raster<- paste0(path_raster,"RASTER_0B38DAE79059_",incr,"_",
                     zoom_map,"_",ul[1] %>% round(4),"_",
                     lr[1] %>% round(4),".RDS")
if(file.exists(file_raster)){
  print("Este raster ya existe en: ", file_raster)
}else{
  
  lon_location<- seq(lr[1],ul[1],length.out = Point_number ) 
  lat_location<- seq(ul[2],lr[2], length.out = Point_number)
  data_loc<- expand.grid(lat_location,lon_location)
  colnames(data_loc)<- c("x", "y")
  
  spdf <- SpatialPointsDataFrame(coords = data_loc, data = data_loc,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  
  map_raster<- get_elev_raster(spdf, z=zoom_map)
  saveRDS(map_raster, file_raster)
  
}


#EXISTEN VARIAS PALETAS DE COLORES POR DEFECTO PARA PLOTEAR LOS 
# MAPAS
#magma, viridisTheme, infernoTheme, ç
#plasmaTheme, YlOrRdTheme, BuRdTheme, 
# RdBuTheme, GrTheme, BTCTheme)
levelplot(map_raster, par.settings= BTCTheme(), 
          contour=TRUE,
          interpolate=TRUE) + 
  layer(panel.points(grid_ERA$lon,
                     grid_ERA$lat,
                     pch=21, cex=2, 
                     colour='white', 
                     fill= 'white'))+
  layer(panel.points(grid_ERA$lon_A %>% unique(),
                     grid_ERA$lat_A %>% unique(),
                     pch=21, cex=2, 
                     colour='red', 
                     fill= 'red'))


PUNTO<- layer(panel.points(grid_ERA$lon_A %>% unique(),
                   grid_ERA$lat_A %>% unique(),
                   pch=21, cex=2, 
                   colour='red', 
                   fill= 'red'))

#LOCURA EN PROCESO
plot3D(map_raster)
