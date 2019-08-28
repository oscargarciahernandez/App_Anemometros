library(here)
source('NUEVO/Libraries.R')
library(rasterVis)
library(elevatr)

DATOS<- here::here("NUEVO/Data_calibracion/0B76C7C0FD4F/ERA5_2019-06-01.RDS") %>% readRDS()
PATH_TO_DOWNLOAD_MAPS<-  here::here('NUEVO/Mapas/0B76C7C0FD4F/')


LON_ANEMOMETRO= unique(DATOS$lon)
LAT_ANEMOMETRO= unique(DATOS$lat)


#FIJAMOS EL INCREMENTO PARA HACER MAS GRANDE EL MAPA
incr<- 0.1

n=LAT_ANEMOMETRO + incr  
s=LAT_ANEMOMETRO - incr
e=LON_ANEMOMETRO + incr
w=LON_ANEMOMETRO - incr


#CREAMOS ESQUINA SUPERIOR IZQUIERDA E INFERIOR DERCHA
ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right




# DESCARGAR CREAR Y GUARDAR MAPAS OPENSTREETMAP -----------------------------------------------

download_maps(ul,lr, res=40, 
              maptyp = c("esri-topo","nps","bing"), 
              PATH_TO_DOWNLOAD = PATH_TO_DOWNLOAD_MAPS)

MAP_FOLDERS<- find_mapfolder(PATH_TO_DOWNLOAD_MAPS)

MAPS_RDS<- list.files(MAP_FOLDERS[1], full.names = TRUE)



for (i in 1: length(MAPS_RDS)) {
  nombre<- str_split(MAPS_RDS[i], '/') %>% .[[1]]%>% .[length(.)] %>% str_replace(., '.RDS', '')
  map.latlon<- readRDS(file = MAPS_RDS[i])
  ggmap1<- map_anemo_point(map.latlon = map.latlon,
                           Coord_anemo =data.frame(lon= LON_ANEMOMETRO,
                                                   lat= LAT_ANEMOMETRO))
  ggmap1
  
  ggsave(paste0(PATH_TO_DOWNLOAD_MAPS,nombre[i],".png"),
         device = "png", dpi=300,
         width =7, height =7, 
         units = 'in')
  
  
}



# DESCARGAMOS, CREAMOS Y GUARDAMOS MAPAS DE ELEVACION ---------------------
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
  
  ggsave(FILE_NAME %>% str_replace('.RDS', '.png'),
         device = "png", dpi=300,
         width =7, height =7, 
         units = 'in')
}





# CREAMOS MAPA CON ROSA DE LOS VIENTOS ------------------------------------

DATOS_ROSA<- DATOS %>% filter(ERAlon, ERAlat)

MAP_FILES<- list.files(PATH_TO_DOWNLOAD_MAPS,
                       full.names = TRUE, 
                       recursive = T) %>% .[str_detect(., '.RDS')]

display.brewer.all()

REMOVE_WR_FILES<- FALSE
if(REMOVE_WR_FILES){
  WR_FILES<- list.files(PATH_TO_DOWNLOAD_MAPS ,
                        full.names = T, 
                        recursive = T) %>% 
    .[str_detect(., 'WR')]
  sapply(WR_FILES, file.remove)
  
}



for (i in 1: length(MAP_FILES)) {
  map.latlon<- readRDS(MAP_FILES[i])
  pmap2<-autoplot(map.latlon)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),legend.position="none",
                                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                     panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  for (anch in seq(0.01,0.1,0.01)) {
    p_ros<-WR_parameters2(data = DATOS_ROSA ,
                          spd_name = 'WS_N',
                          dir_name = 'WD_N', 
                          paleta = 'YlGnBu',
                          lon_pos = LON_ANEMOMETRO,
                          lat_pos = LAT_ANEMOMETRO,
                          anchura = anch)
    
    dev.off()
    pmap2+p_ros$subgrobs
    
    
    NOMBRE_MAPA<- MAP_FILES[i] %>% str_split('/') %>% .[[1]] %>% .[length(.)] %>% str_replace('.RDS', '_WR.png')
    PATH_MAPA<- MAP_FILES[i] %>% str_split('/') %>% .[[1]] %>% .[1:length(.)-1] %>% paste(collapse = '/')
    
    ggsave(paste0(PATH_MAPA,'/Anch_',anch,'_',NOMBRE_MAPA),
           device = "png", dpi=600,
           width =7, height =7, 
           units = 'in')
    
  }
}

