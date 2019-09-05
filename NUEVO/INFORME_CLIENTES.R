library(here)
source('NUEVO/Libraries.R')
library(rasterVis)
library(elevatr)

DATOS<- here::here("NUEVO/Data_calibracion/0B76C7C0FD4F/ERA5_2019-06-01.RDS") %>% readRDS()
PATH_TO_DOWNLOAD_MAPS<-  here::here('NUEVO/Mapas/0B76C7C0FD4F/')


# FIJAMOS COORDENADAS PARA LOS MAPAS -------------------------------------------------------------------


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


# CONSTRUIMOS CURVA DE POTENCIA DE AEROGENERADORES BORNAY -----------------
library(WindCurves)


WIND_TURBINE_CURVE<- function(VSTART, VNOM,VOFF,
                              PNOM, PPICO, RADIO){
  V<- c(VSTART, VNOM/2)
  P<- c(0.5*1.2*pi*RADIO^2*VSTART^3 * 0.3, 
        0.5*1.2*pi*RADIO^2*(VNOM/2)^3 * 0.5)
  
  Bornay<- data.frame(v= c(0,V,VNOM,(VNOM+VOFF)/2,VOFF),wp = c(0,P,PNOM,(PNOM+PPICO)/2,PPICO))
  
  AJUSTE_CURVA<- fitcurve(Bornay)
  
  INTER_LOG_CURVE<- spline(AJUSTE_CURVA$Speed,AJUSTE_CURVA$`Logistic Function`, n = 100)
  INTER_LOG_CURVE$WS <- INTER_LOG_CURVE$x
  INTER_LOG_CURVE$WP<- INTER_LOG_CURVE$y
  INTER_LOG_CURVE$x<- NULL
  INTER_LOG_CURVE$y<- NULL
  
  return(list(INTERP= INTER_LOG_CURVE, FITTED = AJUSTE_CURVA))
}


#AEROGENERADOR BORNAY +13
#RANGO 2/30 m/S
# ARRANQUE 3 m/s
#P NOMINAL 1500W (12 m/s)
#P NOMINAL 2500W (DICE 14 m/s para frenado)

DATOS_7MIN<- here::here("NUEVO/Data_calibracion/0B76C7C0FD4F/0B76C7C0FD4F_2019-08-13.RDS") %>% readRDS()

RADIO13 <- 2.86/2
VSTART13<- 3
VNOM13<- 12
VOFF13<- 30
PNOM13<- 1500
PPICO13<- 2500


RADIO25.2 <- 4.05/2
VSTART25.2<- 3
VNOM25.2<- 12
VOFF25.2<- 30
PNOM25.2<- 3000
PPICO25.2<- 4500

RADIO25.3 <- 4.05/2
VSTART25.3<- 3
VNOM25.3<- 12
VOFF25.3<- 30
PNOM25.3<- 5000
PPICO25.3<- 7500

CURVE13<- WIND_TURBINE_CURVE(VSTART = VSTART13,
                             VNOM = VNOM13,
                             VOFF = VOFF13, 
                             PNOM = PNOM13,
                             PPICO = PPICO13,
                             RADIO = RADIO13)

CURVE25.2<- WIND_TURBINE_CURVE(VSTART = VSTART25.2,
                             VNOM = VNOM25.2,
                             VOFF = VOFF25.2, 
                             PNOM = PNOM25.2,
                             PPICO = PPICO25.2,
                             RADIO = RADIO25.2)

CURVE25.3<- WIND_TURBINE_CURVE(VSTART = VSTART25.3,
                             VNOM = VNOM25.3,
                             VOFF = VOFF25.3, 
                             PNOM = PNOM25.3,
                             PPICO = PPICO25.3,
                             RADIO = RADIO25.3)


ggplot()+
  geom_line(aes(x=CURVE13$INTERP$WS, y= CURVE13$INTERP$WP), colour= "red")+
  geom_line(aes(x=CURVE25.2$INTERP$WS, y= CURVE25.2$INTERP$WP),colour= "green")+
  geom_line(aes(x=CURVE25.3$INTERP$WS, y= CURVE25.3$INTERP$WP), colour= "pink") + 
  geom_line(aes(x=CURVE13$FITTED$Speed, y= CURVE13$FITTED$Power), colour= "red", linetype = 'dashed')+
  geom_line(aes(x=CURVE25.2$FITTED$Speed, y= CURVE25.2$FITTED$Power),colour= "green", linetype = 'dashed')+
  geom_line(aes(x=CURVE25.3$FITTED$Speed, y= CURVE25.3$FITTED$Power), colour= "pink", linetype = 'dashed') + 
  theme_light()





DATOS_7MIN$BORN13<- DATOS_7MIN$WS_N %>% cut(CURVE13$INTERP$WS %>% round(1), 
                   labels= CURVE13$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()

DATOS_7MIN$BORN13_MAX<- DATOS_7MIN$WSMAX %>% cut(CURVE13$INTERP$WS %>% round(1), 
                                            labels= CURVE13$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()


DATOS_7MIN$BORN25.2<- DATOS_7MIN$WS_N %>% cut(CURVE25.2$INTERP$WS %>% round(1), 
                                            labels= CURVE25.2$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()

DATOS_7MIN$BORN25.2_MAX<- DATOS_7MIN$WSMAX %>% cut(CURVE25.2$INTERP$WS %>% round(1), 
                                                 labels= CURVE25.2$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()


DATOS_7MIN$BORN25.3<- DATOS_7MIN$WS_N %>% cut(CURVE25.3$INTERP$WS %>% round(1), 
                                            labels= CURVE25.3$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()

DATOS_7MIN$BORN25.3_MAX<- DATOS_7MIN$WSMAX %>% cut(CURVE25.3$INTERP$WS %>% round(1), 
                                                 labels= CURVE25.3$INTERP$WP[1:99]) %>%
  as.character()%>% as.numeric()




DATOS_7MIN<- DATOS_7MIN %>% .[complete.cases(.),]
DATOS_7MIN$DIFF_TIME<-lubridate::make_difftime(c(0,diff.difftime(DATOS_7MIN$Date)), units = 'mins')

DATOS_7MIN$DIFF_TIME<- ifelse(DATOS_7MIN$DIFF_TIME>0.15 , 0.15, DATOS_7MIN$DIFF_TIME)


DATOS_7MIN<- DATOS_7MIN %>% mutate(Emed_BORN13 = BORN13 * as.numeric(DIFF_TIME),
                      Emax_BORN13 = BORN13_MAX * as.numeric(DIFF_TIME),
                      Emix_BORN13 = (Emed_BORN13 + Emax_BORN13)/2)

DATOS_7MIN<- DATOS_7MIN %>% mutate(Emed_BORN25.2 = BORN25.2 * as.numeric(DIFF_TIME),
                                   Emax_BORN25.2 = BORN25.2_MAX * as.numeric(DIFF_TIME),
                                   Emix_BORN25.2 = (Emed_BORN25.2 + Emax_BORN25.2)/2)

DATOS_7MIN<- DATOS_7MIN %>% mutate(Emed_BORN25.3 = BORN25.3 * as.numeric(DIFF_TIME),
                                   Emax_BORN25.3 = BORN25.3_MAX * as.numeric(DIFF_TIME),
                                   Emix_BORN25.3 = (Emed_BORN25.3 + Emax_BORN25.3)/2)




DATOS_7MIN$Emix_BORN13 %>% sum()
DATOS_7MIN$Emix_BORN25.2 %>% sum()
DATOS_7MIN$Emix_BORN25.3 %>% sum()



