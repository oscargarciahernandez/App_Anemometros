library(here)
source(here::here("NUEVO/Libraries.R"))

# Calibración Cosas para el artículo --------------------------------------



### Rosa de los vienos ERA5 y anemometro
load(here::here("NUEVO/Data_calibracion/datos_uni.Rdata"))

datos_rosa<- datos_uni[,c("Date","lon","lat","uv_wind","uv_dwi","Mean","Dir")]
datos_rosa<- datos_rosa[complete.cases(datos_rosa),]
datos_rosa$Dir<- as.numeric(datos_rosa$Dir)

Paletas<- c("GnBu","YlGnBu")



map_folder<- find_mapfolder()

dir.path<- map_folder[6]

map_files<- list.files(dir.path, full.names = TRUE) %>% .[str_detect(., ".Rdata")]
nombre<- str_split(map_files,"/") %>% lapply(., function(x) return(x[length(x)])) %>% str_remove(., ".Rdata")


for (j in 1:length(map_files)) {
  load(file = map_files[j])
  pmap2<-autoplot(map.latlon)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),legend.position="none",
                                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                     panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  for (i in 1:length(Paletas)) {
    p_ros_ERA<- WR_parameters2(data = datos_rosa, 
                               anchura = 0.023,
                               spd_name ="uv_wind" ,
                               dir_name = "uv_dwi",
                               lon_pos = Coord_era$lon,
                               lat_pos =Coord_era$lat, 
                               paleta = Paletas[i],
                               opacidad = 0.5)
    
    p_ros_UNI<- WR_parameters2(data = datos_rosa, 
                               anchura = 0.02,
                               spd_name ="Mean" ,
                               dir_name = "Dir",
                               lon_pos = Coord_anemo$lon,
                               lat_pos =Coord_anemo$lat, 
                               paleta = Paletas[i],
                               opacidad = 0.5)
    
    pmap2 + p_ros_ERA$subgrobs + p_ros_UNI$subgrobs
    
    ggsave(paste0(dir.path,'/',Paletas[i],nombre[j],"_WR.png"),
           device = "png", dpi=800,
           width =7, height =7, 
           units = 'in')
    
    
  }
  
  
}



## separar por direcciones
#Separar por direcciones de anemos

datos_rosa_dir=list()
dirs=unique(datos_rosa$Dir)      #Que direcciones tenemos en el anemo?  #Quitar NA
dirs=as.numeric(dirs)           #ESTAN EN CHARACTER!  Cambiar a numerico para que se ordenen bien
dirs=dirs[order(dirs)]          #Ordenar de menor a mayor
for (i in 1:length(dirs)) {
  datos_rosa_dir[[i]]=datos_rosa[which(datos_rosa$Dir==dirs[i]),]
}
names(datos_rosa_dir)=dirs       #Los elementos de la lista se llamaran como la direcion que les corresponde

#Correlaciones por direcciones
cors_dir=data.frame()  #Aqui iran las correlaciones correspondientes a cada direccion
cors_dir[1:length(datos_rosa_dir),1]=names(datos_rosa_dir)  #Col 1=las direcciones
for (i in 1:length(datos_rosa_dir)) {  
  cors_dir[i,2]=cor(datos_rosa_dir[[i]]$uv_wind,datos_rosa_dir[[i]]$Mean,"na") #Col 2=las correlaciones
  cors_dir[i,3]=nrow(datos_rosa_dir[[i]])*100/sum(unlist(lapply(datos_rosa_dir,nrow)))  #Col 3=porcentaje de datos en esa direccion
}
colnames(cors_dir)=c("Dir","cor","%")

#Calculo factor K diferencia de modulo entre punto ERA y anemo
zo=3 #[m] Centers of cities with tall buildings - Manwell pag 46, tabla 2.2
z=155 + 3.5*6 + 1.5 #[m] Altura anemo = altitud segun google earth + altura edificio + altura poste anemo
zr=401 + 10 #[m] Altura era = altitud segun google earth + 10m
k=log(z/zo)/log(zr/zo)  #k=U(z)/U(zr)


#Cojo solo la dirección con la mejor correlación
#para gráficas para el artículo
ploteo_cor<- function(prueba, vectores){
  ggplot(data = prueba, aes(x=Date, y=uv_wind)) + 
    geom_line(size=1, color="blue",linetype="longdash", alpha=0.5)+
    geom_line(data=prueba, aes(y=uv_wind*k), size=1.2, color="blue")+
    geom_text(data=vectores, aes(x=Date,y=8,
                                 angle=ERA, label="→"), 
              color="blue", 
              alpha=0.5,
              size=15)+
    geom_line(aes(x=Date, y=Mean),size=1.2, color="red")+
    geom_text(data=vectores, 
              aes(x=Date,y=6,
                  angle=Anem, 
                  label="→"),
              color="red", 
              alpha=0.5,
              size=15)+
    ylim(0,20)+theme(panel.background = element_blank(),
                       panel.border = element_rect(linetype = "dashed", fill = NA),
                       panel.grid.major = element_line(colour = "grey50"),
                       axis.text = element_text(size=20,face="bold"),
                       axis.title = element_text(size = 25, face="bold"))+
    labs(y= "Wind speed (m/s)",
         x= "")+
    geom_text(x=prueba$Date[length(prueba$Date)-3], y=10.3, 
              label=paste0("Cor= ",
                           round(cor(prueba$Mean, prueba$uv_wind),digits = 3)),
              size=10)
  
}

prueba1<- datos_rosa_dir$`247.5`
vec_semanas<- unique(week(prueba1$Date))
for (i in 1:length(vec_semanas)) {
  prueba<-prueba1[which(week(prueba$Date)==vec_semanas[i]),]
  prueba$uv_dwi2<-cut(prueba$uv_dwi, 
                      breaks = c(0,seq(11.5,349.5,22.5),360), 
                      labels = c(as.numeric(names(datos_rosa_dir)),0)) 
  prueba$uv_dwi2<- as.numeric(as.character(prueba$uv_dwi2)) 
  
  vectores<- as.data.frame(cbind(prueba$Date,(-prueba$uv_dwi2+90)+180, (-prueba$Dir+90)+180))
  colnames(vectores)<- c("Date","ERA","Anem")
  vectores$Date<- prueba$Date
  vectores<- vectores[seq(0,length(vectores[,1]), 2),]
  
  ploteo_cor(prueba, vectores)
  
  dir.path<- here::here("NUEVO/Mapas/")
  ggsave(paste0(dir.path,'/',i,"cor_plot.png"),
         device = "png", dpi=200,
         width =14, height =7, 
         units = 'in')
  
  
}


#### A partir de aquí hacemos distribucion de weibull, para el artículo. 
datos_wei<-datos_rosa %>%
  mutate(grup_vel=cut(datos_rosa$uv_wind,
                      seq(0,max(datos_rosa$uv_wind),
                          by=0.5),
                      labels = seq(0.5,
                                   max(datos_rosa$uv_wind),
                                   by=0.5),
                      include.lowest = T,right = T))



wei1<- table(datos_wei$grup_vel)
wei2<-sum(wei1)
wei_per<- wei1/wei2
wei_an<- wei_per*8600
wei_an<- as.data.frame(wei_an)

ggplot(wei_an)+
  geom_bar(aes(x=as.numeric(row.names(wei_an)), y= Freq),
           stat = "identity",
           alpha=.95,fill='lightblue',
           color='lightblue4',
           show.legend = T)+
  geom_line(aes(y=dweibull(seq(0,10, len=length(wei_an[,1])), shape = k, scale = c)*8600))



k <- (sd(datos_rosa$Mean)/mean(datos_rosa$Mean))^(-1.086)
c <- mean(datos_rosa$Mean)/(gamma(1+1/k))
c


# SACANDO MEDIAS PARA EL ARTÍCULO -----------------------------------------

# COORDENADA MÁS CERCANA --------------------------------------------------
GENERAR_RDS_PUNTO_MC<-FALSE
if(GENERAR_RDS_PUNTO_MC){
  
  #IMPORTAR TABLA DE REGISTRO
  if (!exists("t_reg")) {
    t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
  }
  
  
  #SELECCIONAMOS ANEMOMETRO
  head(t_reg)
  ANEMO_SELECCIONADO<- 1
  name_ERA5_coord_anem<- paste0("ERA5_coord_",
                                str_replace_all(as.character(t_reg$ID[ANEMO_SELECCIONADO]), " ",""),
                                ".RDS")
  
  #SACAR COORDENADAS MÁS CERCANAS SI ES NECESARIO
  if (file.exists(paste0(here::here("NUEVO/Data_ERA5/"), name_ERA5_coord_anem))) {Coord_ERA5_anemo<- readRDS(paste0(here::here("NUEVO/Data_ERA5/"),
                                                                                                                    name_ERA5_coord_anem))
  }else{#SACAR LAS COORDENADAS ERA5 SI ES NECESARIO...CUIDADO CON EL UNIQUE(ERA5_DF)
    if (file.exists(here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))) {ER5_coord<- readRDS(here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))
    }else{
      ERA5_df<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))
      Coordenadas_era<- unique(ERA5_df[,c("lon","lat")])
      saveRDS(Coordenadas_era, file=here::here("NUEVO/Data_ERA5/ERA5_coord.RDS"))
    }
    
    
    #SACAMOS LAS COORDENADAS DEL ANEMO DE LA TABLA DE REGISTRO
    Coordenadas_anemos<- as.data.frame(cbind(as.numeric(sub(",",".",as.character(t_reg$lon))),
                                             as.numeric(sub(",",".",as.character(t_reg$lat)))))
    colnames(Coordenadas_anemos)=c("lon","lat")
    
    #Seleccionar anemo
    Coord_anemo<- Coordenadas_anemos[ANEMO_SELECCIONADO,]
    
    
    #Ordenarlos puntos del ERA de cercanos a lejanos
    Coord_era<- Coordenadas_era[order((Coordenadas_era$lon-Coord_anemo$lon)^2+(Coordenadas_era$lat-Coord_anemo$lat)^2),]
    
    
    #SELECCIONAMOS LOS N MÁS CERCANOS 
    n=1
    Coord_era=Coord_era[1:n,]
    Coord_ERA5_anemo<- Coord_era
    
    saveRDS(Coord_era, file=paste0(here::here("NUEVO/Data_ERA5/"),
                                   name_ERA5_coord_anem))
  }
  
}



####JUNTAMOS LOS DATOS DEL ANEMO

#CARGAMOS LOS DATOS DE ERA5 2018_2019
ERA5_df<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))


#CARGAMOS ANEMOS
#LOS CARGAMOS DESDE JUNTOS.RDS PORQUE ES DONDE EL SCRIPT CALIBRACIÓN A ENTRADO EN 
# JUEGO PASANDO LOS FILTROS PARA LIMPIAR LOS DATOS Y ADEMÁS SE HAN EQUIPARADO LOS DATOS 
# CON ERA5... HABLANDO DE RESOLUCIÓN TEMPORAL. 

DATOS_ANEMOMETRO_UNI<- readRDS(here::here('NUEVO/Data_calibracion/0B38DAE79059_juntos.rds')) %>% 
  .[,c(5,2,3,4)]
colnames(DATOS_ANEMOMETRO_UNI)<- c("Date", colnames(readRDS(here::here('NUEVO/Data_calibracion/0B38DAE79059_juntos.rds'))[,2:4]))

#CARGAMOS COORDENADAS DEL ANEMO
Coord_ERA5_anemo<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_coord_0B38DAE79059.RDS'))

#CORTAMOS DATOS ERA PARA EL PUNTO MAS CERCANO. MC
ERA5_df_MC<- ERA5_df %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) 

#JUNTAMOS LOS DATOS 
DATOS_JUNTOS<- left_join(DATOS_ANEMOMETRO_UNI, ERA5_df_MC, by="Date")


##### AÑADIMOS EL FACTOR K DEBIDO A LA ALTITUD

zo=3 #[m] Centers of cities with tall buildings - Manwell pag 46, tabla 2.2
z=155 + 3.5*6 + 1.5 #[m] Altura anemo = altitud segun google earth + altura edificio + altura poste anemo
zr=401 + 10 #[m] Altura era = altitud segun google earth + 10m
k=log(z/zo)/log(zr/zo)  #k=U(z)/U(zr)



#CALCULO DE MEDIAS
###HACIENDO MEDIA DE LOS DATOS DE LOS ANEMOS Y ERA_5 TENIENDO EN CUENTA 
#EL PERIODO DE SOLAPAMIENTO
HACER_MEDIAS<- FALSE

if(HACER_MEDIAS){
  DATOS_JUNTOS %>% .[complete.cases(.),] %>% summarise(Vmean=mean(Mean), Gust_mean=mean(Gust),
                                                       ERA5_mean=mean(uv_wind), ERA5_mean_K=mean(uv_wind*k))
  
  
  
  #########HACIENDO MEDIAS DEL HISTÓRICO DE ERA5
  RDS_HISTORICO_ERA<-list.files(here::here('NUEVO/Data_ERA5/'), full.names = T) %>% 
    .[1:40]
  
  ERA_mean<- vector()
  #ERA_mean_k<- vector()
  
  for (i in 1:length(RDS_HISTORICO_ERA) ) {
    ERA_mean[i]<- readRDS(RDS_HISTORICO_ERA[i]) %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) %>% 
      summarise(ERA_mean= mean(uv_wind, na.rm = T))
    #ERA_mean_k[i]<- readRDS(RDS_HISTORICO_ERA[i]) %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) %>%summarise(ERA_mean= mean(uv_wind*k, na.rm = T))
    
  }
  
  
  
}


# AQUÍ VIENE GGANIMATE!!!! ------------------------------------------------
#OKEY, PARA ESTA SECCIÓN VAMOS A UTILIZAR DATOS_JUNTOS.R QUE LO SACAMOS JUSTO EN 
# LA SECCIÓN ANTERIOR HE INTENTAR SACAR UN GIF CON UN VECTOR DE DIRECCIÓN. 
# INCLUYENDO A PODER SER MAPAS Y ROLLOS 


head(DATOS_JUNTOS)
#UNO DE LOS PROBLEMILLAS QUE LE VEO ES QUE DIR Y DIR_UV_DWI NO SE QUIEREN LLEVAR MUY BIEN... 
# HABRÁ QUE EQUIPARARLOS PRIMERO... ANTES DE INTENTAR NADA...


labels_dir<- unique(DATOS_JUNTOS$Dir) %>% .[order(as.numeric(.))] %>% 
  .[1:(length(.)-1)] %>% c(., "0")


DATOS_JUNTOS$Dir_ERA<- cut(DATOS_JUNTOS$uv_dwi,
            breaks = c(0,seq(11.25,360,by=22.50),361),
            labels = labels_dir)


#AHORA QUE YA TENEMOS LOS DATOS EKIPARADOS VAMOS A POR GGANIMATE
#install.packages("gganimate")
library(gganimate)


#EN EL SCRIPT MAPS.R SE DESCARGAN LOS MAPAS AQUÍ, LOS BUSCAMOS PARA REPRESENTAR ENCIMA
# LOS VECTORES
mapfile<- find_mapfolder() %>% list.files(full.names = T) %>% .[1] %>% readRDS()

#IMPORTAR TABLA DE REGISTRO PARA OBTENER LAS COORDENADAS DDE NUESTRO ANEMO
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}

Coord_anemo<- t_reg[1,c("lon","lat")]

DATOS_JUNTOS$lon_anem<- as.numeric(as.character(Coord_anemo$lon) %>% str_replace(",","."))
DATOS_JUNTOS$lat_anem<- as.numeric(as.character(Coord_anemo$lat) %>% str_replace(",","."))
DATOS_JUNTOS<- DATOS_JUNTOS[complete.cases(DATOS_JUNTOS), ]

DATOS_JUNTOS$Dir<- as.numeric(as.character(DATOS_JUNTOS$Dir))
DATOS_JUNTOS$Dir_ERA<- as.numeric(as.character(DATOS_JUNTOS$Dir_ERA))

### FIJAR PERIODO 
FECHA_INI<-ymd("2019/01/01")
FECHA_FIN<- ymd("2019/01/05")



DATOS_JUNTOS1<- DATOS_JUNTOS[DATOS_JUNTOS$Date>FECHA_INI&DATOS_JUNTOS$Date<FECHA_FIN,]

FIXED_MODULE<- 0.01
MODULE_MULTIPLIER<- 0.0025
ARROW_LENGTH<- 0.5 # NO LE HARÍA MUCHO CASO
ARROW_SIZE<- 1.5

############# A PARTIR DE AQUI DIFERENTES METODOS PARA GENERAR LOS VECTORES
rng<- range(DATOS_JUNTOS1$uv_wind,DATOS_JUNTOS1$Mean)

#VECTORES CON MODULO FIJO
animate_dir_fix<- autoplot(mapfile)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon, y=lat, angle=(((-Dir_ERA)+90)-180)*pi/180,
                                    radius=FIXED_MODULE,
                                    colour=uv_wind),
             arrow= arrow(ends = "last",  
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon_anem, y=lat_anem,
                                    angle=(((-Dir)+90)-180)*pi/180,
                                    radius=FIXED_MODULE,
                                    colour=Mean),
             arrow= arrow(ends = "last",
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  scale_colour_gradient2(low="aquamarine", mid="green", high="firebrick", #colors in the scale
                         midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                         breaks=seq(0,max(rng),0.25), #breaks in the scale bar
                         limits=c(floor(rng[1]), ceiling(rng[2])))
## MODULO VARIABLE 
animate_dir_var<- autoplot(mapfile)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon, y=lat, angle=(((-Dir_ERA)+90)-180)*pi/180,
                                    radius=uv_wind*MODULE_MULTIPLIER*k,
                                    colour=uv_wind),
             arrow= arrow(ends = "last",  
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon_anem, y=lat_anem,
                                    angle=(((-Dir)+90)-180)*pi/180,
                                    radius=Mean*MODULE_MULTIPLIER,colour=Mean),
             arrow= arrow(ends = "last",
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  scale_colour_gradient2(low="aquamarine", mid="green", high="firebrick", #colors in the scale
                         midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                         breaks=seq(0,max(rng),0.25), #breaks in the scale bar
                         limits=c(floor(rng[1]), ceiling(rng[2])))

## MUDULO VARIABLE CON UNA PEQUEÑA PARTE VARIABLE 
FIXED_MODULE2<- 0.002
MULTIPLIER_MODULE<- 0.0007

animate_dir_fix_var<- autoplot(mapfile)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon, y=lat, angle=(((-Dir_ERA)+90)-180)*pi/180,
                                    radius=FIXED_MODULE2+MULTIPLIER_MODULE*uv_wind,
                                    colour=uv_wind),
             arrow= arrow(ends = "last",  
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  geom_spoke(data=DATOS_JUNTOS1,aes(x=lon_anem, y=lat_anem,
                                    angle=(((-Dir)+90)-180)*pi/180,
                                    radius=FIXED_MODULE2+MULTIPLIER_MODULE*Mean,
                                    colour=Mean),
             arrow= arrow(ends = "last",
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  scale_colour_gradient2(low="aquamarine", high="firebrick", #colors in the scale
                         midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                         breaks=seq(0,max(rng),0.25), #breaks in the scale bar
                         limits=c(floor(rng[1]), ceiling(rng[2])))






########SELECCIONAR TIPO DE MODULO 

ANIMATE_DIRCLEAN<- animate_dir_fix_var+theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())+
  labs(title ="Date: {as.Date(frame_along)}") +
  shadow_wake(alpha=0.01, size=0.8,wake_length = 1)+
  transition_reveal(Date)




animate(ANIMATE_DIRCLEAN, fps=10,nframes = nrow(DATOS_JUNTOS1))

GUARDAR<- FALSE

if(GUARDAR){
  path_animaciones<- here::here('GGanimate/')
  if(!dir.exists(path_animaciones)){dir.create(path_animaciones)}
  anim_save(paste0(path_animaciones,'ERA5_ANM_VEC_CM_sat.gif'))
}
    


##### GENERAR ROSA DE LOS VIENTOS PARA CADA LOCALIZACION 
# HAY QUE USAR ROSA$SUBGROBS.... IMPORTANTE 

ROSA_UNI<- WR_parameters2(data = DATOS_JUNTOS, 
               anchura = 0.02,
               spd_name ="Mean" ,
               dir_name = "Dir",
               lon_pos = DATOS_JUNTOS1$lon_anem[1],
               lat_pos =DATOS_JUNTOS1$lat_anem[1], 
               paleta = "YlGnBu",
               opacidad = 0.5, 
               border_size = 0.01)

ROSA_ERA<- WR_parameters2(data = DATOS_JUNTOS, 
                          anchura = 0.02,
                          spd_name ="uv_wind" ,
                          dir_name = "Dir_ERA",
                          lon_pos = DATOS_JUNTOS1$lon[1],
                          lat_pos =DATOS_JUNTOS1$lat[1], 
                          paleta = "YlGnBu",
                          opacidad = 0.5,
                          border_size = 0.01)


MAPA_CON_ROSA<- autoplot(mapfile)+ROSA_ERA$subgrobs+ROSA_UNI$subgrobs
#MAPA_CON_ROSA




FIXED_MODULE2<- 0.008
MULTIPLIER_MODULE<- 0.0007

animate_dir_fix_var<- MAPA_CON_ROSA +
  geom_spoke(data=DATOS_JUNTOS,aes(x=lon, y=lat, angle=(((-Dir_ERA)+90)-180)*pi/180,
                                    radius=FIXED_MODULE2+MULTIPLIER_MODULE*uv_wind,
                                    colour=uv_wind),
             arrow= arrow(ends = "last",  
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  geom_spoke(data=DATOS_JUNTOS,aes(x=lon_anem, y=lat_anem,
                                    angle=(((-Dir)+90)-180)*pi/180,
                                    radius=FIXED_MODULE2+MULTIPLIER_MODULE*Mean,
                                    colour=Mean),
             arrow= arrow(ends = "last",
                          length = unit(ARROW_LENGTH, "cm")),
             size=ARROW_SIZE)+
  scale_colour_gradient2(low="aquamarine", high="firebrick", #colors in the scale
                         midpoint=mean(rng),    #same midpoint for plots (mean of the range)
                         breaks=seq(0,max(rng),0.25), #breaks in the scale bar
                         limits=c(floor(rng[1]), ceiling(rng[2])))






########ANIMAMOS
ANIMATE_DIRCLEAN<- animate_dir_fix_var+theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                             axis.title.x=element_blank(),
                                             axis.title.y=element_blank(),legend.position="none",
                                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                             panel.grid.minor=element_blank(),plot.background=element_blank())+
  labs(title ="Date: {as.Date(frame_along)}") +
  shadow_mark(alpha=0.9, size=0.8)+
  transition_reveal(Date)+
  ease_aes('cubic-in-out')




animate(ANIMATE_DIRCLEAN, fps=15,
        nframes = nrow(DATOS_JUNTOS),
        renderer = ffmpeg_renderer())
