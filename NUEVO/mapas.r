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
n=1
Coord_era=Coord_era[1:n,]


#Seteamos el tamaño del mapa, para ello habrá que elegir 
n=max(c(Coord_era$lat),Coord_anemo$lat)    
s=min(c(Coord_era$lat),Coord_anemo$lat)    
e=max(c(Coord_era$lon),Coord_anemo$lon)    
w=min(c(Coord_era$lon),Coord_anemo$lon)    


#Fijamos incremento para hacer más grande el mapa
<<<<<<< HEAD
incr<- 0.0215
=======
incr<- 0.01
>>>>>>> f1247100009fce85c245c2cb5f8a2761d1474834

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
download_maps(ul,lr, res=40, maptyp = c("esri-topo","nps"))



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
prueba<- datos_rosa_dir$`247.5`
rango<- c(0:59)
prueba<-prueba[which(week(prueba$Date)==24),]
prueba$uv_dwi2<-cut(prueba$uv_dwi, 
                    breaks = c(0,seq(11.5,349.5,22.5),360), 
                    labels = c(as.numeric(names(datos_rosa_dir)),0)) 
prueba$uv_dwi2<- as.numeric(as.character(prueba$uv_dwi2)) 

vectores<- as.data.frame(cbind(prueba$Date,(-prueba$uv_dwi2+90)+180, (-prueba$Dir+90)+180))
colnames(vectores)<- c("Date","ERA","Anem")
vectores$Date<- prueba$Date
vectores<- vectores[seq(0,length(vectores[,1]), 2),]


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
  ylim(0,10.5)+theme(panel.background = element_blank(),
                    panel.border = element_rect(linetype = "dashed", fill = NA),
                    panel.grid.major = element_line(colour = "grey50"),
                    axis.text = element_text(size=20,face="bold"),
                    axis.title = element_text(size = 25, face="bold"))+
  labs(y= "Wind speed (m/s)",
       x= "")+
  geom_text(x=prueba$Date[55], y=10.3, 
            label=paste0("Cor= ",
                         round(cor(prueba$Mean, prueba$uv_wind),digits = 3)),
            size=10)

dir.path<- here::here("NUEVO/Mapas/")
ggsave(paste0(dir.path,'/',"cor_plot2.png"),
       device = "png", dpi=1200,
       width =14, height =7, 
       units = 'in')

