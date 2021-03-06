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

#Importar a crear mapa con alta resolución... 40 minNumtiles y guardarlo
#Tarda mogollón en hacerlo... asi que mejor guardarlo
while(TRUE){
  if(dir.exists(here::here("NUEVO/Mapas"))){
    if (!file.exists(here::here("NUEVO/Mapas/mapa1.Rdata"))) {
      map <- openmap(ul,lr, minNumTiles=40,
                     type='bing',
                     zoom=NULL)  
      map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      save(map.latlon, file=here::here("NUEVO/Mapas/mapa1.Rdata"))
    }else{
      load(here::here("NUEVO/Mapas/mapa1.Rdata"))
      break}
    
  }else{
    dir.create(here::here("NUEVO/Mapas"))
  }
  
}



pmap<-autoplot(map.latlon)+
  geom_point(data = Coord_era, aes(lon,lat), shape=23,
             size=3, fill= "blue",colour = "black")+
  geom_point(data = Coord_anemo, aes(lon,lat),shape=21,
             size=3, colour="black", fill="red")



pmap2<- pmap + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

prueba<- ERA5_df[which(ERA5_df$lon%in%Coord_era$lon & ERA5_df$lat%in%Coord_era$lat),]

source(here::here('windrose_sin_nada.R'))




prueba<- ERA5_df[which(ERA5_df$lon%in%Coord_era$lon & ERA5_df$lat%in%Coord_era$lat),]


p_ros<- prueba%>%group_by(., lon,lat)%>% do(subplots= plot.windrose(., spd = "uv_wind",
                                                                    dir="uv_dwi",
                                                                    dirres = 22.5,
                                                                    spdseq= c(0,0.3,1,2,3,5,7,10,15,20),
                                                                    palette = "PuRd"))%>%
  mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                           x = lon-0.07,      # change from 1 to other 
                                           y = lat-0.07,      # values if necessary,
                                           xmax = lon+0.07,   # depending on the map's
                                           ymax = lat+0.07))) # resolution.

#PLOTEAR Y GUARDAR
#pmap2+p_ros$subgrobs
  
#ggsave(paste0(here::here("NUEVO/Mapas//"),"mapaprueba.tiff"), device = "tiff", dpi=1200,width =7, height =7, units = 'in')
  
  

# Mejorando calidad de las gráficas ---------------------------------------

 

WR_parameters<- function(data,anchura=0.06, opacidad=0.5, paleta){
  p_ros<- data%>%group_by(., lon,lat)%>% do(subplots= plot.windrose(., spd = "uv_wind",
                                                                      dir="uv_dwi",
                                                                      dirres = 22.5,
                                                                      spdseq= c(0,0.3,1,2,3,5,7,10,15,20),
                                                                      palette = paleta,
                                                                      opacity = opacidad))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = lon-anchura,      # change from 1 to other 
                                             y = lat-anchura,      # values if necessary,
                                             xmax = lon+anchura,   # depending on the map's
                                             ymax = lat+anchura))) # resolution.
  
  
  return(p_ros)
}

## Para la prueba plotearemos solamente 1 rosa. La central.
prueba_1<- prueba[which(prueba$lon==(-2.5) & round(prueba$lat, digits = 1)== 43.2),]




######Cambiando paletas

#hay más paletas pero estas son las secuenciales...
#ideales para representar valores progresivos
# las otras paletas estan preparadas para representar mediante colores
#diferentes categorías o señalar valores críticos en el mid-range

Paletas<- c("Blues", "BuGn", "BuPu", "GnBu" , "Greys", "Oranges", "OrRd", 
            "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", 
            "YlGnBu" ,"YlOrBr" ,"YlOrRd")

Paletas_div<- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu","RdGy","RdYlBu",
                "RdYlGn", "Spectral")

for (i in 1:length(Paletas)) {
  p_ros<- WR_parameters(data = prueba_1, paleta = Paletas[i])
  pmap2+p_ros$subgrobs
  
  
  
  if(dir.exists(here::here('NUEVO/Mapas/Paleta'))){
    ggsave(paste0(here::here("NUEVO/Mapas/Paleta//"),"paleta",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }else{
    dir.create(here::here('NUEVO/Mapas/Paleta'))
    ggsave(paste0(here::here("NUEVO/Mapas/Paleta//"),"paleta",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
  }
  
  
}

for (i in 1:length(Paletas_div)) {
  p_ros<- WR_parameters(data = prueba_1, paleta = Paletas_div[i])
  pmap2+p_ros$subgrobs
  
  ggsave(paste0(here::here("NUEVO/Mapas//"),"paleta_div",i,".tiff"),
         device = "tiff", dpi=200,
         width =7, height =7, 
         units = 'in')
  
  
}

######Cambiando opacidad
opacidad_vec<- seq(0,1, 0.05)

for (i in 1:length(opacidad_vec)) {
  p_ros<- WR_parameters(data = prueba_1, opacidad = opacidad_vec[i], paleta = Paletas[1])
  pmap2+p_ros$subgrobs
  
  if(dir.exists(here::here('NUEVO/Mapas/Opacidad'))){
    ggsave(paste0(here::here("NUEVO/Mapas/Opacidad//"),"opacidad",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }else{
    dir.create(here::here('NUEVO/Mapas/Opacidad'))
    ggsave(paste0(here::here("NUEVO/Mapas/Opacidad//"),"opacidad",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }

  
  
}


######Cambiando opacidad
anchura_vec<- seq(0.05,0.1, 0.005)

for (i in 1:length(opacidad_vec)) {
  p_ros<- WR_parameters(data = prueba, anchura = anchura_vec[i], paleta = Paletas[1] )
  pmap2+p_ros$subgrobs
  
  
  if(dir.exists(here::here('NUEVO/Mapas/anchura'))){
    ggsave(paste0(here::here("NUEVO/Mapas/anchura//"),"anchura",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }else{
    dir.create(here::here('NUEVO/Mapas/anchura'))
    ggsave(paste0(here::here("NUEVO/Mapas/anchura//"),"anchura",i,".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }
  
  
}




# Descargar todos los tipos de mapas con buena calidad --------------------

maptypes<- c("osm", "osm-bw",
  "maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain",
  "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri",
  "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap",
  "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")

for (i in 1:length(maptypes)) {
  if(file.exists(here::here(paste0("NUEVO/Mapas/",
                                   maptypes[i],".Rdata")))){}else{
    
   tryCatch({
      map<- openmap(ul,lr, minNumTiles=40,
                         type=maptypes[i],
                         zoom=NULL)
    map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    save(map.latlon, file=here::here(paste0("NUEVO/Mapas/",maptypes[i],".Rdata")))
   }, error=function(e){})
                                   }
}

  


# Probar diferentes mapas con los puntos ----------------------------------
maps<- list.files(here::here('NUEVO/Mapas/')) %>% .[str_detect(.,".Rdata")]

for(i in 1:length(maps)){
  #al ejecutar load, se carga un objeto llamado map.latlon
  load(here::here(paste0("NUEVO/Mapas/",maps[i])))
  pmap_types<-autoplot(map.latlon)+
    geom_point(data = Coord_era, aes(lon,lat), 
               size=3, colour = "white", alpha=0.7)+
    geom_point(data = Coord_anemo, aes(lon,lat),
               size=3, colour="red",alpha=0.7)
  if(dir.exists(here::here('NUEVO/Mapas/diff_type'))){
    ggsave(paste0(here::here("NUEVO/Mapas/diff_type//"),maps[i],".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }else{
    dir.create(here::here('NUEVO/Mapas/diff_type'))
    ggsave(paste0(here::here("NUEVO/Mapas/diff_type//"),maps[i],".tiff"),
           device = "tiff", dpi=200,
           width =7, height =7, 
           units = 'in')
    
  }
  
  
}




# Representar vectores ----------------------------------------------------


ggplot(data=prueba_1, aes(x=lon, y=lat)) + 
  geom_spoke( aes(angle=uv_dwi*pi/180,radius=uv_wind), 
              arrow = arrow(ends = "last", length = unit(0.2, "cm")))





# haciendo funciones ------------------------------------------------------
map_wpoints<- function(map.latlon, Coord_era,Coord_anemo){
  pmap<-autoplot(map.latlon)+
    geom_point(data = Coord_era, aes(lon,lat), shape=23,
               size=3, fill= "blue",colour = "black")+
    geom_point(data = Coord_anemo, aes(lon,lat),shape=21,
               size=3, colour="black", fill="red")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  print(pmap)
  return(pmap)
  
}



download_maps<- function(ul,lr,new_folder=TRUE, maptyp=NULL,res=40){
  if(is.character(maptyp)){
    maptypes<- maptyp
  }
  else{
    maptypes<- c("osm", "osm-bw",
                 "maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain",
                 "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri",
                 "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap",
                 "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")
    }
  if(length(maptypes)>1){
    for (i in 1:length(maptypes)) {
                                       
                                       tryCatch({
                                         map<- openmap(ul,lr, minNumTiles=res,
                                                       type=maptypes[i],
                                                       zoom=NULL)
                                         map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
                                         if(new_folder==TRUE){
                                           dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
                                           if(dir.exists(dirpath)){
                                             save(map.latlon, file=paste0(dirpath,"/",maptypes[i],res,".Rdata"))
                                             
                                           }else{
                                             dir.create(dirpath)
                                             save(map.latlon, file=paste0(dirpath,"/",maptypes[i],res,".Rdata"))
                                             
                                           }
                                           
                                           
                                         }else{save(map.latlon, file=here::here(paste0("NUEVO/Mapas/",maptypes[i],".Rdata")))
                                         }
                                       }, error=function(e){})
                                     }
  
  }
  else {
    
      map<- openmap(ul,lr, minNumTiles=res,
                    type=maptypes,
                    zoom=NULL)
      map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      if(new_folder==TRUE){
        dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
        dir.create(dirpath)
        save(map.latlon, file=paste0(dirpath,"/",maptypes,res,".Rdata"))
        
        
      }else{
        if(dir.exists(here::here(paste0("NUEVO/Mapas/")))){
          save(map.latlon, file=here::here(paste0("NUEVO/Mapas/",maptypes,"_",ul,lr,".Rdata")))
          
          
        }else{
          dir.create(here::here(paste0("NUEVO/Mapas/")))
          save(map.latlon, file=here::here(paste0("NUEVO/Mapas/",maptypes,"_",ul,lr,".Rdata")))
          
        }
        
      }
    
  }

}



##Rosas de los vientos para logo anemoi

for (i in seq(0,1,0.1)) {
  
    plot.windrose(prueba_1, spd = "uv_wind",
                  dir="uv_dwi",
                  dirres = 22.5,
                  spdseq= c(0,0.3,1,1.5,2,2.5,3,4,5,7,9,11,15,20),
                  palette = Paletas[4],
                  opacity = 1,
                  border_color = "white",
                  border_size = i)
    
    ggsave(paste0(here::here(paste0("NUEVO/Mapas/paleta4",i,".jpg"))),
           device = "jpg", dpi=400,
           width =7, height =7, 
           units = 'in')
    
  
  
  
}
