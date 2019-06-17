library(here)
source(here::here("NUEVO/Libraries.R"))



# IMPORTAR DATOS DE ANEMOMETROS Y ERA5 ------------------------------------
ERA5_df<- readRDS(here::here("NUEVO/Data_ERA5/ERA5_df.RDS"))
Anemometros<- readRDS(here::here('NUEVO/Data_anemometros/Anemometros.RDS'))

# IMPORTAMOS TABLA DE REGISTRO 
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}


# ACONDICIONAMOS DATOS DE ANEMOMETROS -------------------------------------

#ENSAÑAMOS LOS ANEMOMETROS DISPONIBLES Y ELEGIMOS UNO
levels(t_reg$ID)  

anemo_elegido="0B75FE3A4FB6"

#RELLENAMOS HUECOS
datos_anemos=rellenar_huecos_anemos(Anemometros[[which(names(Anemometros)==as.character(anemo_elegido))]])

#ORDENAMOS CRONOLÓGICAMENTE
datos_anemos=datos_anemos[order(datos_anemos$Date),]

#FILTRAMOS DATOS ANEMOS
#ME DEVULVE ERROR PORQUE NECESITA QUE LA COLUMNA DIR SEA CHARACTER??? 

datos_anemos$Dir<-as.character(datos_anemos$Dir)
lista_pos_errores=filtrar_datos(datos_anemos,desglosar = TRUE)




N_errores=unique(c(lista_pos_errores$N1_mean,
                   lista_pos_errores$N2_mean,
                   lista_pos_errores$N3_mean,
                   lista_pos_errores$N1_gust,
                   lista_pos_errores$N2_gust,
                   lista_pos_errores$N3_gust))


#DONDE ESTÁN LOS NA's
N_na=which(rowSums(is.na(datos_anemos[,c(2,3,4)]))>0)





#Llenar de NAs las mediciones consideradas erroneas. No eliminamos la fila; queremos mantener la fechas de los NAs.
datos_anemos[N_errores,c(2,3,4)]=NA
datos_anemos[N_na,c(2,3,4)]=NA    #Esto parece redundante pero viene bien asegurarse



#ELIMINAMOS LOS VALORES QUE ESTÉN POR DEBAJO DE FECHA_INI DE LA TABLA DE REGISTRO
datos_anemos=datos_anemos[-(1:which(datos_anemos$Date < filter(t_reg,ID==anemo_elegido) %>% 
                                      select(Fecha_ini) %>% .[1,] %>% 
                                      as.character()%>%
                                      dmy())),]

#PONEMOS EN FORMATO LOS DATOS DE LOS ANEMOMETROSE
#CARGAMOS COORDENADAS ANEMOS Y ELIMINAMOS LAS COMAS
Coordenadas_anemos=filter(t_reg,ID==anemo_elegido) %>% select(lon,lat)  #Primero lon, luego lat
Coordenadas_anemos$lon<- Coordenadas_anemos$lon %>% as.character() %>% str_replace(",",".") %>% as.numeric()
Coordenadas_anemos$lat<- Coordenadas_anemos$lat %>% as.character() %>% str_replace(",",".") %>% as.numeric()

#datos_anemos$Date<- datos_anemos$Date %>% round_date(unit="hour")
#datos_anemos$Gust<- NULL
datos_anemos$Dir<- datos_anemos$Dir %>% as.numeric()


datos_anemos$lon<- rep(Coordenadas_anemos$lon, nrow(datos_anemos))
datos_anemos$lat<- rep(Coordenadas_anemos$lat, nrow(datos_anemos))

#CREAMOS ANEMOS TABLA PARA NO JODER DATOS ANEMOS
ANEMOS_TABLA<- datos_anemos[,c(1,5,6,2,4,3)]
colnames(ANEMOS_TABLA)<-c("Date","lon","lat", "WS", "WD","WSMAX")

#CREAMOS TABLA HORARIA EN LOS ANEMOMETROS

VEC_DATE<- seq(min(ANEMOS_TABLA$Date) %>% round_date(unit = "hour") ,
    max(ANEMOS_TABLA$Date) %>% round_date(unit = "hour"), 
    by="hour") %>% c(ANEMOS_TABLA$Date, .) %>% .[order(.)] %>% 
  as.data.frame() %>% as.tbl() %>% .[!duplicated(.),]
colnames(VEC_DATE)<- "Date"



###
library(tidyverse)
ANEMOS_TABLA<- ANEMOS_TABLA[!duplicated(ANEMOS_TABLA$Date),]

ANEMOS_TABLA_ADDROWS<- left_join(VEC_DATE,
                                 ANEMOS_TABLA,
                                 by="Date") %>% 
  replace_na(list(lon = ANEMOS_TABLA$lon %>% unique(),
                  lat= ANEMOS_TABLA$lat %>% unique(),
                  id= ANEMOS_TABLA$id %>% unique()))

 

#IMPUTE TS PARA INTERPOLAR... SE PUEDE HACER "linear", "spline", "stine"
library(imputeTS)
#FUNCIÓN PARA RELLENAR CON EL VALOR NON-NA MAS CERCANO
REPLACE_NNONA <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

ANEMOS_TABLA_ADDROWS<-ANEMOS_TABLA_ADDROWS  %>% mutate(WS_l= na.interpolation(.$WS, "linear"),
         WS_sp= na.interpolation(.$WS, "spline"),
         WS_st= na.interpolation(.$WS, "stine"),
         WSMAX_l= na.interpolation(.$WSMAX, "linear"),
         WSMAX_sp= na.interpolation(.$WSMAX, "spline"),
         WSMAX_st= na.interpolation(.$WSMAX, "stine"),
         WS_N=REPLACE_NNONA(.$WS),
         WSMAX_N=REPLACE_NNONA(.$WSMAX),
         WD_N=REPLACE_NNONA(.$WD))




#GUARDAMOS DATOS ANEMOS LIMPIOS 
path_anemo<- here::here('NUEVO/Data_calibracion/') %>% paste0(anemo_elegido)
ifelse(dir.exists(path_anemo),NA, dir.create(path_anemo,recursive = T))

#GUARDAMOS EL ARCHIVO CON EL IDENTIFICADOR DEL ANEMOMETRO Y CON LA ULTIMA FECHA CONTENIDA EN 
# LA TABLA
nombre_archivo<- paste0(anemo_elegido,"_",
                        ANEMOS_TABLA$Date %>% max() %>% 
                          str_split(" ") %>% .[[1]] %>% .[1], ".RDS")
saveRDS(ANEMOS_TABLA_ADDROWS,paste0(path_anemo, "/",nombre_archivo) )





# ACONDICIONAMOS ERA5 -----------------------------------------------------


#TODAS LAS LOCALIZACIONES DE ERA5
if (!exists("Coordenadas_era")) {
  if(file.exists(here::here('NUEVO/Data_ERA5/ERA5_coord.RDS'))){
    Coordenadas_era<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_coord.RDS'))
  }else{
    Coordenadas_era=unique(ERA5_df[,c(2,3)])
    Coordenadas_era=arrange(Coordenadas_era,lon,lat)  #Asegurarse orden correcto para distm
    saveRDS(Coordenadas_era, here::here('NUEVO/Data_ERA5/ERA5_coord.RDS'))
  }
}

#COJEMOS LOS N PUNTOS DE ERA MÁS CERCANOS
n=9
Coordenadas_era_cercanas=Coordenadas_era[order(distm(Coordenadas_era, 
                                                     Coordenadas_anemos, 
                                                     fun = distHaversine)[,1]),] %>% 
  .[1:n,]

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
ERA5_df_CERCANOS=filter(ERA5_df,
                        lat %in% Coordenadas_era_cercanas$lat,
                        lon %in% Coordenadas_era_cercanas$lon) %>% 
  select(Date,"lon",lat,"uv_wind",uv_dwi)
colnames(ERA5_df_CERCANOS)<- c("Date", "ERAlon",
                               "ERAlat","ERAWS","ERAWD")


#LEY LOGARITMICA PARA MODIFICAR LA ALTURA DEL PUNTO DE ERA5
zo=3 # RUGOSIDAD
z=155 + 3.5*6 + 1.5 #ALTURA ANEMO
zr=401 + 10 #ALTURA ERA
k=log(z/zo)/log(zr/zo)  #FACTOR K

ERA5_df_CERCANOS$ERAWS<- ERA5_df_CERCANOS$ERAWS*k


# JUNTAMOS DATOS DE ERA5 Y ANEMOMETROS ------------------------------------

DATOS_JUNTOS<- inner_join(ERA5_df_CERCANOS,
                          ANEMOS_TABLA_ADDROWS, by="Date")
DATOS_JUNTOS[,c("WS","WD","WSMAX")]<- NULL

DATOS_JUNTOS_LISTA<- DATOS_JUNTOS %>% group_split(ERAlon,ERAlat)

#object.size(DATOS_JUNTOS)-object.size(DATOS_JUNTOS_LISTA)
#-35424bytes 

#ES POCA DIFERENCIA PERO OKUPA MENOS EN FORMATO TALBLA... Y SIEMPRE SE PUEDE CONVERTIR 
# A LISTA DE UNA MANERA RÁPIDA Y SENCILLA 
path_anemo<- here::here('NUEVO/Data_calibracion/') %>% paste0(anemo_elegido)
nombre_archivo<- paste0("ERA5_",
                        DATOS_JUNTOS$Date %>% max() %>% 
                          str_split(" ") %>% .[[1]] %>% .[1], ".RDS")
saveRDS(DATOS_JUNTOS,paste0(path_anemo, "/",nombre_archivo) )




# A PARTIR DE AQUÍ CALIBRACION  -------------------------------------------

DATA_FOLDERS<- list.dirs(here::here('NUEVO/Data_calibracion/'), recursive = F)

DATOS_JUNTOS<- DATA_FOLDERS[1] %>% list.files(full.names = T) %>% 
  .[str_detect(., "ERA5")] %>% readRDS()


DATOS_JUNTOS_LISTA<- DATOS_JUNTOS %>% group_split(ERAlon,ERAlat)
#PLOTEOS PARA COMPROBAR SIMILITUD ENTRE ERA5 Y ANEMOMETRO

'
DATOS_JUNTOS_LISTA[[1]] %>%
.[which(month(.$Date)==8),] %>%  
ggplot( aes(x=Date)) + 
  geom_line(aes(y=ERAWS)) + 
  geom_line(aes(y=WS_N), col="red")+
  ylab("Wind Speed [m/s]")+
  theme_light()

DATOS_JUNTOS_LISTA[[1]] %>% 
.[which(month(.$Date)==8 & day(.$Date)>15),] %>% 
ggplot( aes(x=Date)) + 
  geom_line(aes(y=WS_st), col="green") +
  geom_line(aes(y=WS_l), col="blue") + 
  geom_line(aes(y=WS_N), col="red")+
  ylab("Wind Speed [m/s]")+
  theme_light()
'

Tabla_cor<- DATOS_JUNTOS_LISTA %>% lapply(function(y){
  x<- y %>%  .[complete.cases(.),]
  Tabla_cor<- cbind(cor(x$ERAWS, x$WS_l),
  cor(x$ERAWS, x$WS_st),
  cor(x$ERAWS, x$WS_N),
  cor(x$ERAWS, x$WS_sp)) %>% as.data.frame()
  colnames(Tabla_cor)<- c("linear", "stine",
                          "nearest", "spline")
  
  return(Tabla_cor)
  
}) %>% bind_rows()

rownames(Tabla_cor)<- sapply(DATOS_JUNTOS_LISTA, function(x){
  paste(c(x$ERAlon %>% unique() %>% round(2),
          x$ERAlat %>% unique()) %>% round(2),
        collapse = "_") %>% 
    str_replace_all("-","m")
})

#MEDIAS DE CORRELACIÓN
Tabla_cor %>% summarise_all(mean)


SMA_table<- DATOS_JUNTOS_LISTA %>% lapply(function(x) x %>% .[complete.cases(.),] %>%
                                            mutate(SMA_l=  SMA(WS_l, n = 3),
                                                   SMA_N=  SMA(WS_N, n = 3),
                                                   SMA_ERA=  SMA(ERAWS, n = 3)))

Tabla_cor_SMA<- SMA_table %>% lapply(function(y){
  x<- y %>%  .[complete.cases(.),]
  Tabla_cor<- cbind(cor(x$SMA_ERA, x$SMA_l),
                    cor(x$SMA_ERA, x$SMA_N))%>% 
    as.data.frame()
  colnames(Tabla_cor)<- c("linear",
                          "nearest")
  
  return(Tabla_cor)
  
}) %>% bind_rows()
rownames(Tabla_cor_SMA)<- sapply(DATOS_JUNTOS_LISTA, function(x){
  paste(c(x$ERAlon %>% unique() %>% round(2),
          x$ERAlat %>% unique()) %>% round(2),
        collapse = "_") %>% 
    str_replace_all("-","m")
})
Tabla_cor_SMA %>% summarise_all(mean)

# TAYLOR DIAGRAMS ---------------------------------------------------------

library(plotrix)

DATA_FOLDERS<- list.dirs(here::here('NUEVO/Data_calibracion/'), recursive = F)

DATOS_JUNTOS<- DATA_FOLDERS[1] %>% list.files(full.names = T) %>% 
  .[str_detect(., "ERA5")] %>% readRDS()


DATOS_JUNTOS_LISTA<- DATOS_JUNTOS %>% group_split(ERAlon,ERAlat)
#PLOTEOS PARA COMPROBAR SIMILITUD ENTRE ERA5 Y ANEMOMETRO

Tabla_dist<- DATOS_JUNTOS_LISTA %>% lapply(function(x){y<- cbind(x$ERAlon %>% unique, x$ERAlat %>% unique()) %>% as.data.frame(); colnames(y)<- c("lon","lat"); return(y)}) %>% sapply(., function(x){
  distm(x, c(DATOS_JUNTOS_LISTA[[1]]$lon %>% unique,
             DATOS_JUNTOS_LISTA[[1]]$lat %>% unique))
}) 

DATOS_PLOT<- DATOS_JUNTOS_LISTA[[which.min(Tabla_dist)]] %>%
  mutate(ERA_binDir= cut(ERAWD ,
                         breaks =c(0,seq(22.5,337.5,22.5),360, 361), 
                         labels = c(0,seq(22.5,337.5,22.5),0) %>% as.factor()))
DATOS_PLOT$ERA_binDir<- DATOS_PLOT$ERA_binDir %>% as.character() %>% as.numeric()
DATOS_PLOT$WD_N<- ifelse(DATOS_PLOT$WD_N==155, 157.5, DATOS_PLOT$WD_N) 

#ANTES DE TAYLOR VAMOS A HACER WEIBULL
library(data.table)
library(reshape)
library(reshape2)
library(MASS)


#DISTRIBUCIÓN DE VELOCIDADES
bind_anch<- 0.1
fitweibull <- function(column, bind_anch) {
  x <- seq(0,10,by=bind_anch)
  fitparam <- column %>%
    fitdistr(densfun=dweibull,start=list(scale=1,shape=2))
  return(dweibull(x, scale=fitparam$estimate[1], shape=fitparam$estimate[2]))
}


shape_factor<- 1500
DATOS_weibull<- DATOS_PLOT %>% .[complete.cases(.), ] 
DATOS_weibull$WS_N<- ifelse(DATOS_weibull$WS_N==0,NA,DATOS_weibull$WS_N) 
DATOS_weibull<- DATOS_weibull %>% .[complete.cases(.), ]


  ggplot() + 
    geom_histogram(data= DATOS_weibull, 
                   aes(x=ERAWS),
                   binwidth = bind_anch,
                   alpha=0.4,
                   fill="red")+
    geom_histogram(data=DATOS_weibull,
                   aes(x=WS_N),
                   binwidth = bind_anch,
                   alpha=0.4,
                   fill="blue")+
    geom_line(aes(x=seq(0,10,by=bind_anch),
                  y=fitweibull(DATOS_weibull$ERAWS, bind_anch)*shape_factor), col="darkred")+
    geom_line(aes(x=seq(0,10,by=bind_anch),
                  y=fitweibull(DATOS_weibull$WS_N, bind_anch)*shape_factor), col="darkblue")+
    ylab("")+
    xlab("Wind Speed [m/s]")+
    labs(title = element_text("Speed distribution and Weibull fit", hjust = 0.5))+
    theme_light()
  
  
# CUMULATIVE WEIBULL
cdweibull <- function(column, bind_anch) {
  x <- seq(0,10,by=bind_anch)
  fitparam <- column %>%
    fitdistr(densfun=dweibull,start=list(scale=1,shape=2))
  dd<- dweibull(x, scale=fitparam$estimate[1], shape=fitparam$estimate[2])
  dd <- cumsum(dd) * c(0, diff(x))
  return(dd)
}

shape_factor<- 1000
ggplot() + 
  geom_histogram(data= DATOS_weibull, 
                 aes(x=ERAWS),
                 binwidth = bind_anch,
                 alpha=0.4,
                 fill="red")+
  geom_histogram(data=DATOS_weibull,
                 aes(x=WS_N),
                 binwidth = bind_anch,
                 alpha=0.4,
                 fill="blue")+
  geom_line(aes(x=seq(0,10,by=bind_anch),
                y=cdweibull(DATOS_weibull$ERAWS, bind_anch)*shape_factor), col="darkred")+
  geom_line(aes(x=seq(0,10,by=bind_anch),
                y=cdweibull(DATOS_weibull$WS_N, bind_anch)*shape_factor), col="darkblue")+
  ylab("")+
  xlab("Wind Speed [m/s]")+
  labs(title = element_text("Acumulated weibull distribution", hjust = 0.5))+
  theme_light()
  






##DIAGRAMAS DE TAYLOR
####TAYLOR
library(plotrix)
library(DescTools)


taylor.diagram(ref,model,add=FALSE,col="red",pch=19,pos.cor=TRUE,
               xlab="",ylab="",main="Taylor Diagram",show.gamma=TRUE,ngamma=3,
               gamma.col=8,sd.arcs=0,ref.sd=FALSE,sd.method="sample",
               grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
               pcex=1,cex.axis=1,normalize=FALSE,mar=c(5,4,6,6),...)

#FILTRADO POR DIRECCIONES
West<- c(247.5,270.0,292.5)
North_west<-c(292.5,315.0,337.5)
South_west<- c(202.5, 225.0, 247.5)
North<- c(0.0,22.5,337.5)
North_east<- c(22.5,45.0,67.5)
East<- c(67.5,90,112.5)
South_east<- c(112.5,135.0,157.5)
South<- c(157.5,180.0, 202.5)

lista_dir<- list(W=West,NW= North_west,N= North, NE=North_east, 
     E=East, SE=South_east,S=South, SW=South_west)

######filtrando por anemometro
tabla_taylor<- matrix(ncol = 5, nrow = length(lista_dir)) %>% as.data.frame()
for (i in 1:length(lista_dir)) {
  DATOS_PLOT_fil<- DATOS_PLOT %>% filter(WD_N%in%lista_dir[[i]]) %>%
    .[complete.cases(.),]
  
  if (i==1) {taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                   as.vector(DATOS_PLOT_fil$WS_N), col = i)}else{
                     taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                                    as.vector(DATOS_PLOT_fil$WS_N), col = i, add = TRUE)
                   }
  
  taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                 as.vector(DATOS_PLOT_fil$ERAWS),add = TRUE,col = i)
  
  tabla_taylor[i,1]<- cor(DATOS_PLOT_fil$ERAWS,
                          DATOS_PLOT_fil$WS_N)
  tabla_taylor[i,2]<- sd(DATOS_PLOT_fil$ERAWS,
                          DATOS_PLOT_fil$WS_N)
  tabla_taylor[i,3]<- RMSE(DATOS_PLOT_fil$ERAWS,
                         DATOS_PLOT_fil$WS_N,na.rm = TRUE)
  tabla_taylor[i,4]<- MAPE(DATOS_PLOT_fil$WS_N,
                           DATOS_PLOT_fil$ERAWS, na.rm = TRUE)
  tabla_taylor[i,5]<- length(DATOS_PLOT_fil$WS_N)
}

# get approximate legend position
lpos<-2*sd(DATOS_PLOT_fil$WS_N)
# add a legend
legend(lpos-0.1,lpos-0.7,legend=names(lista_dir),
       pch=19,col=seq(1,length(lista_dir), by=1))


colnames(tabla_taylor)<- c("Corr", "SD", "RMSE", "MAPE", "Cases")
rownames(tabla_taylor)<- names(lista_dir)
tabla_taylor[order(tabla_taylor$Corr, decreasing = TRUE),]


#filtrando por ERA5
tabla_taylorERA<- matrix(ncol = 5, nrow = length(lista_dir)) %>% as.data.frame()
for (i in 1:length(lista_dir)) {
  DATOS_PLOT_fil<- DATOS_PLOT %>% filter(ERA_binDir%in%lista_dir[[i]]) %>%
    .[complete.cases(.),]
  
  if (i==1) {taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                            as.vector(DATOS_PLOT_fil$WS_N), col = i)}else{
                              taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                                             as.vector(DATOS_PLOT_fil$WS_N), col = i, add = TRUE)
                            }
  
  taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                 as.vector(DATOS_PLOT_fil$ERAWS),add = TRUE,col = i)
  
  tabla_taylorERA[i,1]<- cor(DATOS_PLOT_fil$ERAWS,
                          DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,2]<- sd(DATOS_PLOT_fil$ERAWS,
                         DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,3]<- RMSE(DATOS_PLOT_fil$ERAWS,
                           DATOS_PLOT_fil$WS_N,na.rm = TRUE)
  tabla_taylorERA[i,4]<- MAPE(DATOS_PLOT_fil$WS_N,
                           DATOS_PLOT_fil$ERAWS, na.rm = TRUE)
  tabla_taylorERA[i,5]<- length(DATOS_PLOT_fil$WS_N)
}

# get approximate legend position
lpos<-2*sd(DATOS_PLOT_fil$WS_N)
# add a legend
legend(lpos+0.2,lpos-0.7,legend=names(lista_dir),
       pch=19,col=seq(1,length(lista_dir), by=1))


colnames(tabla_taylorERA)<- c("Corr", "SD", "RMSE", "MAPE", "Cases")
rownames(tabla_taylorERA)<- names(lista_dir)
tabla_taylorERA[order(tabla_taylorERA$Corr, decreasing = TRUE),]


ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$Corr), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$Corr), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("Correlation")+
  ggtitle("ERA5 (RED) Vs ANEMOMETER (BLUE)")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$SD), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$SD), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("SD")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$RMSE), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$RMSE), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("RMSE")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$MAPE), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$MAPE), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("MAPE")+
  theme_light()
ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$Cases), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$Cases), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("CASES")+
  theme_light()

# FILTRANDO POR DIRECCIONES INDENPENDIENTES -------------------------------------------------------------------------


######filtrando por anemometro
lista_dir<- DATOS_PLOT$ERA_binDir %>% 
  table() %>% .[order(., decreasing = T)] %>% 
  names() %>% as.numeric()
tabla_taylorERA<- matrix(ncol = 5, nrow = length(lista_dir)) %>% as.data.frame()
EscalaGrises<- seq(8, 87, length.out = length(lista_dir)) %>% round() %>% paste0("gray",.)



for (i in 1:length(lista_dir)) {
  DATOS_PLOT_fil<- DATOS_PLOT %>% filter(WD_N%in%lista_dir[[i]]) %>%
    .[complete.cases(.),]
  
  if (i==1) {taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                            as.vector(DATOS_PLOT_fil$WS_N), col = EscalaGrises[i])}else{
                              taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                                             as.vector(DATOS_PLOT_fil$WS_N),
                                             col = EscalaGrises[i], add = TRUE)
                            }
  
  taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                 as.vector(DATOS_PLOT_fil$ERAWS),add = TRUE,col = EscalaGrises[i])
  
  tabla_taylorERA[i,1]<- cor(DATOS_PLOT_fil$ERAWS,
                             DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,2]<- sd(DATOS_PLOT_fil$ERAWS,
                            DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,3]<- RMSE(DATOS_PLOT_fil$ERAWS,
                              DATOS_PLOT_fil$WS_N,na.rm = TRUE)
  tabla_taylorERA[i,4]<- MAPE(DATOS_PLOT_fil$WS_N,
                              DATOS_PLOT_fil$ERAWS, na.rm = TRUE)
  tabla_taylorERA[i,5]<- length(DATOS_PLOT_fil$WS_N)
}

# get approximate legend position
lpos<-2*sd(DATOS_PLOT_fil$WS_N)
# add a legend
legend(lpos+0.5,lpos+0.3,legend=lista_dir %>% as.character(),
       pch=19,col=EscalaGrises)


colnames(tabla_taylorERA)<- c("Corr", "SD", "RMSE", "MAPE", "Cases")
rownames(tabla_taylorERA)<- names(lista_dir)
tabla_taylorERA[order(tabla_taylorERA$Corr, decreasing = TRUE),]


#filtrando por ERA5
lista_dir<- DATOS_PLOT$ERA_binDir %>% 
  table() %>% .[order(., decreasing = T)] %>% 
  names() %>% as.numeric()
tabla_taylorERA<- matrix(ncol = 5, nrow = length(lista_dir)) %>% as.data.frame()
EscalaGrises<- seq(8, 87, length.out = length(lista_dir)) %>% round() %>% paste0("gray",.)



for (i in 1:length(lista_dir)) {
  DATOS_PLOT_fil<- DATOS_PLOT %>% filter(ERA_binDir%in%lista_dir[[i]]) %>%
    .[complete.cases(.),]
  
  if (i==1) {taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                            as.vector(DATOS_PLOT_fil$WS_N), col = EscalaGrises[i])}else{
                              taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                                             as.vector(DATOS_PLOT_fil$WS_N),
                                             col = EscalaGrises[i], add = TRUE)
                            }
  
  taylor.diagram(as.vector(DATOS_PLOT_fil$WS_N), 
                 as.vector(DATOS_PLOT_fil$ERAWS),add = TRUE,col = EscalaGrises[i])
  
  tabla_taylorERA[i,1]<- cor(DATOS_PLOT_fil$ERAWS,
                             DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,2]<- sd(DATOS_PLOT_fil$ERAWS,
                            DATOS_PLOT_fil$WS_N)
  tabla_taylorERA[i,3]<- RMSE(DATOS_PLOT_fil$ERAWS,
                              DATOS_PLOT_fil$WS_N,na.rm = TRUE)
  tabla_taylorERA[i,4]<- MAPE(DATOS_PLOT_fil$WS_N,
                              DATOS_PLOT_fil$ERAWS, na.rm = TRUE)
  tabla_taylorERA[i,5]<- length(DATOS_PLOT_fil$WS_N)
}

# get approximate legend position
lpos<-2*sd(DATOS_PLOT_fil$WS_N)
# add a legend
legend(lpos+0.5,lpos+0.3,legend=lista_dir %>% as.character(),
       pch=19,col=EscalaGrises)


colnames(tabla_taylorERA)<- c("Corr", "SD", "RMSE", "MAPE", "Cases")
rownames(tabla_taylorERA)<- names(lista_dir)
tabla_taylorERA[order(tabla_taylorERA$Corr, decreasing = TRUE),]


ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$Corr), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$Corr), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("Correlation")+
  ggtitle("ERA5 (RED) Vs ANEMOMETER (BLUE)")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$SD), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$SD), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("SD")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$RMSE), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$RMSE), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("RMSE")+
  theme_light()

ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$MAPE), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$MAPE), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("MAPE")+
  theme_light()
ggplot()+
  geom_histogram(aes(x=row.names(tabla_taylor),
                     y=tabla_taylor$Cases), 
                 stat = "identity",
                 fill= "blue",
                 alpha=0.5)+
  geom_histogram(aes(x=row.names(tabla_taylorERA),
                     y=tabla_taylorERA$Cases), 
                 stat = "identity",
                 fill= "red",
                 alpha=0.5)+
  xlab("Wind direction")+
  ylab("CASES")+
  theme_light()
