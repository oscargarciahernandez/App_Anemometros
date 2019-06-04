library(here)
source(here::here("NUEVO/Libraries.R"))



# IMPORTAR DATOS DE ANEMOMETROS Y ERA5 ------------------------------------
ERA5_df<- readRDS(here::here("NUEVO/Data_ERA5/ERA5_df.RDS"))
Anemometros<- readRDS(here::here('NUEVO/Data_anemometros/Anemometros.RDS'))



# IMPORTAMOS TABLA DE REGISTRO 
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}

#ENSAÑAMOS LOS ANEMOMETROS DISPONIBLES Y ELEGIMOS UNO
levels(t_reg$ID)  

anemo_elegido="0B38DAE79059"

#RELLENAMOS HUECOS
datos_anemos=rellenar_huecos_anemos(Anemometros[[which(names(Anemometros)==as.character(anemo_elegido))]])

#ORDENAMOS CRONOLÓGICAMENTE
datos_anemos=datos_anemos[order(datos_anemos$Date),]

#FILTRAMOS DATOS ANEMOS
#ME DEVULVE ERROR PORQUE NECESITA QUE LA COLUMNA DIR SEA CHARACTER??? 

datos_anemos$Dir<-as.character(datos_anemos$Dir)
lista_pos_errores=filtrar_datos(datos_anemos,desglosar = TRUE)




N_errores=unique(c(lista_pos_errores$N1_mean,lista_pos_errores$N2_mean,lista_pos_errores$N3_mean,
                   lista_pos_errores$N1_gust,lista_pos_errores$N2_gust,lista_pos_errores$N3_gust))


#DONDE ESTÁN LOS NA's
N_na=which(rowSums(is.na(datos_anemos[,c(2,3,4)]))>0)




#Quitar mediciones erroneas, guardar, cargar----
#Llenar de NAs las mediciones consideradas erroneas. No eliminamos la fila; queremos mantener la fechas de los NAs.
datos_anemos[N_errores,c(2,3,4)]=NA
datos_anemos[N_na,c(2,3,4)]=NA    #Esto parece redundante pero viene bien asegurarse



#ELIMINAMOS LOS VALORES QUE ESTÉN POR DEBAJO DE FECHA_INI DE LA TABLA DE REGISTRO
datos_anemos=datos_anemos[-(1:which(datos_anemos$Date< filter(t_reg,ID==anemo_elegido) %>% 
                                      select(Fecha_ini) %>% .[1,] %>% 
                                      as.character()%>%
                                      dmy())),]

#PONEMOS EN FORMATO LOS DATOS DE LOS ANEMOMETROSE
#CARGAMOS COORDENADAS ANEMOS Y ELIMINAMOS LAS COMAS
Coordenadas_anemos=filter(t_reg,ID==anemo_elegido) %>% select(lon,lat)  #Primero lon, luego lat
Coordenadas_anemos$lon<- Coordenadas_anemos$lon %>% as.character() %>% str_replace(",",".") %>% as.numeric()
Coordenadas_anemos$lat<- Coordenadas_anemos$lat %>% as.character() %>% str_replace(",",".") %>% as.numeric()

#datos_anemos$Date<- datos_anemos$Date %>% round_date(unit="hour")
datos_anemos$Gust<- NULL
datos_anemos$WD<- datos_anemos$WD %>% as.numeric()


datos_anemos$lon<- rep(Coordenadas_anemos$lon, nrow(datos_anemos))
datos_anemos$lat<- rep(Coordenadas_anemos$lat, nrow(datos_anemos))

datos_anemos<- datos_anemos[,c(1,4,5,2,3)]
colnames(datos_anemos)<-c("Date","lon","lat", "WS", "WD")
datos_anemos$id<- rep("ANEM", nrow(datos_anemos))


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
colnames(ERA5_df_CERCANOS)<- c("Date", "lon",
                               "lat","WS","WD")


#LEY LOGARITMICA PARA MODIFICAR LA ALTURA DEL PUNTO DE ERA5
zo=3 # RUGOSIDAD
z=155 + 3.5*6 + 1.5 #ALTURA ANEMO
zr=401 + 10 #ALTURA ERA
k=log(z/zo)/log(zr/zo)  #FACTOR K

ERA5_df_CERCANOS$WS<- ERA5_df_CERCANOS$WS*k
ERA5_df_CERCANOS$id<- rep("ERA5", nrow(ERA5_df_CERCANOS))




#UNIMOS DATOS DE LOS ANEMOMETROS Y ERA5

Tabla_unida<- bind_rows(ERA5_df_CERCANOS, datos_anemos)



