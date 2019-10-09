library(here)
source('NUEVO/Libraries.R')

# IMPORTAR DATOS DE ANEMOMETROS Y ERA5 ------------------------------------
ERA5_df<- readRDS(here::here("NUEVO/Data_ERA5/ERA5_df.RDS"))
Anemometros<- readRDS(here::here('NUEVO/Data_anemometros/Anemometros.RDS'))

# IMPORTAMOS TABLA DE REGISTRO 
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}


# ACONDICIONAMOS DATOS DE ANEMOMETROS -------------------------------------

#ENSAÑAMOS LOS ANEMOMETROS DISPONIBLES Y ELEGIMOS UNO
ID_ANEMO<- levels(t_reg$ID)  

for (ID_ind  in 1:length(ID_ANEMO)) {
  
  anemo_elegido=ID_ANEMO[ID_ind]
  tryCatch({
    #RELLENAMOS HUECOS
    datos_anemos=rellenar_huecos_anemos(Anemometros[[which(names(Anemometros)==as.character(anemo_elegido))]])
    
    #ORDENAMOS CRONOLÓGICAMENTE
    datos_anemos=datos_anemos[order(datos_anemos$Date),]
    
    #FILTRAMOS DATOS ANEMOS
    #ME DEVULVE ERROR PORQUE NECESITA QUE LA COLUMNA DIR SEA CHARACTER??? 
    
    #datos_anemos$Dir<-as.character(datos_anemos$Dir)
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
                                          dplyr::select(Fecha_ini) %>% .[1,] %>% 
                                          as.character()%>%
                                          dmy())),]
    
    #PONEMOS EN FORMATO LOS DATOS DE LOS ANEMOMETROSE
    #CARGAMOS COORDENADAS ANEMOS Y ELIMINAMOS LAS COMAS
    Coordenadas_anemos<- filter(t_reg,ID==anemo_elegido) %>% dplyr::select(lon,lat)  #Primero lon, luego lat
    Coordenadas_anemos$lon<- Coordenadas_anemos$lon %>% as.character() %>% str_replace(",",".") %>% as.numeric()
    Coordenadas_anemos$lat<- Coordenadas_anemos$lat %>% as.character() %>% str_replace(",",".") %>% as.numeric()
    
    #datos_anemos$Date<- datos_anemos$Date %>% round_date(unit="hour")
    #datos_anemos$Gust<- NULL
    datos_anemos$Dir<- datos_anemos$Dir %>% as.numeric()
    
    
    datos_anemos$lon<- rep(Coordenadas_anemos$lon, nrow(datos_anemos))
    datos_anemos$lat<- rep(Coordenadas_anemos$lat, nrow(datos_anemos))
    
    #CREAMOS ANEMOS TABLA PARA NO JODER DATOS ANEMOS
    ANEMOS_TABLA<- datos_anemos[,c("Date","lon","lat", "Mean", "Dir","Gust")]
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
    
    ANEMOS_TABLA_ADDROWS<-ANEMOS_TABLA_ADDROWS  %>% 
      mutate(WS_l= na.interpolation(.$WS, "linear"),
             #WS_sp= na.interpolation(.$WS, "spline"),
             #WS_st= na.interpolation(.$WS, "stine"),
             WSMAX_l= na.interpolation(.$WSMAX, "linear"),
             #WSMAX_sp= na.interpolation(.$WSMAX, "spline"),
             #WSMAX_st= na.interpolation(.$WSMAX, "stine"),
             WS_N=REPLACE_NNONA(.$WS),
             WSMAX_N=REPLACE_NNONA(.$WSMAX),
             WD_N=REPLACE_NNONA(.$WD))
    
    
    
    
    #GUARDAMOS DATOS ANEMOS LIMPIOS 
    path_anemo<- here::here('NUEVO/Data_calibracion/') %>% paste0(anemo_elegido)
    ifelse(dir.exists(path_anemo),('YA EXISTE EL DIRECTORIO'), dir.create(path_anemo,recursive = T))
    
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
    n=50
    Coordenadas_era_cercanas=Coordenadas_era[order(distm(Coordenadas_era, 
                                                         Coordenadas_anemos, 
                                                         fun = distHaversine)[,1]),] %>% 
      .[1:n,]
    
    #De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
    ERA5_df_CERCANOS=filter(ERA5_df,
                            lat %in% Coordenadas_era_cercanas$lat,
                            lon %in% Coordenadas_era_cercanas$lon) %>% 
      dplyr::select(Date,lon,lat,uv_wind,uv_dwi)
    colnames(ERA5_df_CERCANOS)<- c("Date", "ERAlon",
                                   "ERAlat","ERAWS","ERAWD")
    
    
    library(rgbif)
    ALTURA_ANEMO<- elevation(longitude = Coordenadas_anemos$lon,
                             latitude = Coordenadas_anemos$lat, 
                             username = 'proyectoroseo')
    
    ERA_LIST<- ERA5_df_CERCANOS %>% group_by(ERAlat, ERAlon) %>% group_split()
    
    
    ERA_LIST_CORRECTED<-list()
    
    ZO_DATA<- readRDS('NUEVO/Data_ERA5/DATOS_RUGOSIDAD.RDS')
    
    for(i in 1:length(ERA_LIST) ){
      ERA_DATA<- ERA_LIST[[i]]
      
      ALTURA_ERA<- elevation(longitude =ERA_DATA$ERAlon %>% unique(),
                             latitude = ERA_DATA$ERAlat %>% unique(), 
                             username = 'proyectoroseo')
      
      #LEY LOGARITMICA PARA MODIFICAR LA ALTURA DEL PUNTO DE ERA5
      zo=2 # RUGOSIDAD
      z=ALTURA_ANEMO$elevation_geonames #ALTURA ANEMO
      zr=ifelse(ALTURA_ERA$elevation_geonames<0,
                0,
                ALTURA_ERA$elevation_geonames) + 10 #ALTURA ERA
      
      k=log(z/zo)/log(zr/zo)  #FACTOR K
      
      zo1<- ZO_DATA[which(ZO_DATA$lon==ERA_DATA$ERAlon %>% unique() & 
                            ZO_DATA$lat==ERA_DATA$ERAlat %>% unique() ), "sfr"]
      k1=log(z/zo1)/log(zr/zo1)
      
      ERA_DATA$ERAWS_CORRECTED<- ERA_DATA$ERAWS*k
      ERA_DATA$ERAWS_CORRECTED_ZO1<- ERA_DATA$ERAWS*as.numeric(k1)
      ERA_LIST_CORRECTED[[i]]<- ERA_DATA
    }
    
    ERA5_df_CERCANOS<- ERA_LIST_CORRECTED %>% bind_rows()
    
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
    
  }, error= function(e){
    print(e)
    return(anemo_elegido)
  })

}
