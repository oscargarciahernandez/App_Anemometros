library(here)
library(stringr)
library(lubridate)
library(rlist)
library(RNetCDF)
library(dplyr)
library(magrittr)
library(TTR)
library(readr)
library(htmltools)
library(htmlwidgets)


# Funciones ERA5 ----------------------------------------------------------


uv_transformation<- function(tabla_comp){
  
  u10<- tabla_comp$u10
  v10<- tabla_comp$v10
  
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  tabla_comp<- as.data.frame(cbind(tabla_comp,wind_abs,wind_dir_deg2))
  tabla_comp$u10<- NULL
  tabla_comp$v10<- NULL
  colnames(tabla_comp)<- c("Date","lon","lat","wind","dwi","uv_wind","uv_dwi")
  return(tabla_comp)
  
}
Formato_fecha_ERA<- function(lista_ERA){
  time_1<-utcal.nc("hours since 1900-01-01 00:00:0.0",lista_ERA$time, type = "n")
  ymd_1<-paste(time_1[,1],time_1[,2],time_1[,3],sep = "-")
  hms_1<- paste(time_1[,4],time_1[,5],time_1[,6],sep = "-")
  time_2<-ymd_hms(paste0(ymd_1," ",hms_1))
  lista_ERA$time<- time_2
  return(lista_ERA)
  
  
}
ls_to_df_ERA<- function(data_ERA_ls){
  tabla2<- data.frame()
  
  for (lon in 1:length(data_ERA_ls$longitude)) {
    tabla1<- data.frame()
    for (lat in 1:length(data_ERA_ls$latitude)) {
      x<- cbind(data_ERA_ls$longitude[lon],
                data_ERA_ls$latitude[lat],
                data_ERA_ls[["u10"]][lon,lat, ],
                data_ERA_ls[["v10"]][lon,lat, ],
                data_ERA_ls[["wind"]][lon,lat, ],
                data_ERA_ls[["dwi"]][lon,lat, ])
      
      tabla1<- rbind(tabla1,x)
      
      
    }
    tabla2<- rbind(tabla2,tabla1)
    
  }
  
  tabla_3<- cbind(data_ERA_ls$time,tabla2)
  colnames(tabla_3)<- c("Date","lon","lat","u10","v10","wind","dwi")
  
  
  ERA5_df<- uv_transformation(tabla_3)
  
  return(ERA5_df)
  
}

Dirlab_round_ERA<- function(ERA_df){
  
  
  a_dwi<- cut(ERA_df$dwi,
              breaks = c(0,seq(11.25,360,by=22.50),361),
              labels = c("N","NNE","NE","NEE","E",
                         "SEE","SE","SSE","S","SSW","SW",
                         "SWW","W","NWW","NW","NNW","N"))
  
  a_uv_dwi<- cut(ERA_df$uv_dwi,
                 breaks = c(0,seq(11.25,360,by=22.50),361),
                 labels = c("N","NNE","NE","NEE","E",
                            "SEE","SE","SSE","S","SSW","SW",
                            "SWW","W","NWW","NW","NNW","N"))
  
  tabla<- as.data.frame(cbind(ERA_df,a_dwi,a_uv_dwi))
  colnames(tabla)<- c(names(ERA_df), "Dir_dwi","Dir_uv_dwi")
  
  
  tabla$wind<- round(tabla$wind,digits = 1)
  tabla$uv_wind<- round(tabla$uv_wind,digits = 1)
  tabla$dwi<- round(tabla$dwi,digits = 1)
  tabla$uv_dwi<- round(tabla$uv_dwi,digits = 1)
  
  
  
  
  
  return(tabla)
}


# Funciones Anemometros ---------------------------------------------------

cambio_to_UTC<-function(x){
  Fecha<- dmy_hms(x$Date)
  a<- hour(with_tz(Sys.time(), tzone = Sys.timezone()))
  b<- hour(with_tz(Sys.time(), tzone = "UTC"))
  corregir_hora<- a-b
  
  x$Date<- Fecha-hm(paste(corregir_hora,":00"))
  return(x)
}

equal_dir_lab<-function(Tabla_CSV){
  a<- Tabla_CSV$Dir
  a_1<-str_remove_all(a,"h")
  a_2<- vector()
  for (i in 1:length(a_1)) {
    if(nchar(a_1[i])==4){
      a_2[i]<- str_sub(a_1[i],1,1)
    } else{
      if(str_detect(a_1[i],"-")){
        a_2[i]<- paste(str_sub(a_1[i],1,1),str_sub(a_1[i],6,6),str_sub(a_1[i],10,10))
      }else{
        a_2[i]<- paste(str_sub(a_1[i],1,1),str_sub(a_1[i],5,5))
        
      }
    }
    
  }
  a_3<-str_remove_all(str_to_upper(a_2)," ")
  
  a_4<- vector()
  for (i in 1:length(a_1)) {
    
    if(nchar(a_3[i])>1){
      x<- str_sub(a_3[i],1,1)
      if(x=="E" || x=="W"){
        a_4[i]<-str_remove_all(paste(str_sub(a_3[i],2,2),str_sub(a_3[i],1,1),str_sub(a_3[i],3,3))," ")
      }else{a_4[i]<- a_3[i]}
      
    }else{a_4[i]<- a_3[i]}
    
    
  }
  
  Tabla_CSV$Dir<- a_4
  
  return(Tabla_CSV)
}


# Funciones Calibración ---------------------------------------------------

#!!!!!!. Filtrar_datos no esta del todo adaptado por que nuestros datos de anemos no tienen $dir en numerico, sino en character.
filtrar_datos=function(datos_anemos){
  #Esta funcion hace lo mismo que el script FiltroObservaciones.r de Sheilla, pero
  #adaptandolo a nuestros formatos y pudiendo definir vel_max, dif_max.
  
  #Valores originales de Sheila:
  #vel_max(media)=50 km/h
  #dif_max=30 km/h
  
  #Valores nuevos propuestos por Sheila:
  #vel_max(media)=90 km/h
  #vel_max(racha)=200 km/h
  #dif_max=30 km/h
  
  #Esta funcion trabaja con un data.frame de mediciones de anemos que sigan nuestro
  #nuestro formato estandar. Si en vez de eso recibe una lista, se llama a si mismo
  #para cada elemento de la lista.

  if (class(datos_anemos)!="data.frame") {
    #Plotear racha con todos los datos
    plot(datos_anemos$Gust,x = datos_anemos$Date,type="p",col="blue")
    #Plotear media con todos los datos
    lines(datos_anemos$Mean,x = datos_anemos$Date,type="p")
    #Fitros de viento medio
    #Nivel 1 -- limites
    #Para mean, Velocidad [0,50/3.6] (m/s)
    N<-which(datos_anemos$Mean>50/3.6 | datos_anemos$Mean<=0)
    points(x = datos_anemos$Date[N],y = datos_anemos$Mean[N],col="red",lwd=5)
    if (length(N)!=0){
      datos_anemos$Mean[N]<-NA
    }
    
    #Para gust, Velocidad [0,200/3.6] (m/s)
    N<-which(datos_anemos$Gust>50/3.6 | datos_anemos$Gust<=0)
    points(x = datos_anemos$Date[N],y = datos_anemos$Gust[N],col="green",lwd=5)
    if (length(N)!=0){
      datos_anemos$Mean[N]<-NA
    }
    
    #Direccion [0,360]
    #N<-which(Datosdf$dir>360 | Datosdf$dir<0)
    #if (length(N)!=0){
    #  Datosdf$dir[N]<-NA
    #}
    
    #Nivel 2 -- coherencia temporal del dato 
    #Step test:
    #Velocidad diferencia con el dato anterior de 30 m/s tanto si la diferencia es + como si es -
    #Este filtro solo se lo pasamos al mean
    i2<-NULL
    for (i in 2:length(datos_anemos$Mean)){
      difer<-datos_anemos$Mean[i-1]-datos_anemos$Mean[i]
      if (is.na(difer)==FALSE & abs(difer)>30/3.6){
        i2<-cbind(i,i2)
      }
    }
    points(x = datos_anemos$Date[i2],y = datos_anemos$Mean[i2],col="red",lwd=5)
    datos_anemos$Mean[i2]<-NA
    
    
    #Ahora al gust
    i2<-NULL
    for (i in 2:length(datos_anemos$Gust)){
      difer<-datos_anemos$Gust[i-1]-datos_anemos$Gust[i]
      if (is.na(difer)==FALSE & abs(difer)>30/3.6){
        
        i2<-cbind(i,i2)
      }
    }
    points(x = datos_anemos$Date[i2],y = datos_anemos$Gust[i2],col="green",lwd=5)
    datos_anemos$Gust[i2]<-NA
    
    
    
    #Nivel 4 -- coherencia temporal de la serie
    # Velocidad 
    # En 1 horas (6 tomas) que la velocidad no varie en 0.1
    N2<-c()
    for (i in 6:c(dim(Datosdf)[1])){
      if(is.na(datos_anemos$Mean[i])==FALSE){
        difer<-max(Datosdf[c((i-5):i),2],na.rm=TRUE)-min(Datosdf[c((i-5):i),2],na.rm=TRUE)
        if (is.na(difer)==FALSE & abs(difer)<=0.1){
          #datos_anemos$Mean[i]<-NA
          N2<-c(N2,i)
        }
      }
    }
    
    # Direcci?n
    # En 1 hora que la direcci?n no varie en 1
    N3<-c()
    for (i in 6:c(dim(Datosdf)[1])){
      if(is.na(Datosdf$dir[i])==FALSE){
        difer<-max(Datosdf$dir[(i-5):i],na.rm=TRUE)-min(Datosdf$dir[(i-5):i],na.rm=TRUE)
        if (is.na(difer)==FALSE & abs(difer)<=1){
          N3<-c(N3,i)
        }
      }
    }
    
    
    # Direcci?n
    # En 1 hora no varia nada si no tenemos en cuenta los 0s
    N4<-c()
    for (i in 6:c(dim(Datosdf)[1])){
      if(is.na(Datosdf$dir[i])==FALSE){
        data<-Datosdf$dir[(i-5):i]
        n_data<-which(data==0)
        data<-data[-n_data]
        if(is.na(data)==FALSE){
          difer<-max(data,na.rm=TRUE)-min(data,na.rm=TRUE)
          if (is.na(difer)==FALSE & abs(difer)<=1){
            N4<-c(N4,i)
          }
        }
      }
    }
    
  }else{
  if (class(datos_anemos)!="list") {
    for (i in 1:length(datos_anemos)) {
      datos_anemos[[i]]=filtrar_datos(datos_anemos[[i]])
    }
    
  }else{
    print("ERROR. El input datos_anemos que ha recibido la funcion filtrar_datos no es ni data.frame ni list")
  }
  }
  
  
}
  
suavizar_vector_viento=function(vector_viento,n){
  #Esta funcion recibe un vector de valores de viento y un valor de n y devuelve otro 
  #vector que es el resultado de aplicarle un SMA de valor n al primero.
  for (i in 1:length(vector_viento)) {
    vector_viento[i]=SMA(vector_viento[i],n=n)
  }
  return(vector_viento)
}

cor_y_plot=function(vector_viento_anemos,vector_viento_ER5){
  #Esto necesita una vuelta
  cor=cor(vector_viento_anemos,vector_viento_ER5)
  print(cor)
  plot(vector_viento_anemos,type = "l")
  lines(vector_viento_ER5,col="red")
}

