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
library(openair)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(OpenStreetMap)
library(rJava)
library(rgdal)
library(RColorBrewer)
library(geosphere)
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
uv_transformation_2019<- function(tabla_comp){
  
  u10<- tabla_comp$u10
  v10<- tabla_comp$v10
  
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  tabla_comp<- as.data.frame(cbind(tabla_comp,wind_abs,wind_dir_deg2))
  tabla_comp$u10<- NULL
  tabla_comp$v10<- NULL
  colnames(tabla_comp)<- c("Date","lon","lat","uv_wind","uv_dwi")
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
ls_to_df_ERA_2019<- function(data_ERA_ls){
  tabla2<- data.frame()
  
  for (lon in 1:length(data_ERA_ls$longitude)) {
    tabla1<- data.frame()
    for (lat in 1:length(data_ERA_ls$latitude)) {
      x<- cbind(data_ERA_ls$longitude[lon],
                data_ERA_ls$latitude[lat],
                data_ERA_ls[["u10"]][lon,lat, ],
                data_ERA_ls[["v10"]][lon,lat, ])
      
      tabla1<- rbind(tabla1,x)
      
      
    }
    tabla2<- rbind(tabla2,tabla1)
    
  }
  
  tabla_3<- cbind(data_ERA_ls$time,tabla2)
  colnames(tabla_3)<- c("Date","lon","lat","u10","v10")
  
  
  ERA5_df<- uv_transformation_2019(tabla_3)
  
  return(ERA5_df)
  
}


Dirlab_round_ERA<- function(ERA_df){
  
  
  a_dwi<- cut(ERA_df$dwi,
              breaks = c(0,seq(11.25,360,by=22.50),361),
              labels = c("N","NNE","NE","NEE","E",
                         "SEE","SE","SSE","S","SSW","SW",
                         "SWW","W","NWW","NW","NNW","N1"))
  a_dwi<- ifelse(a_dwi=="N1", "N",a_dwi)
  
  
  a_uv_dwi<- cut(ERA_df$uv_dwi,
                 breaks = c(0,seq(11.25,360,by=22.50),361),
                 labels = c("N","NNE","NE","NEE","E",
                            "SEE","SE","SSE","S","SSW","SW",
                            "SWW","W","NWW","NW","NNW","N1"))
  
  a_uv_dwi<- ifelse(a_uv_dwi=="N1", "N",a_uv_dwi)
  
  
  
  tabla<- as.data.frame(cbind(ERA_df,a_dwi,a_uv_dwi))
  colnames(tabla)<- c(names(ERA_df), "Dir_dwi","Dir_uv_dwi")
  
  
  tabla$wind<- round(tabla$wind,digits = 1)
  tabla$uv_wind<- round(tabla$uv_wind,digits = 1)
  tabla$dwi<- round(tabla$dwi,digits = 1)
  tabla$uv_dwi<- round(tabla$uv_dwi,digits = 1)
  
  
  
  
  
  return(tabla)
}
Dirlab_round_ERA_2019<- function(ERA_df){
  
  
  a_uv_dwi<- cut(ERA_df$uv_dwi,
                 breaks = c(0,seq(11.25,360,by=22.50),361),
                 labels = c("N","NNE","NE","NEE","E",
                            "SEE","SE","SSE","S","SSW","SW",
                            "SWW","W","NWW","NW","NNW","N"))
  
  tabla<- as.data.frame(cbind(ERA_df,a_uv_dwi))
  colnames(tabla)<- c(names(ERA_df),"Dir_uv_dwi")
  
  
  tabla$uv_wind<- round(tabla$uv_wind,digits = 1)
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

#Igual habria que borrar la funcion equal_dir_lab, no le veo ninguna ventaja sobre direccion_en_numerico
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

direccion_en_numerico=function(datos_anemos){
  #Esta funcion recibe un data.frame de mediciones de anemos y pone $Dir en grados
  if (is.factor(datos_anemos$Dir)){
    datos_anemos$Dir=as.character(datos_anemos$Dir)
    print("La columna $Dir de tus datos ha pasado de estar en factor a estar en character")
  }
  if (is.character(datos_anemos$Dir)){
    #Es importante que se sustituyan por numeros primero los string de tipo "North-northeast" luego "Northeast" y luego "North"
    #Asi no evitamos que salgan cosas tipo "0-northeast".(Originalmente pondria "North-northeast")
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "North-northeast",replacement = 22.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "East-northeast",replacement = 67.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "East-southeast",replacement = 112.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "South-Southeast",replacement = 155)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "South-southwest",replacement = 202.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "West-southwest",replacement = 247.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "West-northwest",replacement = 292.5)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "North-northwest",replacement = 337.5)
    
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "Northeast",replacement = 45)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "Southeast",replacement = 135)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "Southwest",replacement = 225)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "Northwest",replacement = 315)
    
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "North",replacement = 0)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "East",replacement = 90)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "South",replacement = 180)
    datos_anemos$Dir=gsub(x = datos_anemos$Dir,pattern = "West",replacement = 270)
    
    
    
  }else{
    if (is.numeric(datos_anemos$Dir)){
      print("Parece que tus mediciones ya tienen la direccion en numerico")
    }else{
      print("ERROR. Parece que tus mediciones no tienen la direccion ni en numerico ni en character ")
    }
  }
  datos_anemos$Dir=as.numeric(datos_anemos$Dir)
  return(datos_anemos)
}


# Funciones Calibración ---------------------------------------------------

buscar_huecos_anemos=function(datos_anemos){
  #Esta funcion busca que huecos tenemos en las mediciones.
  #No sobreescribe nada, solo devuelve el dataframe huecos.
  #huecos[1] muestra cual es la medicion posterior (en el tiempo) al hueco.
  #huecos[2] muestra cual es la medicion anterior (en el tiempo) al hueco.
  #Las fechas estan en numerico (segundos desde 1970-01-01)
  huecos=data.frame(a=as.POSIXct(character(),tz="UTC"), b=as.POSIXct(character(),tz="UTC"))  #Creamos relleno de esta forma para que cada columna este ya en el formato que queremos
  colnames(huecos)=c("despues","antes")
  cont=1
  for(i in 1:(dim(datos_anemos)[1]-1)){
    difer=time_length((datos_anemos$Date[i]-datos_anemos$Date[i+1]))     #La duracion del hueco en segundos, formato numeric
    if (isTRUE(difer<=0)){
      print(paste0("ERROR! datos_anemos$Date[",as.character(i-1),"]-datos_anemos$Date[",as.character(i),"]=",as.character(difer/60)," minutos"))
    }
    if (isTRUE(difer>7*1.5*60)){
      huecos[cont,1]=datos_anemos$Date[i]
      huecos[cont,2]=datos_anemos$Date[i+1]
      cont=cont+1
    }
  }
  return(huecos)
}

#Esto ahora mismo no funciona bien
rellenar_huecos_anemos=function(datos_anemos){
  #Esta funcion recoge un data.frame de formato estandar de anemos
  #Devuelve un data.frame parecido, pero con las mediciones filtradas por MobileAlerts.
  #Las lineas añadidas tienen la fecha en la que se supone que se tendrian que haber hecho mediciones.
  #El resto (mean, gust, dir) son NAs.
  huecos=buscar_huecos_anemos(datos_anemos)
  lista=list()  #Vamos a crear un a lista que contega dataframes. Estos seran los cachitos que van a conformar datos_anemos_rellenado.
  lista[[1]]=datos_anemos[1:which(datos_anemos$Date==huecos[1,1]),]
  for (i in 1:(nrow(huecos)))
    #Por cada hueco hay que crear un data.frame con los datos anteriores (anteriores en el data.frame, posteriores en el tiempo) y otro con las mediciones vacias (el relleno).
    {#Primero el relleno, las lineas que parece que MobileAlerts nos ha filtrado, con NAs en vez de mediciones.
    relleno=data.frame(a=as.POSIXct(character(),tz="UTC"), b=numeric(), c=numeric(), d=numeric())  #Creamos relleno de esta forma para que cada columna este ya en el formato que queremos
    difer=time_length((huecos$despues[i]-huecos$antes[i]))     #La duracion del hueco en segundos, formato numeric
    for (j in 1:(round(difer/(7*60))-2)) #Cuantas mediciones faltan? Solo una si difer ~= 2*7*60 segs, 2 si difer ~= 3*7*60 ...
    {relleno[j,1]=huecos$despues[i]-j*(difer)/(round(difer/(7*60))-1)
    #relleno[j,1]=la fecha posterior en el tiempo al hueco - j*(tamaño del hueco)/(el numero de mediciones que faltan en este hueco)
    }
    colnames(relleno)=colnames(datos_anemos)
    lista[[2*i]]=relleno
    if (i==(nrow(huecos))) {
      lista[[length(lista)+1]]=datos_anemos[which(datos_anemos$Date==huecos$antes[i]):nrow(datos_anemos),] #Si estamos con el ultimo hueco, hay que coger todo hasta el final de datos_anemos
    }else{
      lista[[2*i+1]]=datos_anemos[which(datos_anemos$Date==huecos$antes[i]):which(datos_anemos$Date==huecos[i+1,1]),] #Las mediciones entre el principio del hueco y el final del siguiente hueco
    }
  }
  datos_anemos_rellenado=do.call("rbind", lista) #Juntar todos los cachitos. Haciendo esto en vez de monton de rbins usamos menos recursos del pc, y tarda mucho menos.
  colnames(datos_anemos_rellenado)=colnames(datos_anemos)
  return(datos_anemos_rellenado)
}

filtrar_datos=function(datos_anemos,
                       mean_max=50/3.6,     #[m/s]
                       gust_max=200/3.6,    #[m/s]
                       dif_max_gust=30/3.6, #[m/s]
                       dif_max_mean=20/3.6, #[m/s]
                       dif_min=0/3.6,       #[m/s]
                       tomas_dif_min=20,    #[-]  Numero de tomas consecutivas en las que el viento varia en menos de dif_min
                       desglosar=FALSE)
  {#Esta funcion trabaja con un data.frame de mediciones de anemos que sigan nuestro
  #nuestro formato estandar.
  
  #Explicacion desglosar: si desglosar=FALSE, se devolveran todas las posiciones de mediciones supuestamente erroneas
  #en un solo vector. Si desglosar=TRUE, se devolvera una lista de vectores de posiciones, cada vector correspondiente a un filtro
  
  #Analizar los inputs.Errores?
  if (!is.data.frame(datos_anemos)) {stop("El input no es un dataframe")}
  if (nrow(datos_anemos)==0)  {stop("El dataframe esta vacio")}
  if (sum(unlist(lapply(datos_anemos, class))!=c("POSIXct","POSIXt","numeric","numeric","numeric"))) {stop("Formato inadecuado")}
  
  #Nivel 1 -- Fitros de viento medio
  N1_mean=c()
  #N1_mean=which(datos_anemos$Mean>mean_max | datos_anemos$Mean<=0 )   #<=0
  N1_mean=which(datos_anemos$Mean>mean_max | datos_anemos$Mean<0 )     #<0
  
  #N1_gust=which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<=0)     #<=0
  N1_gust=which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<0)       #<0
  
  #Nivel 2 -- coherencia temporal del dato 
  #Step test:
  #Velocidad diferencia con el dato anterior de dif_max m/s tanto si la diferencia es + como si es -
  N2_mean=c()
  for (i in 2:length(datos_anemos$Mean)){
    difer<-datos_anemos$Mean[i-1]-datos_anemos$Mean[i]
    if (is.na(difer)==FALSE & abs(difer)>dif_max_mean){
      N2_mean[length(N2_mean)+1]=i
    }
  }
  
  N2_gust=c()
  for (i in 2:length(datos_anemos$Gust)){
    difer<-datos_anemos$Gust[i-1]-datos_anemos$Gust[i]
    if (is.na(difer)==FALSE & abs(difer)>dif_max_gust){
      N2_gust[length(N2_gust)+1]=i
    }
  }
  
  #Nivel 3 -- coherencia temporal de la serie
  #Idea para calibrar este filtro: fijarse si vuando detecta errores en mean tambien lo hace en gust. Solo mean=puedes ser un perido e calma. Ambos:el anemo esta totalmente quieto durante decenas de minutos. 
  #En tomas_dif_min tomas que la velocidad no varie en dif_min (Mean)
  N3_mean=c()
  i=1
  while (i<=(nrow(datos_anemos)-tomas_dif_min+1)){
    if(is.na(datos_anemos$Mean[i])==FALSE){
      difer<-max(datos_anemos$Mean[i:(i+tomas_dif_min-1)],na.rm=TRUE)-min(datos_anemos$Mean[i:(i+tomas_dif_min-1)],na.rm=TRUE)
      if (is.na(difer)==FALSE & abs(difer)<=dif_min){
        N3_mean[(length(N3_mean)+1):(length(N3_mean)+tomas_dif_min)]=(i:(i+tomas_dif_min-1))
        i=i+tomas_dif_min-1 #Se hace un -1 para compensar el +1 que se le va hacer al final del while
      }
    }
    i=i+1
  }
  
  # En tomas_dif_min tomas que la velocidad no varie en dif_min (Gust)
  N3_gust=c()
  i=1
  while (i<=(nrow(datos_anemos)-tomas_dif_min+1)){
    if(is.na(datos_anemos$Gust[i])==FALSE){
      difer<-max(datos_anemos$Gust[i:(i+tomas_dif_min-1)],na.rm=TRUE)-min(datos_anemos$Gust[i:(i+tomas_dif_min-1)],na.rm=TRUE)
      if (is.na(difer)==FALSE & abs(difer)<=dif_min){
        N3_gust[(length(N3_gust)+1):(length(N3_gust)+tomas_dif_min)]=(i:(i+tomas_dif_min-1))
        i=i+tomas_dif_min-1 #Se hace un -1 para compensar el +1 que se le va hacer al final del while
      }
    }
    i=i+1
  }
  
  # Direcci?n
  # En 1 hora que la direcci?n no varie en 1
  #Esto no creo que nos sirva porque tenemos poca resolucion en $Dir (22,5>>1)
  #Por eso meto los filtros de direccion en un if que nunca se va a ejecutar
  if (FALSE) {
    for (i in 6:c(nrow(datos_anemos))){
      if(is.na(datos_anemos$Dir[i])==FALSE){
        difer<-max(datos_anemos$Dir[(i-5):i],na.rm=TRUE)-min(datos_anemos$Dir[(i-5):i],na.rm=TRUE)
        if (!is.na(difer) & abs(difer)<=1){
          N_dir[length(N_dir)+1]=i
        }
      }
    }
    
    
    # Direcci?n
    # En 1 hora no varia nada si no tenemos en cuenta los 0s
    N4<-c()
    for (i in 6:c(nrow(datos_anemos))){
      if(is.na(datos_anemos$Dir[i])==FALSE){
        data<-datos_anemos$Dir[(i-5):i]
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
  }
  
  if (desglosar==TRUE) {
    lista=list(N1_mean,N1_gust,N2_mean,N2_gust,N3_mean,N3_gust)
    names(lista)=c("N1_mean","N1_gust","N2_mean","N2_gust","N3_mean","N3_gust")
    return(lista)
  }else{
    return(unique(c(N1_mean,N1_gust,N2_mean,N2_gust,N3_mean,N3_gust)))
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
  cor=cor(vector_viento_anemos,vector_viento_ER5,"na")
  print(cor)
  plot(vector_viento_anemos,col="black",type="l")
  lines(vector_viento_ER5,col="red")
}

extract_hourly_data_2=function(datos_anemos){
  #Primero definimos las horas en punto
  #No cogemos las mas cercanas (round_date) sino las que estan "dentro"
  fechainicio<-floor_date(range(datos_anemos$Date)[1],unit = "hours")+3600
  fechafinal<-floor_date(range(datos_anemos$Date)[2],unit = "hours")
  
  #Creamos el dataframe que devolveremos. En vez de crearlo de cero, hacemos esto para quitarnos problemas de formatos.
  datos_anemos_horario=datos_anemos[1,]   #Copiar la primea linea
  datos_anemos_horario[1,]=NA             #Vaciarla
  datos_anemos_horario=cbind(datos_anemos_horario,Date_roud=seq(fechainicio,fechafinal, by="hours"))  #Añadir una columna extra con las hora en punto. Asi mantenemos la hora original en $Date
  
  #Para cada hora en punto, buscamos el dato de anemo mas cercano y rellenamos datos_anemos_horario
  for (i in 1:nrow(datos_anemos_horario)) {
    n=(datos_anemos$Date-datos_anemos_horario$Date_roud[i]) %>% as.numeric %>% abs %>% which.min
    datos_anemos_horario[i,1:4]=datos_anemos[n,]
  }
  return(datos_anemos_horario)
}

juntar_datos=function(datos1,datos2,interpolar=F,nombres_col_fechas=c("Date","Date"),coletillas=c("","")){
  #Esta funcion recoge dos dataframes de datos (1 y 2), y devuelve otro (3)
  #(3) contiene informacion de la epoca en la que se solapan (1) y (2)
  #(1) marca las fechas a usar. A cada fecha de (1) se le asigna la mas cercana de (2)
  
  #Explicación nombres_col_fechas: vector en el que especificamos los nombres de las columnas donde estan las fechas en nuestros datos.
  #Explicación coletillas: strings que se añaden a los nombres de columnas de los inputs
  
  #Analizar los inputs.Errores?
  if (!is.data.frame(datos1)) {stop("El primer input no es un dataframe")}
  if (!is.data.frame(datos2)) {stop("El segundo input no es un dataframe")}
  if (nrow(datos1)==0) {stop("El primer input tiene 0 lineas")}
  if (nrow(datos2)==0) {stop("El segundo input tiene 0 lineas")}
  #Donde estan las fechas en cada uno? (Estaria bien algun dia mirar el tipo de cada columna, no el nombre, pero ahora se me atraganta)
  if (!is.vector(nombres_col_fechas)) {stop("nombres_col_fechas no es un vector")}
  if (length(nombres_col_fechas)!=2) {stop("nombres_col_fechas tiene que tener 2 elementos")}
  if (length(nombres_col_fechas)!=2) {stop("nombres_col_fechas tiene que tener 2 elementos")}
  if (sum(lapply(nombres_col_fechas, class)=="character")!=2) {stop("nombres_col_fechas solo puede contener strings")}
  if (sum(colnames(datos1)==nombres_col_fechas[1])==0) {stop(paste0("El primer input no tiene ninguna columna llamada \"",nombres_col_fechas[1],"\""))}
  if (sum(colnames(datos2)==nombres_col_fechas[2])==0) {stop(paste0("El segundo input no tiene ninguna columna llamada \"",nombres_col_fechas[2],"\""))}
  if (sum(colnames(datos1)==nombres_col_fechas[1])>1) {stop(paste0("El primer input tiene varias columnas llamadas \"",nombres_col_fechas[1],"\". Solo puede haber una"))}
  if (sum(colnames(datos2)==nombres_col_fechas[2])>1) {stop(paste0("El segundo input tiene varias columnas llamadas \"",nombres_col_fechas[2],"\". Solo puede haber una"))}
  if (!is.vector(coletillas)) {stop("nombres_col_fechas no es un vector")}
  if (length(coletillas)!=2) {stop("coletillas tiene que tener 2 elementos")}
  if (sum(lapply(coletillas, class)=="character")!=2) {stop("coletillas solo puede contener strings")}
  if (!is.character(coletillas) & !is.null(coletillas)) {stop("La coletilla tiene que ser character o NULL")}
  
  #La funcion range se cabrea con los tibbles. Pasamos a dataframe
  datos1=as.data.frame(datos1)
  datos2=as.data.frame(datos2)
  
  col_fechas1=which(colnames(datos1)==nombres_col_fechas[1])
  col_fechas2=which(colnames(datos2)==nombres_col_fechas[2])
  
  #Aplicar coletillas
  colnames(datos1)=paste0(colnames(datos1),coletillas[1])
  colnames(datos2)=paste0(colnames(datos2),coletillas[2])
  
  #De donde a donde van nuestros datos?
  fechamin=max(c(range(datos1[,col_fechas1])[1],range(datos2[,col_fechas2])[1]))
  fechamax=min(c(range(datos1[,col_fechas1])[2],range(datos2[,col_fechas2])[2]))
  if (fechamin>=fechamax) {stop("Los datos no se solapan en el tiempo")}
  
  #Cuales son las fechas del primer input mas cercanas a los limites de nuestros datos?
  #Fechainicio?
  if (fechamin==range(datos1[,col_fechas1])[1]) { #Si se cumple la condicion, nos ahorramos el procesamiento que requiere else{}, aunque deberian dar lo mismo
    fechainicio=fechamin
  }else{
    pos=(datos1[,col_fechas1]-fechamin) %>% as.numeric %>% abs %>% which.min
    fechainicio=datos1[,col_fechas1][pos]
  }
  #Fechafinal?
  if (fechamax==range(datos1[,col_fechas1])[2]) { #Si se cumple la condicion, nos ahorramos el procesamiento que requiere else{}, aunque deberian dar lo mismo
    fechafinal=fechamax
  }else{
    pos=(datos1[,col_fechas1]-fechamax) %>% as.numeric %>% abs %>% which.min
    fechafinal=datos1[,col_fechas1][pos]
  }
  
  #Ahora que tenemos las fechas podemos crear datos3
  datos3=cbind(datos1[1,],datos2[1,])  #En vez de crear de cero, juntamos la primera linean de ambas. Menos problemas de formato!
  datos3[1,]=NA   #Vaciamos porseaca
  datos1=datos1[which(datos1[,col_fechas1]==fechainicio):which(datos1[,col_fechas1]==fechafinal),]  #Coger las fechas de datos1 que esten en la parte solapada
  datos3[1:nrow(datos1),1:ncol(datos1)]=datos1  #Rellenar la mitad izquierda de datos3, la relativa a la aprte solapada de datos1
  
  #Para cada fecha de datos1, buscamos el dato de datos2 mas cercano, y asi vamos rellenando datos3
  if (isTRUE(interpolar)) {
  stop("Todavia no se ha desarrollado para interpolar = TRUE !")
  }else{
    for (i in 1:nrow(datos3)) {
      n=(datos2[,col_fechas2]-datos3[,col_fechas1][i]) %>% as.numeric %>% abs %>% which.min
      datos3[i,(ncol(datos1)+1):ncol(datos3)]=datos2[n,]
    }
  }
  return(datos3)
}

crear_tabla_cors_por_dirs=function(datos_juntos,porcentajes=T){
  
  #Esta función coge un data.frame de tipo datos_juntos (aquel donde se juntan las mediciones
  #de un anemos con las mediciones correspondientes de n puntos de era) y devuelve una tabla
  #con las correlaciones entre el anemo y los puntos para cada dirección. La función reconoce
  #la columna llamada "Mean" como las mediciones de anemos y todas las que contengan "uv_wind"
  #como viento de era.
  
  #Si porcentajes=T, se añade una columna extra que especifica que cantidad de las lineas
  #corresponden a cada dirección.
  
  #Comparar anemo con todos los puntos por direcciones
  cors_anemo_vs_puntos=data.frame()
  col_mean=grep("Mean",colnames(datos_juntos)) #Numeros de columna cuyo nombre contenga "Mean"
  dirs=unique(na.omit(datos_juntos$Dir))
  dirs=dirs[order(as.numeric(dirs))]
  
  #Bucle para correlaciones
  kont_i=1
  for (i in grep("uv_wind",colnames(datos_juntos))) { #Vector: Numeros de columnas cuyos nombres contengan "uv_wind
    kont_j=1
    for (j in dirs) {
      cors_anemo_vs_puntos[kont_j,kont_i]=cor(datos_juntos[which(datos_juntos$Dir==j),i],datos_juntos[which(datos_juntos$Dir==j),col_mean],"na")
      kont_j=kont_j+1
    }
    kont_i=kont_i+1
  }
  colnames(cors_anemo_vs_puntos)=1:(ncol(cors_anemo_vs_puntos))
  row.names(cors_anemo_vs_puntos)=dirs
  
  #Bucle para porcentajes
  if (porcentajes){
    porcentajes_dir=c()
    for (i in 1:length(dirs)) {
      porcentajes_dir[i]=nrow(filter(datos_juntos,Dir==dirs[i]))*100/nrow(datos_juntos)
    }
    cors_anemo_vs_puntos=cbind(cors_anemo_vs_puntos,porcentajes_dir)
    colnames(cors_anemo_vs_puntos)[ncol(cors_anemo_vs_puntos)]="%"
  }
  
  return(cors_anemo_vs_puntos)
}

computeMASE <- function(forecast,train,test,period){
  #Encontrado en:
  #https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments
  
  # forecast - forecasted values
  # train - data used for forecasting .. used to find scaling factor
  # test - actual data used for finding MASE.. same length as forecast
  # period - in case of seasonal data.. if not, use 1
  
  forecast <- as.vector(forecast)
  train <- as.vector(train)
  test <- as.vector(test)
  
  n <- length(train)
  scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)
  
  et <- abs(test-forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt)
  return(meanMASE)
}

remixear_datos_juntos=function(datos_juntos){
  
  #Esta funcion recibe un dataframe de tipo datos_juntos y escoge los datos necesarios para devolver un datos_remix.
  #EXPLICACION datos_juntos: dataframe que tiene para cada linea los datos de anemo (Date, Mean, Gust y Dir) y de
  #n puntos de era (Date_eran,lonn,latn,uv_windn y uv_dwin, sustituyendo la n final con el numero))
  #EXPLICACION datos_remix: dataframe que tiene para cada linea los datos de anemo (Date, Mean, Gust y Dir) y del
  #punto de era que haya demostrado tener mejor correlacion  con las mediciones de datos_juntos. lat y lon permiten
  #conocer el punto de era con el que se ha completado cada linea.
  
  #Que punto vamos a usar para cada direccion?
  cors_por_dirs=crear_tabla_cors_por_dirs(datos_juntos,porcentajes = F) #Tabla sin porcentajes!
  puntos_era_para_cada_dir=apply(cors_por_dirs,1, which.max)  #Esto devuelve un "named int": contiene tanto la direccion como el punto de era mas apropiado.
  
  #Construir datos_remix
  datos_remix=datos_juntos[,c("Date","Mean","Gust","Dir")]
  datos_remix[,c("Date_era","lon","lat","uv_wind","uv_dwi")]=NA
  class(datos_remix$Date_era)= class(datos_juntos$Date)  #Esto es necesario para que luego no nos ponga la fecha en numerico
  for (i in 1:nrow(datos_remix)) {
    punto=puntos_era_para_cada_dir[which(names(puntos_era_para_cada_dir)==datos_juntos$Dir[i])]
    columnas=grep(as.character(punto),colnames(datos_juntos))
    datos_remix[i,c("Date_era","lon","lat","uv_wind","uv_dwi")]=datos_juntos[i,columnas,drop = FALSE]  #Esta linea nos jode las fechas, las pone en numerico
  }
  return(datos_remix)
}


# Funciones mapas ---------------------------------------------------------

#Descargar mapas seteando coordenadas
download_maps<- function(ul,lr, 
                         maptyp=NULL,
                         res=40){
  if(is.character(maptyp)){
    maptypes<- maptyp
  }else{
    maptypes<- c("waze", "bing",
                 "esri","esri-topo", "nps", 
                 "apple-iphoto", "skobbler",
                 "hillshade")
  }
  if(length(maptypes)>1){
    res1<- res
    for (i in 1:length(maptypes)) {
      res<- res1
      
      
      tryCatch({
        while(TRUE){
          tryCatch({
            map1<- openmap(ul,lr, minNumTiles=res,
                           type=maptypes[i],
                           zoom=NULL)
            
          },error=function(e){cat("Error Java")})
          
          if(!exists("map1")){
            res<- res-1
            print("Bajando minNumtiles")
            if(res<1){break}
          }else{
            print(paste0(maptypes[i],": Descargado con minNumtiles=", res))
            break}
        }
        map.latlon <- openproj(map1, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        rm(map1)
       
        if(!dir.exists(here::here("NUEVO/Mapas/"))){dir.create(here::here("NUEVO/Mapas/"))}
        dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
        
        if(!dir.exists(dirpath)){dir.create(dirpath)}
        
        saveRDS(map.latlon, file=paste0(dirpath,"/",maptypes[i],res,".RDS"))
        print(paste0("Guardado ",paste0(dirpath,"/",maptypes[i],res,".RDS")))
        rm(map1) 
          
       
      }, error=function(e){print(paste0(maptypes[i],": No descargado"))})
    }
    
  }else{
    while(TRUE){
      tryCatch({
        map1<- openmap(ul,lr, minNumTiles=res,
                      type=maptypes,
                      zoom=NULL)
        
      },error=function(e){cat("Error Java")})
      
      if(!exists("map1")){
        res<- res-1
        print("Bajando minNumtiles")
        if(res<1){break}
      }else{
        print(paste0(maptypes,": Descargado con minNumtiles=", res))
        break}
    }
    
    map.latlon <- openproj(map1, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    rm(map1)
    
    dirpath<- here::here(paste0("NUEVO/Mapas/",ul[1],"_",lr[2],"/"))
    
    if(!dir.exists(dirpath)){dir.create(dirpath)}
    saveRDS(map.latlon, file=paste0(dirpath,"/",maptypes,res,".RDS"))
    print(paste0("Guardado ",paste0(dirpath,"/",maptypes,res,".RDS")))
      
    }
    
}
  


#Plotear mapas con puntos de ERA5 y anemos
map_wpoints<- function(map.latlon, 
                       Coord_era,
                       Coord_anemo){
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


#Rosa de los vientos para plotear sobre mapas
plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 0.5,
                          dirres = 22.5,
                          spdmin = 0,
                          spdmax = 15,
                          spdseq = NULL,
                          palette,
                          countmax = NA,
                          opacity=0.6,
                          border_color="NA",
                          border_size=0.001){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  }
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  data<-data[,c("Date","lon","lat",spd,dir)]
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } 
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,n.colors.in.range),
                                                min(9,n.colors.in.range)),
                                            palette ))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("ggplot2 version > V2.2")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar(width = 1,color=border_color, size=border_size, alpha=opacity) + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) + 
    theme(legend.position = "none",
          plot.background = element_rect(fill= "transparent", colour= NA),
          panel.background = element_rect(fill= "transparent", colour = NA),
          panel.grid.major = element_line(colour = "NA"), 
          axis.line = element_line(colour = NA),
          axis.text.y=element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.x = element_blank()) +
    xlab("")+ ylab("") +
    coord_polar(start = -((dirres/2)/360) * 2*pi)+
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE)
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

#Ajuste de parametros windrose para plotear sobre mapa
#Esto devuele formato ggplot¡¡
WR_parameters<- function(data,
                         anchura=0.06, 
                         opacidad=0.5, 
                         paleta){
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




find_mapfolder<- function(){
  map_folder<- list.dirs(here::here("NUEVO/Mapas/"))
  map_folder1<- str_split(map_folder, "/")
  map_folder2<- map_folder[str_detect(sapply(map_folder1, function(x) x[length(x)]),"[[:digit:]]")]
  return(map_folder2)
}



WR_parameters2<- function(data,
                         anchura=0.06, 
                         opacidad=0.5, 
                         paleta,
                         lon_pos,
                         lat_pos,
                         spd_name,
                         dir_name,
                         border_size=0.5){
  p_ros<- data%>%group_by(., lon,lat)%>% do(subplots= plot.windrose(., spd = spd_name,
                                                                    dir=dir_name,
                                                                    dirres = 22.5,
                                                                    spdseq= c(0,0.3,1,2,3,5,7,10,15,20),
                                                                    palette = paleta,
                                                                    opacity = opacidad,
                                                                    border_color = "white",
                                                                    border_size = border_size))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x = lon_pos-anchura,      # change from 1 to other 
                                             y = lat_pos-anchura,      # values if necessary,
                                             xmax = lon_pos+anchura,   # depending on the map's
                                             ymax = lat_pos+anchura))) # resolution.
  
  
  return(p_ros)
}




# Funciones graficos ----

plot_n_graficos=function(x,n=1,...,col=NULL,type=NULL,main="",leyenda=NULL){
  
  #IDEA: nos vendría bien en algun momento poder definir títulos de eje? Dos opciones: o definidas
  #por usuario, oen el caso del eje horizontal poner primer y última fecha representadas en cada grafico.
  
  #Explicacion argumentos:
  #n=número de gráficos entre los que se quieren dividir los datos
  #x=vector con valores para el eje horizontal (normalmente fechas)
  #...=vectores con valores para el eje vertical (normalmente velocidades de viento)
  #col=vector con colores para cada vector de ...
  #type=vector con tipos de graficos (de lineas, de puntos etc.) para cada vector de ...
  #main=string con el título para los gráficos
  #leyenda=vector de strings para la leyenda de los graficos. Si es NULL (opcion por defecto) no habrá leyenda
  
  y=list(...) #Todos los inputs sin nombrar se consideraran valores para el eje vertical. Guardar en lista.
  
  #Analizar los inputs.Errores?
  
  #Falta verificar el input x, pero ahora no se como hacerlo. Al parecer las columnas con fechas no son vectores.
  
  if (!is.numeric(n)) {stop("El input de numero de graficos (n) no es un número")}
  
  if (!is.vector(y)) {stop("Algun input de eje vertical (y) no es un vector")}
  if (!is.numeric(unlist(y))) {stop("Algun input de eje vertical (y) no es un vector numérico")}
  
  if (!is.null(col) & !is.character(col)) {stop("El input de colores (col) no es de tipo character")}
  if (sum(!(col %in% colors()))>0) {stop("Hay ",sum(!(col %in% colors()))," strings en el input de colores (col) que no son parte de la gama de colores de R. Usa colors() para ver colores posibles")}
  
  if (!is.null(type) & !is.character(type)) {stop("El input de tipo de grafico (type) no es de tipo character")}
  if (sum(!(type %in% c("p","l","b","c","o","h","s","S","n")))>0) 
  {stop("Hay ",sum(!(type %in% c("p","l","b","c","o","h","s","S","n")))," strings en el input de tipo de grafico (type) que no son aceptables. Usa help(\"plot\") para ver posibilidades")}
  
  #Falta verificar input main
  
  #Falta verificar input leyenda
  
  #Definir color de cada gráfico. Vamos a usar primero los colores definidos por el usuario. Si no son suficientes, colores del arcoiris.
  col=c(col,rainbow(length(y)-length(col)))
  
  #Definir tipo de cada gráfico.Vamos a usar primero los colores tipos por el usuario. Si no son suficientes, usamos lineas.
  type=c(type,rep("l",(length(y)-length(type))))
  
  #IDEA:si se quiere usar esta función para trabajar con datos no ordenados cronológicamente, incorporar código que ordene todos los vectores de input segun order(x)
  
  for (i in 1:n) {
    for (j in 1:length(y)) {
      valores=((i-1)*length(x)/n):(i*length(x)/n) #Posiciones de los valores a plotear para cada grafico
      if (j==1) {
        plot(x[valores],y[[j]][valores],col=col[j],type=type[j],
             ylim = c(0,max(unlist(y))),  #Cual es el valor mas alto a plotear? Usar como ylim de todos los graficos
             xlab=paste0(min(x[valores])," - ",max(x[valores])),
             ylab="",
             main = paste0(main," ",i,"/",n)) 
      }else{
        lines(x[valores],y[[j]][valores],col=col[j],type=type[j])
      }
    }
    if (is.character(leyenda)) {  #Esto es para no intentar meter una leyenda erronea, o nula
      legend("topright",legend = leyenda,fill = col)
    }
  }
}
