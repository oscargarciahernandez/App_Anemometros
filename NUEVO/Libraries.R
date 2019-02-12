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
  colnames(huecos)=c(colnames(datos_anemos)[1],"Date[i]-Date[i+1]")
  cont=1
  for(i in 1:(dim(datos_anemos)[1])-1){
    diferencia=datos_anemos$Date[i]-datos_anemos$Date[i+1]  #En minutos
    if (isTRUE(diferencia<=0)){
      print(paste0("ERROR! datos_anemos$Date[",as.character(i-1),"]-datos_anemos$Date[",as.character(i),"]=",as.character(diferencia)," minutos"))
    }
    if (isTRUE(diferencia>7*1.5)){
      huecos[cont,1]=datos_anemos$Date[i]
      huecos[cont,2]=datos_anemos$Date[i+1]
      cont=cont+1
    }
  }
  return(huecos)
}

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
    for (j in 1:(as.numeric(round((huecos[i,1]-huecos[i,2]))/7)-1)) #Cuantas mediciones faltan? Solo una si huecos[i,2] ~= 2*7*60 mins, 2 si huecos[i,2] ~= 3*7*60 ...
    {relleno[j,1]=datos_anemos$Date[which(datos_anemos$Date==huecos[i,1])]-j*(huecos[i,1]-huecos[i,2])/(as.numeric(round((huecos[i,1]-huecos[i,2]))/7))
    #relleno[j,1]=la fecha posterior en el tiempo al hueco - j*(tamaño del hueco)/(el numero de mediciones que faltan en este hueco)
    }
    colnames(relleno)=colnames(datos_anemos)
    lista[[2*i]]=relleno
    if (i==(nrow(huecos))) {
      lista[[length(lista)+1]]=datos_anemos[which(datos_anemos$Date==huecos[i,2]):nrow(datos_anemos),] #Si estamos con el ultimo hueco, hay que coger todo hasta el final de datos_anemos
    }else{
      lista[[2*i+1]]=datos_anemos[which(datos_anemos$Date==huecos[i,2]):which(datos_anemos$Date==huecos[i+1,1]),] #Las mediciones entre el principio del hueco y el final del siguiente hueco
    }
  }
  datos_anemos_rellenado=do.call("rbind", lista) #Juntar todos los cachitos. Haciendo esto en vez de monton de rbins usamos menos recursos del pc, y tarda mucho menos.
  colnames(datos_anemos_rellenado)=colnames(datos_anemos)
  return(datos_anemos_rellenado)
}

#!!!!!!. Filtrar_datos no esta del todo adaptado por que nuestros datos de anemos no tienen $dir en numerico, sino en character.
filtrar_datos=function(datos_anemos){
  #Esta funcion trabaja con un data.frame de mediciones de anemos que sigan nuestro
  #nuestro formato estandar. Si en vez de eso recibe una lista, se llama a si mismo
  #para cada elemento de la lista.
  if (class(datos_anemos)=="data.frame") {
    #En desarrollo en Calibracion.r
  }else{
    if (class(datos_anemos)=="list") {
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

