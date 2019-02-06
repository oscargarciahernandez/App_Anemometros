library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  


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

datos_anemos=

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

if (class(datos_anemos)!="list") {
  for (i in 1:length(datos_anemos)) {
    datos_anemos[[i]]=filtrar_datos(datos_anemos[[i]])
  }
}



#Encontrar una manera de reponer los datos filtrados por ellos, poniendo Na
for (i in 2:(dim(datos_anemos)[1]-1)) {
  diff=as.numeric(datos_anemos$Date[i-1]-datos_anemos$Date[i])
  if (class(diff)!="numeric") {
    print(paste0("ERROR!  class(diff)",class(diff)))
    datos_anemos=datos_anemos[-c(i,i-1),]
    i=i-1
  }
  if (diff<=0) {
    print(paste0("ERROR! datos_anemos$Date[",as.character(i-1),"]-datos_anemos$Date[",as.character(i),"]=",as.character(diff),"minutos"))
    datos_anemos=datos_anemos[-c(i,i-1),]
    i=i-1
  }
  if (diff>7*1.5) {
    print(paste0("ERROR! datos_anemos$Date[",as.character(i-1),"]-datos_anemos$Date[",as.character(i),"]=",as.character(diff),"minutos"))
    i=i-1
  }
}
