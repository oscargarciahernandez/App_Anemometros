library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros----

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  

#Encontrar errores y registrar su posicion----
#Esta funcion hace lo mismo que el script FiltroObservaciones.r de Sheilla, pero
#adaptandolo a nuestros formatos y pudiendo definir vel_max, dif_max.

#Valores originales de Sheila:
#vel_max(media)=50 km/h
#dif_max=30 km/h

#Valores nuevos propuestos por Sheila:
#vel_max(media)=90 km/h
#vel_max(racha)=200 km/h
#dif_max=30 km/h


datos_anemos=rellenar_huecos_anemos(Anemometros$`0B38DAE79059`)




mean_max=50/3.6   #[m/s]
gust_max=200/3.6  #[m/s]
dif_max=30/3.6    #[m/s]

N_mean=c() #Aqui guardamos las posiciones de las mediciones de mean que parecen errores.
N_gust=c()
#N_dir=c()

#Fitros de viento medio
#Nivel 1 -- limites
#Para mean, Velocidad [0,50/3.6] (m/s)
#N_mean=cbind(N_mean,which(datos_anemos$Mean>mean_max | datos_anemos$Mean<=0 ))   #<=0
N_mean=cbind(N_mean,which(datos_anemos$Mean>mean_max | datos_anemos$Mean<0 ))     #<0

#Para gust, Velocidad [0,200/3.6] (m/s)
N_gust=cbind(N_gust,which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<=0))

#Por ahora no hacemos con la direccion por que parece que esta bien
#Direccion [0,360]
#N<-which(datos_anemos$Dir>360 | datos_anemos$Dir<0)
#if (length(N)!=0){
#  datos_anemos$Dir[N]<-NA
#}

#Nivel 2 -- coherencia temporal del dato 
#Step test:
#Velocidad diferencia con el dato anterior de 30 m/s tanto si la diferencia es + como si es -
#Este filtro solo se lo pasamos al mean
for (i in 2:length(datos_anemos$Mean)){
  difer<-datos_anemos$Mean[i-1]-datos_anemos$Mean[i]
  if (is.na(difer)==FALSE & abs(difer)>30/3.6){
    N_mean=cbind(N_mean,i)
  }
}

#Ahora al gust
for (i in 2:length(datos_anemos$Gust)){
  difer<-datos_anemos$Gust[i-1]-datos_anemos$Gust[i]
  if (is.na(difer)==FALSE & abs(difer)>30/3.6){
    N_gust=cbind(N_gust,i)
  }
}

#Nivel 4 -- coherencia temporal de la serie
# Velocidad 
# En 1 horas (6 tomas) que la velocidad no varie en 0.1
for (i in 6:nrow(datos_anemos)){
  if(is.na(datos_anemos$Mean[i])==FALSE){
    difer<-max(datos_anemos$Mean[c((i-5):i)],na.rm=TRUE)-min(datos_anemos$Mean[c((i-5):i)],na.rm=TRUE)
    if (is.na(difer)==FALSE & abs(difer)<=0.1){
      #datos_anemos$Mean[i]<-NA
      N_mean[length(N_mean)+1]=i
    }
  }
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

#Plotear racha con todos los datos
plot(datos_anemos$Gust,x = datos_anemos$Date,type="p",col="blue")
#Plotear media con todos los datos
lines(datos_anemos$Mean,x = datos_anemos$Date,type="p")

#Plotear mediciones consideradas erroneas
points(x = datos_anemos$Date[N_gust],y = datos_anemos$Gust[N_gust],col="green",lwd=1)   #Los errores de gust en verde
points(x = datos_anemos$Date[N_mean],y = datos_anemos$Mean[N_mean],col="red",lwd=1)   #Los errores de mean en rojo
points(x = datos_anemos$Date[N_dir],y = datos_anemos$Mean[N_dir],col="brown",lwd=1)   #Los errores de dir en marron
#Marcar en morado a una altura de 20 alli donde haya huecos
#points(x = datos_anemos$Date[N_huecos],y = seq(20,20,length.out = length(datos_anemos$Date[N_huecos]) ),col="purple",lwd=5)

#Plotear mean de n en n
n=2000
for (i in seq(1,nrow(datos_anemos),n)) {
  plot(datos_anemos$Mean[i:(i+n)],x = datos_anemos$Date[i:(i+n)],type="p")
  points(x = datos_anemos$Date[N_mean],y = datos_anemos$Mean[N_mean],col="red",lwd=1)   #Los errores de mean en rojo
  
}
rm(n)

#Llenar de NAs las mediciones consideradas erroneas. No eliminamos la fila; queremos mantener la fechas de los NAs.
datos_anemos[N_mean,c(2,3,4)]=NA
datos_anemos[N_gust,c(2,3,4)]=NA

#Rosa de los vientos
windRose(datos_anemos,ws = "Mean",wd="Dir")

#Guardar los resultados

