library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros----

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  

#Encontrar errores datos anemos----

<<<<<<< HEAD
datos_anemos=rellenar_huecos_anemos(Anemometros$`0B75FE3A4FB6`)
=======
datos_anemos=rellenar_huecos_anemos(Anemometros$`0B38DAE79059`)
>>>>>>> 3aa45475b74ac1856b8cb083ef7e703d7430583f
#huecos=buscar_huecos_anemos(Anemometros$`0B38DAE79059`)  #Para saber donde nos ha metido NAs la funcion rellenar_huecos_anemos

mean_max=50/3.6   #[m/s]
gust_max=200/3.6  #[m/s]
dif_max=30/3.6    #[m/s]
dif_min=0/3.6     #[m/s]
tomas_dif_min=10  #[-]  Numero de tomas consecutivas en las que el viento varia en menos de dif_min

N_mean=c() #Aqui guardamos las posiciones de las mediciones de mean que parecen errores.
N_gust=c()

#Fitros de viento medio
#N_mean=cbind(N_mean,which(datos_anemos$Mean>mean_max | datos_anemos$Mean<=0 ))   #<=0
N_mean=cbind(N_mean,which(datos_anemos$Mean>mean_max | datos_anemos$Mean<0 ))     #<0

#N_gust=cbind(N_gust,which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<=0))     #<=0
N_gust=cbind(N_gust,which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<0))       #<0

#Nivel 2 -- coherencia temporal del dato 
#Step test:
#Velocidad diferencia con el dato anterior de dif_max m/s tanto si la diferencia es + como si es -
#Se lo pasamos al mean
for (i in 2:length(datos_anemos$Mean)){
  difer<-datos_anemos$Mean[i-1]-datos_anemos$Mean[i]
  if (is.na(difer)==FALSE & abs(difer)>30/3.6){
    N_mean[length(N_mean)+1]=i
  }
}

#Ahora al gust
for (i in 2:length(datos_anemos$Gust)){
  difer<-datos_anemos$Gust[i-1]-datos_anemos$Gust[i]
  if (is.na(difer)==FALSE & abs(difer)>30/3.6){
    N_gust[length(N_gust)+1]=i
  }
}

#Nivel 4 -- coherencia temporal de la serie
# En tomas_dif_min tomas que la velocidad no varie en dif_min (Mean)
i=1
while (i<=(nrow(datos_anemos)-tomas_dif_min+1)){
  if(is.na(datos_anemos$Mean[i])==FALSE){
    difer<-max(datos_anemos$Mean[i:(i+tomas_dif_min-1)],na.rm=TRUE)-min(datos_anemos$Mean[i:(i+tomas_dif_min-1)],na.rm=TRUE)
    if (is.na(difer)==FALSE & abs(difer)<=dif_min){
      N_mean[(length(N_mean)+1):(length(N_mean)+tomas_dif_min)]=(i:(i+tomas_dif_min-1))
      i=i+tomas_dif_min-1 #Se hace un -1 para compensar el +1 que se le va haver al final del while
    }
  }
  i=i+1
}
rm(i)

# En tomas_dif_min tomas que la velocidad no varie en dif_min (Gust)
i=1
while (i<=(nrow(datos_anemos)-tomas_dif_min+1)){
  if(is.na(datos_anemos$Gust[i])==FALSE){
    difer<-max(datos_anemos$Gust[i:(i+tomas_dif_min-1)],na.rm=TRUE)-min(datos_anemos$Gust[i:(i+tomas_dif_min-1)],na.rm=TRUE)
    if (is.na(difer)==FALSE & abs(difer)<=dif_min){
      N_gust[(length(N_gust)+1):(length(N_gust)+tomas_dif_min)]=(i:(i+tomas_dif_min-1))
      i=i+tomas_dif_min-1 #Se hace un -1 para compensar el +1 que se le va haver al final del while
    }
  }
  i=i+1
}
rm(i)

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

#Donde hay NAs? Marcar en rojo mas tarde
N_na=which(rowSums(is.na(datos_anemos[,c(2,3,4)]))>0)

#Ploteos de datos_anemos----

#Plotear de n en n
graphics.off()
n=nrow(datos_anemos)/10   #No hace falta redondear, los corchetes [] redondean siempre para abajo
#n=nrow(datos_anemos)   #Para plotear todo junto
#NUestros datos estan al reves! (Primero los mas nuevos). Asi que los vamos a plotar del reves:
#Ultimo plot=datos mas nuevos (el primero que vemos en la ventana de plots)
#En cada plot los datos en orden cronologico, es decir, en orden contrario al que aparecen en el data.frame
for (i in seq(nrow(datos_anemos),1,-n)) {
  layout(mat = c(1,2))  #Separar la ventana de plts en dos, una para mean, otro para gust
  #Mean
  plot(datos_anemos$Mean[(i-n+1):i],x = datos_anemos$Date[(i-n+1):i],type="p",xlab="",ylab="Mean [m/s]")
  points(x = datos_anemos$Date[N_mean],y = datos_anemos$Mean[N_mean],col="green",lwd=1)   #Los errores de mean en rojo
  points(x = datos_anemos$Date[N_na],y = rep_len(0,length(datos_anemos$Date[N_na])),col="red",lwd=1)
  title(main = paste0(datos_anemos$Date[i]," - ",datos_anemos$Date[i-n+1]))
  #Gust
  plot(datos_anemos$Gust[(i-n+1):i],x = datos_anemos$Date[(i-n+1):i],type="p",col="blue",xlab="",ylab="Gust [m/s]")
  points(x = datos_anemos$Date[N_gust],y = datos_anemos$Gust[N_gust],col="green",lwd=1)
  points(x = datos_anemos$Date[N_na],y = rep_len(0,length(datos_anemos$Date[N_na])),col="red",lwd=1)
}
rm(n)

#Rosas de los vientos
windRose(datos_anemos,ws = "Mean",wd="Dir",paddle = F,key.header = "Mean [m/s]")
windRose(datos_anemos,ws = "Gust",wd="Dir",paddle = F,key.header = "Gust [m/s]")
windRose(a,ws = "Mean",wd="Dir",paddle = F,key.header = "Mean [m/s]")

windRose(Anemometros$`0B75FE3A4FB6`,ws = "Mean",wd="Dir",paddle = F,key.header = "Mean [m/s]")

#Quitar mediciones erroneas, guardar, cargar----
#Llenar de NAs las mediciones consideradas erroneas. No eliminamos la fila; queremos mantener la fechas de los NAs.
datos_anemos[N_mean,c(2,3,4)]=NA
datos_anemos[N_gust,c(2,3,4)]=NA
datos_anemos[N_na,c(2,3,4)]=NA    #Esto parece redundante pero viene bien asegurarse

#Guardar los resultados
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){
  dir.create(here::here("NUEVO/Data_calibracion"))
}
save(datos_anemos,
     file=here::here("NUEVO/Data_calibracion/datos_uni_tratados.Rdata"))

#Cargarlos
load(here::here("NUEVO/Data_calibracion/datos_uni_tratados.Rdata"))

#Tratar datos era----
#Coordenadas que queremos representar

#Pongo este if por que el comando unique tarda lo suyo, para evitar que se ejecute mas de lo necesario
if (!exists("coordenadas_era")) {
  Coordenadas_era=unique(ERA5_df[,c(2,3)])
}
#Ordenarlos de cercanos a lejanos
Coordenadas_era=Coordenadas_era[order((Coordenadas_era$lon-Coordenadas_anemos[1,2])^2+(Coordenadas_era$lat-Coordenadas_anemos[1,1])^2),]
#Coger los n mas cercanos
n=4
Coordenadas_era=Coordenadas_era[1:n,]

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
datos_era=ERA5_df[which((ERA5_df$lon==Coordenadas_era$lon)&(ERA5_df$lat==Coordenadas_era$lat)),]

#Plotear datos_era----
#Plotear de n en n
graphics.off()
n=nrow(datos_era)/5   #No hace falta redondear, los corchetes [] redondean siempre para abajo
#n=nrow(datos_era)   #Para plotear todo junto
#NUestros datos estan ordenados cronologicamente! 
for (i in seq(1,nrow(datos_era),n)) {
  layout(c(1,2))
  #uv
  plot(datos_era$uv_wind[i:(i+n-1)],x = datos_era$Date[i:(i+n-1)],type="p",xlab="",ylab="uv_wind")
  title(main = paste0(datos_era$Date[i]," - ",datos_era$Date[i+n-1]))
  #wind
  plot(datos_era$wind[i:(i+n-1)],x = datos_era$Date[i:(i+n-1)],type="p",xlab="",ylab="wind")
}
rm(n)

#Rosas de los vientos
windRose(datos_era,ws = "uv_wind",wd="uv_dwi",paddle = F,key.header = "uv_wind")
windRose(datos_era,ws = "wind",wd="dwi",paddle = F,key.header = "wind")

#Pongo este if por que el comando unique tarda lo suyo, para evitar que se ejecute mas de lo necesario
if (!exists("coordenadas_era")) {
  Coordenadas_era=unique(ERA5_df[,c(2,3)])
}
#Ordenarlos de cercanos a lejanos
Coordenadas_era=Coordenadas_era[order((Coordenadas_era$lon-Coordenadas_anemos[1,2])^2+(Coordenadas_era$lat-Coordenadas_anemos[1,1])^2),]
#Coger los n mas cercanos
n=4
Coordenadas_era=Coordenadas_era[1:n,]

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
datos_era=ERA5_df[which((ERA5_df$lon==Coordenadas_era$lon)&(ERA5_df$lat==Coordenadas_era$lat)),]
