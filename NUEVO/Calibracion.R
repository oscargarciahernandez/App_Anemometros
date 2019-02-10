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

datos_anemos=Anemometros$`0B38DAE79059`

mean_max=50/3.6   #[m/s]
gust_max=200/3.6  #[m/s]
dif_max=30/3.6    #[m/s]

N_mean=c()
N_gust=c()

#Plotear racha con todos los datos
plot(datos_anemos$Gust,x = datos_anemos$Date,type="p",col="blue")
#Plotear media con todos los datos
lines(datos_anemos$Mean,x = datos_anemos$Date,type="p")
#Fitros de viento medio
#Nivel 1 -- limites
#Para mean, Velocidad [0,50/3.6] (m/s)
N_mean=cbind(N_mean,which(datos_anemos$Mean>mean_max | datos_anemos$Mean<=0 ))

#Para gust, Velocidad [0,200/3.6] (m/s)
N_gust=cbind(N_gust,which(datos_anemos$Gust>200/3.6 | datos_anemos$Gust<=0))

#Por ahora no hacemos con la direccion por que parece que esta bien
#Direccion [0,360]
#N<-which(Datosdf$dir>360 | Datosdf$dir<0)
#if (length(N)!=0){
#  Datosdf$dir[N]<-NA
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


#Plotear mediciones consideradas erroneas
points(x = datos_anemos$Date[N_gust],y = datos_anemos$Gust[N_gust],col="green",lwd=5)   #Los errores de mean en rojo
points(x = datos_anemos$Date[N_mean],y = datos_anemos$Mean[N_mean],col="red",lwd=5)   #Los errores de mean en rojo


#Encontrar una manera de reponer los datos filtrados por ellos, poniendo Na----

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
  datos_anemos_rellenado=do.call("rbind", lista) #Juntar todos los cachitos. Haciendo esto en vez de monton de rbins usamos menos recursos del pc.
  colnames(datos_anemos_rellenado)=colnames(datos_anemos)
  return(datos_anemos_rellenado)
}

#Marcar en morado a una altura de 20 alli donde haya huecos
points(x = datos_anemos$Date[N_huecos],y = seq(20,20,length.out = length(datos_anemos$Date[N_huecos]) ),col="purple",lwd=5)

#Obtenemos puntos del ERA cercanos al Anemo----
pos_anem_uni<-c(43.179361, -2.488510)#lat,lon


nearest_lat<- data_ERA_2018_ls$latitude[order(abs(data_ERA_2018_ls$latitude - pos_anem_uni[1]))[1:2]]
nearest_lon<- data_ERA_2018_ls$longitude[order(abs(data_ERA_2018_ls$longitude - pos_anem_uni[2]))[1:2]]

