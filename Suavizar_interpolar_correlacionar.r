#Explicacion----

#Lo que pretende hacer el script:
#En este script voy a intentar aplicar el SMA a los datos sieteminutales
#de los anemometros para despues buscar los datos horarios de esa curba
#suavizada, y ver que correlacion tienen con los del ERA5.

#Por que?
#Sospecho que se pueden conseguir correlaciones mas altas si suavizamos
#primero las mediciones y luego buscamos los datos horarios, que si lo
#hacemos al reves. Al fin y al cabo, de esta manera no desperdiciamos
#informacion.

#conclusiones: parece ser que con este metodo se consiguen correlaciones de %25-30 como maximo,
#utilizando valores de n del orden de 200-300.
#Yo probaria a hacer dos suavidos: uno antes de selecionar los datos de los anemometros
#que encagan con las frecuencia horaria de los ERA5. Este suavizado ulitizaria un n de alrededor de 60/7,
#y serviria para arreglar el desfase entre frecuencias horarias.
#Despues, un segundo suavizado

#Libraries----
library(RNetCDF)
library(stringr)
library(lubridate)
library(here)
library(rlist)
library(TTR)
#Cargar datos ERA5 y crear lista_near_ERA----

#data_ERA_2018<- open.nc(here::here("python/Data_ERA5/Data_2018.nc"))
data_ERA_2018<- open.nc(here::here("python/Data_2018.nc")) #Creo que este cambio elimina un bug
#print.nc(data_ERA_2018)

data_ERA_2018_ls<- read.nc(data_ERA_2018, unpack = TRUE)

time_1<-utcal.nc("hours since 1900-01-01 00:00:0.0",data_ERA_2018_ls$time, type = "n")
ymd_1<-paste(time_1[,1],time_1[,2],time_1[,3],sep = "-")
hms_1<- paste(time_1[,4],time_1[,5],time_1[,6],sep = "-")
time_2<-ymd_hms(paste0(ymd_1," ",hms_1))

#sustituimos la fecha con formato weno weno 
data_ERA_2018_ls$time<- time_2

#Crear lista_near_Era (datos de ERA5 de los puntos cercanos)

#Cojiendo los 4 puntos más cercanos a la poscion de los anemometros
#con esto creamos una lista de 4 dataframes con fecha y componentes u y v. 
pos_anem_uni<-c(43.179361, -2.488510)#lat,lon

nearest_lat<- data_ERA_2018_ls$latitude[order(abs(data_ERA_2018_ls$latitude - pos_anem_uni[1]))[1:2]]
nearest_lon<- data_ERA_2018_ls$longitude[order(abs(data_ERA_2018_ls$longitude - pos_anem_uni[2]))[1:2]]

#Crear lista_near_ERA. Cada elemento que contiene son los datos de 2018 de un punto de ERA5 cercano.
lista_near_ERA<-list()
nombres_list<- vector()
k<-1
for (i in 1:length(nearest_lon)) {
  for (j in 1:length(nearest_lat)) {
    u<-data_ERA_2018_ls$u10[i,j,] #[longitude,latitude,time]
    v<-data_ERA_2018_ls$v10[i,j,] #[longitude,latitude,time]
    time<-data_ERA_2018_ls$time #[longitude,latitude,time]
    tabla<- as.data.frame(cbind(time,u,v))
    tabla[,1]<- as_datetime(tabla[,1])
    lista_near_ERA[[k]]<- tabla
    nombres_list[k]<- paste(round(nearest_lon[i],digits = 1),"_",round(nearest_lat[j],digits = 1))
    k<-k+1
    
  }
  
}
names(lista_near_ERA)<- nombres_list


#Cargar datos anemometros----

datos<- list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros_UTC.rdata",collapse = NULL))

#Suavizaje de datos anemometros----

#Suavizamos datos$Mean y los metemos en una nueva columna
#La funcion de suavizar datos esta hecha para que si decidimos suavizar varias veces los
#mismos datos con distintas n, en vez de acabar teniendo tropezientas columnas, se vaya
#sobreescribiendo la  misma. Ademas, el nombre de la columna nos dice que n ha usado.
suavizar_datos=function(datos,n){
  for (i in 1:length(datos)) {
    datos[[i]][6]=SMA(datos[[i]]$Mean,n=n)
    colnames(datos[[i]])[6]=paste0("SMA(Mean,n=",n,")") #La gracia de este paste0 es que el nombre de la columna reflejara la n que se utilizo para el suavizado
  }
  return(datos)
}

#Toca elegir valor de n para el suavizado.
#n=1, cor=%10-15
#n=9, cor=%15-20
#n=30, cor=%15-20
#n=100, cor=%20-25
#n=200, cor=%25-30
#n=1000 es demasiado, cor=%0,2
#n=300, cor=%25-30
#n=250, cor=%20-25
n=250
datos=suavizar_datos(datos,n)

#Juntar datos del ERA5 y de anemometros en lista_calibracion----
#La funcion extract_hourly_data_2 se  aqui definida se diferencia en extract_hourly_data_1
#de ERA5_2018.R en un par de pequeños cambios para que no se ralle con la columna extra
#que le hemos metido a datos al suavizar. Si esto va en serio estraia bien hacer una 
#version que pueda trabajar con el nuero de columnas que sea
extract_hourly_data_2<-function(datos_anem){
  datos<- datos_anem
  fechainicio<-round_date(range(datos$Date)[1],unit = "hours")
  fechafinal<-round_date(range(datos$Date)[2],unit = "hours")
  Vector_fechas<-seq(fechainicio,fechafinal, by="hours")
  
  
  a<- rep(NA,6)
  a<- as.data.frame(t(a))
  names(a)<- names(datos)
  
  tabla<-as.data.frame(matrix(ncol = 8,nrow = length(Vector_fechas)))
  
  for (i in 1:length(Vector_fechas)) {
    diferencia<-min(abs(Vector_fechas[i]-datos$Date))
    if (as.numeric(diferencia,units="secs") >= 420) { 
      tabla[i,]<-cbind(as.numeric(diferencia,units="secs"),Vector_fechas[i], a) }
    else { 
      tabla[i,]<- cbind(as.numeric(diferencia,units="secs"),Vector_fechas[i],datos[which.min(abs(Vector_fechas[i]-datos$Date)), ])  
      
    }
  }
  
  names(tabla)<- c("diff_sec","date_roud",names(datos))
  
  tabla$date_roud<- as_datetime(tabla$date_roud)
  tabla$Date<- as_datetime(tabla$Date)
  return(tabla)
  
}
Datos_horarios<- list()
for (i in 1:length(datos)) {
  Datos_horarios[[i]]<- extract_hourly_data_2(datos[[i]])
}
names(Datos_horarios)<- names(datos)



lista_calibracion<- list()
nombres_list<- vector()
k<-1
for (i in 1:length(lista_near_ERA)) {
  for (j in 1:length(Datos_horarios)) {
    Datos_ERA<- lista_near_ERA[[i]][lista_near_ERA[[i]]$time%in%Datos_horarios[[j]]$date_roud,]
    Datos_anem<- Datos_horarios[[j]][Datos_horarios[[j]]$date_roud%in%lista_near_ERA[[i]]$time,]
    tabla<- as.data.frame(cbind(Datos_ERA,Datos_anem))
    tabla$diff_sec<-NULL
    tabla$date_roud<-NULL
    lista_calibracion[[k]]<- tabla
    nombres_list[k]<- paste0(names(lista_near_ERA)[i],"_",names(Datos_horarios)[j])
    k<- k+1
  }
  
  
}
names(lista_calibracion)<- nombres_list




anem_hex<- names(Datos_horarios)[1]
anem_uni<- names(Datos_horarios)[2]

cal_hex<- lista_calibracion[str_detect(names(lista_calibracion),pattern = anem_hex)]
cal_uni<- lista_calibracion[str_detect(names(lista_calibracion),pattern = anem_uni)]

#convertir componentes a dirección y modulo
add_wind_dir<- function(x){
  tabla_comp<- x
  u10<- tabla_comp$u
  v10<- tabla_comp$v
  
  wind_abs <- sqrt(u10^2 + v10^2)
  wind_dir_rad <-  atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 <-  wind_dir_rad * 180/pi 
  wind_dir_deg2 <-  wind_dir_deg1+ 180 
  
  tabla_comp<- as.data.frame(cbind(tabla_comp,wind_abs,wind_dir_deg2))
  tabla_comp$u<- NULL
  tabla_comp$v<- NULL
  return(tabla_comp)
  
}
cal_hex_1<-lapply(cal_hex, add_wind_dir)
cal_uni_1<- lapply(cal_uni, add_wind_dir)


# tratar datos de ERA5 para asemejarlos a los sensores
add_dir_lab<- function(lista_cal){
  lista_new<- list()
  for (i in 1:length(lista_cal)) {
    a<- cut(lista_cal[[i]]$wind_dir_deg2,
            breaks = c(0,seq(11.25,360,by=22.50),361),
            labels = c("N","NNE","NE","NEE","E",
                       "SEE","SE","SSE","S","SSW","SW",
                       "SWW","W","NWW","NW","NNW","N"))
    tabla<- as.data.frame(cbind(lista_cal[[i]],a))
    colnames(tabla)<- c(names(lista_cal[[i]]), "Dir_lab")
    tabla$wind_abs<- round(tabla$wind_abs,digits = 1)
    tabla$wind_dir_deg2<- round(tabla$wind_dir_deg2,digits = 1)
    
    lista_new[[i]]<- tabla
    
  }
  
  names(lista_new)<- names(lista_cal)
  return(lista_new)
}

cal_hex_2<- add_dir_lab(cal_hex_1)
cal_uni_2<- add_dir_lab(cal_uni_1)




# Eliminar columnas en las que exista algún NA ----------------------------

cal_hex_3<- lapply(cal_hex_2,function(x){
  return(x[complete.cases(x),])
})

cal_uni_3<- lapply(cal_uni_2,function(x){
  return(x[complete.cases(x),])
})



# Limpiar enviroment ------------------------------------------------------


Datos_calibracion_uni<- cal_uni_3
Datos_calibracion_hex<- cal_hex_3


rm(list=setdiff(ls(),c("Datos_calibracion_hex","Datos_calibracion_uni")))


# Guardar datos -----------------------------------------------------------

a<- list(Datos_calibracion_hex,Datos_calibracion_uni)
path_data <- here::here("data/Datos_Anemometros/Datos_anemometros_calibracion.rdata")
if (as.numeric(object.size(a))>file.info(path_data)$size) {
  names(a)<- c("hex","uni")
  list.save(a,path_data,type = "rdata")
}

# Calcular correlaciones (no acaba de calcular todas, hay que arreglarlo. Aunque creo que la siguiente seccion es mas util)----
n=c()
k=1
for (i in 1:length(a)) {                          #i=1 -> hex ; i=2 -> uni
  i
  for (j in length(a[[i]])) {                     #j nos dice con que punto de ERA5 comparamos
  #El siete viene de la columna donde guardamos los datos suavizados.
    n[k]=cor(a[[i]][[j]]$wind_abs,a[[i]][[j]][7])
    k=k+1
    }
}

#Calcular correlacion a la carta y plotear----

#INSTRUCCIONES: selecciona i (i=1 -> hex ; i=2 -> uni) y j (punto ERA 5)
#para sacar correlacion y plotear los dos vientos (Mean y suavizado)
i=2
j=4
cor_a_la_carta=cor(a[[i]][[j]]$wind_abs,a[[i]][[j]][7])
plot(a[[i]][[j]]$wind_abs,type = "l")
lines(a[[i]][[j]][7],col="red")


