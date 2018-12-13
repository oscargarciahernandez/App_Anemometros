library(RNetCDF)
library(stringr)
library(lubridate)
library(here)


# Datos ERA5 --------------------------------------------------------------



data_ERA_2018<- open.nc(here::here("python/Data_ERA5/Data_2018.nc"))
#print.nc(data_ERA_2018)

data_ERA_2018_ls<- read.nc(data_ERA_2018, unpack = TRUE)

time_1<-utcal.nc("hours since 1900-01-01 00:00:0.0",data_ERA_2018_ls$time, type = "n")
ymd_1<-paste(time_1[,1],time_1[,2],time_1[,3],sep = "-")
hms_1<- paste(time_1[,4],time_1[,5],time_1[,6],sep = "-")
time_2<-ymd_hms(paste0(ymd_1," ",hms_1))

#sustituimos la fecha con formato weno weno 
data_ERA_2018_ls$time<- time_2


# Datos anemometros -------------------------------------------------------




extract_hourly_data_1<-function(datos_anem){
  datos<- datos_anem
  fechainicio<-round_date(range(datos$Date)[1],unit = "hours")
  fechafinal<-round_date(range(datos$Date)[2],unit = "hours")
  Vector_fechas<-seq(fechainicio,fechafinal, by="hours")
  
  
  a<- rep(NA,5)
  a<- as.data.frame(t(a))
  names(a)<- names(datos)
  
  tabla<-as.data.frame(matrix(ncol = 7,nrow = length(Vector_fechas)))
  
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
extract_hourly_data<- function(){
  datos<- list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros.rdata",collapse = NULL))
  
  lista_anem<- list()
  for (i in 1:length(datos)) {
    lista_anem[[i]]<- extract_hourly_data_1(datos[[i]])
    
  }
  
  names(lista_anem)<- names(datos)
  return(lista_anem)
  
  
}

Datos_horarios<- extract_hourly_data()




# Merging ERA5 and Anem data ----------------------------------------------

#Cojiendo los 4 puntos más cercanos a la poscion de los anemometros
#con esto creamos una lista de 4 dataframes con fecha y componentes u y v. 
pos_anem_uni<-c(43.179361, -2.488510)#lat,lon


nearest_lat<- data_ERA_2018_ls$latitude[order(abs(data_ERA_2018_ls$latitude - pos_anem_uni[1]))[1:2]]
nearest_lon<- data_ERA_2018_ls$longitude[order(abs(data_ERA_2018_ls$longitude - pos_anem_uni[2]))[1:2]]

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


for (i in 1:length(lista_near_ERA)) {
  for (j in 1:length(Datos_horarios)) {
    Datos_ERA<- lista_near_ERA[[i]][lista_near_ERA[[i]]%in%Datos_horarios[[j]]]
    Datos_anem<- Datos_horarios[[j]][Datos_horarios[[j]]%in%lista_near_ERA[[i]]]
    tabla<- as.data.frame(cbind(Datos_ERA,Datos_anem))
  }
 
  
}
