
# Explicación -------------------------------------------------------------

#Este script se encarga de cojer los datos del ERA5 y los datos de los 
#sensores y juntarlos en un Dataframe, (aun por pulir). SIMPLEMENTE HAY QUE 
# HACER UN SOURCE Y NOS DEJARÁ DOS VARIABLES EN EL ENVIROMENT PARA EL 
#ANEMOMETRO DEL HEXÁGONO Y EL ANMÓ9METRO DE LA UNI. 
# A PARTIR DE AQUÍ HAY QUE CENTRARSE EN LA CALIBRACIÓN COMO TAL. UN BESITO

# libraries ---------------------------------------------------------------


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
  datos<- list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros_UTC.rdata",collapse = NULL))
  
  lista_anem<- list()
  for (i in 1:length(datos)) {
    lista_anem[[i]]<- extract_hourly_data_1(datos[[i]])
    
  }
  
  names(lista_anem)<- names(datos)
  return(lista_anem)
  
  
}

Datos_horarios<- extract_hourly_data()




#Obtener los puntos cercanos a los anemos ----------------------------------------------

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


# cambiar u y v por módulo y dirección ------------------------------------

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



# tratar datos de ERA5 para asemejarlos a los sensores --------------------
table(cut(cal_hex_1[[1]]$wind_dir_deg2,
          breaks = c(0,seq(11.25,360,by=22.50),361),
          labels = c("N","NNE","NE","NEE","E",
                     "SEE","SE","SSE","S","SSW","SW",
                     "SWW","W","NWW","NW","NNW","N")))

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
