library(rlist)
library(here)
library(lubridate)#Cargar datos.

datos=list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros.rdata",collapse = NULL))
#Para probar cojo solo el primero de la lista
datos=datos$`0B75FE3A4FB6`
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
