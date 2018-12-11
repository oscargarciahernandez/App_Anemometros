library(rlist)
library(here)
library(lubridate)

#Cargar datos.
datos=list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros.rdata",collapse = NULL))
#Para probar cojo solo el primero de la lista
datos=datos$`0B75FE3A4FB6`
fechainicio<-round_date(range(datos$Date)[1],unit = "hours")
fechafinal<-round_date(range(datos$Date)[2],unit = "hours")
Vector_fechas<-seq(fechainicio,fechafinal, by="hours")
a<-rep(NA,5)
tabla<-data.frame()
for (i in 1:length(Vector_fechas)) {
  diferencia<-min(abs(Vector_fechas[i]-datos$Date))
  if (diferencia>=dminutes(7)) { tabla[i,]<-cbind(Vector_fechas[i], t(a)) } else { tabla[i, ]<-cbind(Vector_fechas[i],datos[which.min(abs(Vector_fechas[i]-datos$Date)), ])  
    
  }
}


