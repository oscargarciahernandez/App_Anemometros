library(TTR)
source(here::here("ERA5_2018.R"))

tabla_corr_1<- data.frame(matrix(nrow = 4))
tabla_corr_2<- data.frame(matrix(nrow = 4))


for (j in 1:50) {
  corr_1<- vector()
  corr_2<- vector()
  for (i in 1:4) {
    a<- Datos_calibracion_uni[[i]]$Mean
    b<- Datos_calibracion_uni[[i]]$wind_abs
    c<- SMA(a,n=j)
    corr_1[i]<-cor(a,b)
    cor(c,b)
    
    z<-cbind(c,b)
    z<- z[complete.cases(z),]
    
    corr_2[i]<- cor(z[,1],z[,2])
    
    
  }
  
  correlacion<- cbind(corr_1,corr_2)
  tabla_corr_1<- cbind(tabla_corr_1,correlacion[,1])
  tabla_corr_2<- cbind(tabla_corr_2,correlacion[,2])
  
  
}


#Ploteo correlacion general
plot(0,ylim = c(0,0.5), xlim = c(1,50))
for (i in 1:4) {
  vector_a<- as.numeric(tabla_corr_2[i,])
  vector_a<- vector_a[complete.cases(vector_a)]
  lines(vector_a)
}

#ploteo correlacion buscando el maximo
plot(0,ylim = c(0.27,0.35), xlim = c(15,22))
for (i in 1:4) {
  vector_a<- as.numeric(tabla_corr_2[i,])
  vector_a<- vector_a[complete.cases(vector_a)]
  lines(vector_a)
}







# smoothing all data ------------------------------------------------------

library(rlist)
library(lubridate)

a<- list.load(here::here("data/Datos_Anemometros/Datos_anemometros_UTC.rdata"))
a_uni<- a$`0B38DAE79059`


extract_hourly_data_SMA<-function(a_uni,points_MA){
  
  a_uni_SMA<- cbind(a_uni, SMA(a_uni$Mean,n=points_MA))
  names(a_uni_SMA)<- c(names(a_uni),"SMA")
  
  fechainicio<-round_date(range(datos$Date)[1],unit = "hours")
  fechafinal<-round_date(range(datos$Date)[2],unit = "hours")
  Vector_fechas<-seq(fechainicio,fechafinal, by="hours")
  
  
 vector_SMA<- vector()
  
  for (i in 1:length(Vector_fechas)) {
    diferencia<-min(abs(Vector_fechas[i]-datos$Date))
    if (as.numeric(diferencia,units="secs") >= 420) { 
      vector_SMA[i]<- NA
      } else { 
        vector_SMA[i]<- a_uni_SMA[which.min(abs(Vector_fechas[i]-a_uni_SMA$Date)), "SMA"]  
      
    }
  }
  
  return(vector_SMA)
  
}


Datos_horarios_uni_SMA<- extract_hourly_data_SMA(a_uni_SMA)

b<- list.load(here::here("data/Datos_Anemometros/Datos_anemometros_calibracion.rdata"))
Datos_horarios_uni_SMA$`SMA(a_uni$Mean, n = 10)`[Datos_horarios_uni_SMA$date_roud%in%b$uni$`-2.5 _ 43.2_0B38DAE79059`$time]
