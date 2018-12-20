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

a<- list.load(here::here("data/Datos_Anemometros/Datos_anemometros_UTC.rdata"))
a_uni<- a$`0B38DAE79059`


extract_hourly_data_1<-function(a_uni){
  datosainicio<-round_date(range(datos$Date)[1],unit = "hours")
  fechafinal<-round_date(range(datos$Da<- a_uni
  datos
  fechte)[2],unit = "hours")
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

