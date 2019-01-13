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
a_hex<- a$`0B75FE3A4FB6`

#Esta funcion lo que hace es devolver una tabla con las fechas
# y con la moving average usando como n, points_MA. 

Data_SMA<-function(tabla_anem,points_MA){
  a_uni<- tabla_anem
  a_uni_SMA<- cbind(a_uni, SMA(a_uni$Mean,n=points_MA))
  names(a_uni_SMA)<- c(names(a_uni),"SMA")
  
  tabla<- as.data.frame(cbind(a_uni_SMA$Date,a_uni_SMA$SMA))
  names(tabla)<- c("Date","SMA")
  return(tabla)
  
}


#Creamos un bucle que sea capaz de calcular las correlaciones variando
# el numero de puntos empleados en la moving averagee
#para todo: 
# calculando moving average de todos los puntos y extrayendo los puntos horarios
# calculando la moving average de la moving average de todos los datos
# calculando la MA de los datos horarios
# calculando la moving average de los datos aportados por el ERA5
corr<- data.frame(matrix(ncol = 7))
k<- 1
for (sma in 1) {
  a_uni_SMA<- Data_SMA( a$`0B38DAE79059`,sma)
  a_hex_SMA<- Data_SMA( a$`0B75FE3A4FB6`,sma)
  
  for (i in 4) {
    
    for (j in 1:200) {
      for (e in 1:200 ) {
        x<- a_uni_SMA[a_uni_SMA$Date%in%Datos_calibracion_uni[[i]]$Date,"SMA"]
        c<- SMA(x,j)
        
        b<- Datos_calibracion_uni[[i]]$wind_abs
        b_1<- SMA(b,e)
        
        z<- Datos_calibracion_uni[[i]]$Mean
        z_1<- SMA(z,j)
        
        tabla_corr_1<-as.data.frame(cbind(c,b_1))
        tabla_corr_1<-tabla_corr_1[complete.cases(tabla_corr_1),]
        correlacion_1<- cor(tabla_corr_1$c,tabla_corr_1$b_1)
        
        tabla_corr_3<-as.data.frame(cbind(b_1,x))
        tabla_corr_3<-tabla_corr_3[complete.cases(tabla_corr_3),]
        correlacion_3<- cor(tabla_corr_3$x,tabla_corr_3$b_1)
        
        tabla_corr_2<-as.data.frame(cbind(b_1,z_1))
        tabla_corr_2<-tabla_corr_2[complete.cases(tabla_corr_2),]
        correlacion_2<- cor(tabla_corr_2$z_1,tabla_corr_2$b_1)
        
        
        corr[k,]<- data.frame(cbind(sma,i,j,e,correlacion_3,correlacion_1,correlacion_2))
        k<- k+1
        
      }
      
    }
    
  }

}

