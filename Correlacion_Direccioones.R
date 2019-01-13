library(dplyr)
library(magrittr)
library(TTR)


# Clasificar por direcciones ----------------------------------------------

#Este script esta pensado para clasificar por direcciones del viento a ver
#si de esta manera aumenta la correlación.
#Para ello emplearemos dplyr

source(here::here("ERA5_2018.R"))


head(Datos_calibracion_uni[[1]])

vectorr_dir<- unique(Datos_calibracion_uni[[1]]$Dir_lab)
tabla_corr_1<- data.frame(matrix(ncol = 3))
tabla_corr_1[1,1:3]<- c(NA,NA,NA)
colnames(tabla_corr_1)<- c("Dir","Corr_1","Corr_2")

for (i in 1:length(vectorr_dir)) {
  Datos_orde<- Datos_calibracion_uni[[1]] %>% filter(Dir_lab==vectorr_dir[i])
  
  
    
    
    a<- Datos_orde$Mean 
    b<- Datos_orde$wind_abs
    #c<- SMA(a,n=j)
    corr_1<-cor(a,b)
    
    #z<-cbind(c,b)
    #z<- z[complete.cases(z),]
    #if(length(z)<= 2){
      #corr_2<- NA
      
    #}else{    corr_2<- cor(z[,1],z[,2])}
    

    
    correlacion<- cbind(NA,corr_1,NA)
    colnames(correlacion)<- c("Dir","Corr_1","Corr_2")
    tabla_corr_1<- rbind(tabla_corr_1,correlacion)
    
    
  
  tabla_corr_1[i+1,1]<- as.character(vectorr_dir[i])
  
  
}
