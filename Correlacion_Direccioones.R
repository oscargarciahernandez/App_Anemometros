library(dplyr)
library(magrittr)
library(TTR)
library(here)


# Clasificar por direcciones ----------------------------------------------

#Este script esta pensado para clasificar por direcciones del viento a ver
#si de esta manera aumenta la correlación.
#Para ello emplearemos dplyr

source(here::here("ERA5_2018.R"))


head(Datos_calibracion_uni[[1]])

vectorr_dir<- unique(Datos_calibracion_uni[[1]]$Dir_lab)
tabla_corr_1<- data.frame(matrix(ncol = 5))
tabla_corr_1[1,1:5]<- c(NA,NA,NA,NA,NA)
colnames(tabla_corr_1)<- c("Dir","Corr_1","cor_SMA","points_SMA","length")

for (i in 1:length(vectorr_dir)) {
  Datos_orde<- Datos_calibracion_uni[[1]] %>% filter(Dir_lab==vectorr_dir[i])
  
  
    
    
    a<- Datos_orde$Mean 
    b<- Datos_orde$wind_abs
    corr_1<-cor(a,b)
    
    SMA_result<- SMA_best(Datos_orde)

    
    correlacion<- cbind(NA,corr_1,SMA_result,length(a))
    colnames(correlacion)<- names(tabla_corr_1)
    tabla_corr_1<- rbind(tabla_corr_1,correlacion)
    
    
  
  tabla_corr_1[i+1,1]<- as.character(vectorr_dir[i])
  
  
}








SMA_best<- function(Datos_ordenados){
  
  
  a<- Datos_orde$Mean
  b<- Datos_orde$wind_abs
  vector_cor<-vector()
  for (j in 1:round(length(a)/6)) {
    c<- SMA(a,n=j)
    z<-cbind(c,b)
    z<- z[complete.cases(z),]
    if(length(z)<= 2){
    vector_cor[j]<- NA
    }else{
      vector_cor[j]<- cor(z[,1],z[,2])}
    
  }
  pos_max<- which.max(abs(vector_cor))
  vuelta<- as.data.frame(cbind(vector_cor[pos_max],pos_max))
  colnames(vuelta)<- c("cor_SMA","points_SMA")
  return(vuelta)
  
}






# equiparando dir labels --------------------------------------------------
#funcion para que las etiquetas de direccion del viento sean iguales para los datos
# de los anemometros y los datos del ERA5

tabla_calibracion<- Datos_calibracion_uni[[1]]
equal_dir_lab<-function(tabla_calibracion){
  a<- tabla_calibracion$Dir_ch
  a_1<-str_remove_all(a,"h")
  
  for (i in 1:length(a_1)) {
    if(nchar(a_1[i])==4){
      a_2[i]<- str_sub(a_1[i],1,1)
    } else{
      if(str_detect(a_1[i],"-")){
        a_2[i]<- paste(str_sub(a_1[i],1,1),str_sub(a_1[i],6,6),str_sub(a_1[i],10,10))
      }else{
        a_2[i]<- paste(str_sub(a_1[i],1,1),str_sub(a_1[i],5,5))
        
      }
    }
    
  }
  a_3<-str_remove_all(str_to_upper(a_2)," ")
  
  a_4<- vector()
  for (i in 1:length(a_1)) {
    
    if(nchar(a_3[i])>1){
      x<- str_sub(a_3[i],1,1)
      if(x=="E" || x=="W"){
        a_4[i]<-str_remove_all(paste(str_sub(a_3[i],2,2),str_sub(a_3[i],1,1),str_sub(a_3[i],3,3))," ")
      }else{a_4[i]<- a_3[i]}
      
    }else{a_4[i]<- a_3[i]}
    
    
  }
  
  tabla_calibracion$Dir_ch<- a_4
  
  return(tabla_calibracion)
}
