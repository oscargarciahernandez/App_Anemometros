library(here)
library(rlist)

###########Actualizar los datos
#Bajar datos y crear ultimas_fechas ----
clean_data_list<- list.load(here::here("data/Datos_anemometros.rdata"))

ultimas_fechas=vector()  #Lo creamos vacio para el for
for(i in 1:length(clean_data_list)){    #Crea ultimas_fechas con la ultilma fecha de cada sensor. #Habria k cambiar por lapply
  if (is.data.frame(clean_data_list[[i]])){
    ultimas_fechas[i]=clean_data_list[[i]][1,1]  #El trm_hig 3 da problemas. Nos lo saltamos.
  }else{ultimas_fechas[i]=NA}
}
#(Insertar nombre de seccion)----
source(here::here("funciones_app_anemometros.R"))  #Cargar funciones
disp<-id_iden(640689911849)
disp<-cbind(disp,ultimas_fechas)

dt_list<- list()  #Lo creamos vacio y lo llenamos en el for
for(i in 1:length(disp[,1])){
  if(disp[i,3]==1){  #Si el sensor i es code=1 (un anemometro)
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(_anem(disp[i,2]))
    dt_list[[nomb]]<- dt
  }
  if(disp[i,3]==2){   #Si el sensor i es code=2 (un pluviometro)
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(get_pluvs(disp[i,2]))
    dt_list[[nomb]]<- dt
    
  }
  if(disp[i,3]==3){  #Si el sensor i es code=3 (termo-higometro)
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(get_term_hig(disp[i,2]))
    dt_list[[nomb]]<- dt
    
  }
  
}


#### creamos lista con la nueva informacion de todos los sensores
dt_list_new_values<- list()
for(i in 1:length(disp[,1])){
  if(disp[i,3]==1){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(actualizar_anemos(disp[i,2], disp[i,5]))
    dt_list_new_values[[nomb]]<- dt
    
  }
  if(disp[i,3]==2){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(actualizar_pluvs(disp[i,2],disp[i,5]))
    dt_list_new_values[[nomb]]<- dt
    
  }
  if(disp[i,3]==3){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(actualizar_term_hig(disp[i,2],disp[i,5]))
    dt_list_new_values[[nomb]]<- dt
    
  }
  
}
######añadir nuevos datos 
dt_sum_list<- list()
for(i in 1:length(dt_list_new_values)){
  nuevos_dt<- as.data.frame(dt_list_new_values[[i]])
  viejos_dt<- as.data.frame(clean_data_list[[i]])
  names(viejos_dt)<- names(nuevos_dt)
  pos_eq<- which(nuevos_dt[,1]== viejos_dt[1,1])
  if(length(pos_eq)==0){
    cc<- rbind(nuevos_dt[,],viejos_dt[,])
    dt_sum_list[[i]]<- cc
    
    
  }else{
    cc<- rbind(nuevos_dt[1:pos_eq-1,],viejos_dt[,])
    dt_sum_list[[i]]<- cc
  }
}

name_list<- as.character(disp[,1])
names(dt_sum_list)<- name_list
#### Sobreescribimos el archivo del dataset y lo volvemos a cargar 
### Nuestros datos validos se seguiran



list.save(dt_sum_list, here::here("data/Datos_anemometros.rdata"))



#### esta funcion realiza la actualizacion de todos los datasets

actualizador_total<-function(){
  clean_data_list<- list.load(here::here("RoseoDashboard/data/Datos_anemometros.rdata"))
  
  last_timestamp<-t(as.data.frame(lapply(clean_data_list,"[", 1, 1)))
  disp<-cbind(disp[,1:4],last_timestamp)
  #### creamos lista con la nueva informacion de todos los sensores
  dt_list_new_values<- list()
  for(i in 1:length(disp[,1])){
    if(disp[i,3]==1){
      nomb<- as.character(disp[i,1])
      dt<-as.data.frame(actualizar_anemos(disp[i,2], disp[i,5]))
      dt_list_new_values[[nomb]]<- dt
      
    }
    if(disp[i,3]==2){
      nomb<- as.character(disp[i,1])
      dt<-as.data.frame(actualizar_pluvs(disp[i,2],disp[i,5]))
      dt_list_new_values[[nomb]]<- dt
      
    }
    if(disp[i,3]==3){
      nomb<- as.character(disp[i,1])
      dt<-as.data.frame(actualizar_term_hig(disp[i,2],disp[i,5]))
      dt_list_new_values[[nomb]]<- dt
      
    }
    
  }
  ######añadir nuevos datos 
  dt_sum_list<- list()
  for(i in 1:length(dt_list_new_values)){
    nuevos_dt<- as.data.frame(dt_list_new_values[[i]])
    viejos_dt<- as.data.frame(clean_data_list[[i]])
    names(viejos_dt)<- names(nuevos_dt)
    pos_eq<- which(nuevos_dt[,1]== viejos_dt[1,1])
    if(length(pos_eq)==0){
      cc<- rbind(nuevos_dt[,],viejos_dt[,])
      dt_sum_list[[i]]<- cc
      
      
    }else{
      cc<- rbind(nuevos_dt[1:pos_eq-1,],viejos_dt[,])
      dt_sum_list[[i]]<- cc
    }
  }
  
  name_list<- as.character(disp[,1])
  names(dt_sum_list)<- name_list
  #### Sobreescribimos el archivo del dataset y lo volvemos a cargar 
  ### Nuestros datos validos se seguiran
  
  
  
  list.save(dt_sum_list, here::here("RoseoDashboard/data/Datos_anemometros.rdata"))
  
}

actualizador_total()
