


###########Actualizar los datos
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
