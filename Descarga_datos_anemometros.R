library(here)


source(here::here('funciones_app_anemometros.R'))



######Descargar todos los datos
disp<-id_iden(640689911849)
#los datos anemometros empiezan el 22 de mayo de 2018
#los datos del pluviometro y termometro el 9 de junio de 2018
# los datos de los sensores de elgoibar empezaron  el 22 de julio

fechaini<- as.character(c("22/05/2018","22/05/2018","09/06/2018",
                          "09/06/2018","22/06/2018","22/06/2018","22/6/2018"))
disp<- cbind(disp, fechaini)

dt_list<- list()
for(i in 1:length(disp[,1])){
  if(disp[i,3]==1){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(get_anem(disp[i,2]))
    dt_list[[nomb]]<- dt
  }
  if(disp[i,3]==2){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(get_pluvs(disp[i,2]))
    dt_list[[nomb]]<- dt
    
  }
  if(disp[i,3]==3){
    nomb<- as.character(disp[i,1])
    dt<-as.data.frame(get_term_hig(disp[i,2]))
    dt_list[[nomb]]<- dt
    
  }
  
}
clean_data_list<-lapply(dt_list, clean_data, fechainicio= fechaini)
path_data <- here::here(paste0("data/Datos_anemometros.rdata",Sys.time()))
list.save(clean_data_list, here::here("data/Datos_anemometros.rdata"))

#Borrar variables que no se vayan a usar ----
remove(fechaini,i,nomb)   #Borrar variables
remove(id_iden,actualizar_anemos,actualizar_pluvs,actualizar_term_hig,data_M_A,clean_data,anemos,pluvs,term_hig,get_anem,get_pluvs,get_term_hig) #Borrar funciones 

