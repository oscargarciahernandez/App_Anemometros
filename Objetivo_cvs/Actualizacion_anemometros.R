source(here::here("Objetivo_cvs/Funciones_Selenium.R"))
##Obtenemos todos los id's de todos los sensores
sensor_ids<- Get_sensor_ID(phoneid)

##Obtenemos Id's de los anemometros
anemo_ID<- sensor_ids[str_detect(str_sub(sensor_ids,1,2), "0B")]


##Actualizamos  Datos empleando get_sensor_Data
Datos_anemometros<-list.load(here::here("data/Datos_Anemometros/Datos_anemometros.rdata"))

new_list<- list()
for (i in 1:length(anemo_ID)) {
  new_list[[i]]<- Get_sensor_Data(anemo_ID[i])
  
}

names(new_list)<- anemo_ID

if(object.size(new_list)>object.size(Datos_anemometros)){
  path_data <- here::here(paste0("data/Datos_Anemometros/Datos_anemometros.rdata"))
  list.save(new_list,path_data)
}

#después de haber actualizado los datos
#ejecutamos el script para el cambio de hora a UTC
source(here::here("Cambio a UTC.R"))
