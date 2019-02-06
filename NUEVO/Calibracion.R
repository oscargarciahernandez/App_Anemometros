library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  


#Obtenemos puntos del ERA cercanos al Anemo
pos_anem_uni<-c(43.179361, -2.488510)#lat,lon


nearest_lat<- data_ERA_2018_ls$latitude[order(abs(data_ERA_2018_ls$latitude - pos_anem_uni[1]))[1:2]]
nearest_lon<- data_ERA_2018_ls$longitude[order(abs(data_ERA_2018_ls$longitude - pos_anem_uni[2]))[1:2]]



