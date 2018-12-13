library(here)
library(stringr)
library(lubridate)
library(rlist)
datos<-list.load(here::here("data/Datos_Anemometros/Datos_anemometros.rdata"))
cambio_to_UTC<-function(x){
  a<- hour(with_tz(Sys.time(), tzone = Sys.timezone()))
  b<- hour(with_tz(Sys.time(), tzone = "UTC"))
  corregir_hora<- a-b
  
  x$Date<- x$Date-hm(paste(corregir_hora,":00"))
  return(x)
}

a<-lapply(datos, cambio_to_UTC)
path_data <- here::here(paste0("data/Datos_Anemometros/Datos_anemometros_UTC.rdata"))
list.save(a,path_data)
