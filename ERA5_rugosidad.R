library(here)
source('NUEVO/Libraries.R')

RUGOSIDAD<- here::here('python/ERA5/ERA5_rugosidad/Data_Z0_2019.nc') %>% open.nc() %>% read.nc(unpack = TRUE)


RUGOSIDAD_ls<- Formato_fecha_ERA(RUGOSIDAD)


tabla2<- data.frame()

for (lon in 1:length(RUGOSIDAD_ls$longitude)) {
  tabla1<- data.frame()
  for (lat in 1:length(RUGOSIDAD_ls$latitude)) {
    x<- cbind(RUGOSIDAD_ls$longitude[lon],
              RUGOSIDAD_ls$latitude[lat],
              RUGOSIDAD_ls[["fsr"]][lon,lat, ])
    
    tabla1<- rbind(tabla1,x)   
  }
  tabla2<- rbind(tabla2,tabla1)
}
tabla_3<- cbind(RUGOSIDAD_ls$time,tabla2)
colnames(tabla_3)<- c("Date","lon","lat","sfr")

TABLA_UNICA<- tabla_3 %>% group_by(Date) %>% group_split() %>% .[[1]]

saveRDS(TABLA_UNICA, here::here('NUEVO/Data_ERA5/DATOS_RUGOSIDAD.RDS'))

