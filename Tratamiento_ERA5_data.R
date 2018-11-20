
# Introduccion y libraries ------------------------------------------------


#install.packages("RNetCDF")
  library(RNetCDF)
  library(stringr)
  library(lubridate)
  
  ### Ya tenemos los datos del ERA5 para trabajar (no todos, pero alguno si)
  ## El problema es que vamos a manejar entorno a 14 gb's de datos
  ## PAra que se hagan una idea los datos de 1 año pesan 700 mb's
  ## no se pueden subir al repo cosas tan pesadas, 
  ## Cuando halla descargado todos los datos lo compartiremos por drive o vete a saber
  ## He subido al drive un fragmento por si alguno se anima a empezar a trabajar
  ##  con datos tipo .nc
 


# Primeros pasos archivos .nc ---------------------------------------------

  
  #Pasos previos lógicos con datos tipo .nc
  
   data_ERA<- open.nc(here::here("python/Data_2000.nc"))
  print.nc(data_ERA)
  
  
  #El unpack es muy importante
  #Se hace para añadir offset y factor de escala
  data_ERA_ls<- read.nc(data_ERA, unpack = TRUE)
  
  
  #Usar este for para extraer todas las variables individualmente
  #innecesario
  #for (i in 1:length(names(data_ERA))) {assign(names(data_ERA)[i],data_ERA[i])}
  
  
  
  
  

  
# Lista con todos los valores ---------------------------------------------

   
  #Creación de una lista con todos los valores
  longitud<- as.vector(data_ERA_ls$longitude)
  latitud<- as.vector(data_ERA_ls$latitude)
  lista_pos <- list()
  k<- 1 
  for (lon in 1:length(longitud)) {
   
    for (lat in 1:length(latitud)) {
      x <- lon
      y <- lat 
      tabla<- data.frame(cbind(longitud[x],latitud[y],data_ERA_ls$time,
                               data_ERA_ls$u10n[x,y,],data_ERA_ls$u10[x,y,],
                               data_ERA_ls$v10n[x,y,],data_ERA_ls$v10[x,y,],
                               data_ERA_ls$dwi[x,y,],data_ERA_ls$fg10[x,y,], 
                               data_ERA_ls$wind[x,y,],data_ERA_ls$t2m[x,y,],
                               data_ERA_ls$i10fg[x,y,]))
                               
                              
                              
     
      nombre<- paste0(round(longitud[x],digits = 1),"_",
                      round(latitud[y],digits = 1))
      lista_pos[[nombre]]<- tabla
      k<-k+1
      }
  }
 
  
# Nombramos columnas ------------------------------------------------------

 
  #Añadimos nombres de columnas 
  
  colum_names<- vector()
  for (i in 1:length(names(data_ERA_ls))) {
    var <- names(data_ERA_ls)[i]
    colum_names[i]<- str_replace_all(att.get.nc(data_ERA,var,"long_name"), fixed(" "), "")
    }
  for (i in 1:length(lista_pos)) {
    colnames(lista_pos[[i]])<- colum_names
  }
  
  
 

# Fecha formato POSIXct ---------------------------------------------------

  
  #Creamos una columna del tiempo tipo Posixct
  time_1<-utcal.nc("hours since 1900-01-01 00:00:0.0",data_ERA_ls$time, type = "n")
  ymd_1<-paste(time_1[,1],time_1[,2],time_1[,3],sep = "-")
  hms_1<- paste(time_1[,4],time_1[,5],time_1[,6],sep = "-")
  time_2<-ymd_hms(paste0(ymd_1," ",hms_1))
  
  # la añadimos a la lista
  for (i in 1:length(lista_pos)) {
    lista_pos[[i]]$time <- time_2
    
  }
  

# Buscar todos los archivos DAta...nc -------------------------------------

  filesnc<- list.files(path="python/",all.files = TRUE)  
  filesnc <- filesnc[str_detect(filesnc,pattern = "Data_")]
  
  anio<- vector()
  for (i in 1:length(filesnc)) {
    data_1<- open.nc(paste0(here::here("python/"),"/",filesnc[i]))
  
  data_ls<- read.nc(data_1, unpack = TRUE)$time[1]
    fecha<- utcal.nc("hours since 1900-01-01 00:00:0.0",data_ls, type = "n")
    anio[i]<- fecha[,1]
  
  }
  tabla<- cbind(filesnc,anio)
  
  
  