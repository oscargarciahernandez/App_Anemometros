
# Introduccion y libraries ------------------------------------------------


#install.packages("RNetCDF")
  library(RNetCDF)
  library(stringr)
  library(lubridate)
  library(here)
  
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
  
  
  
  
  

  
# Lista con todos los valores, (NO ME GUSTA) ---------------------------------------------

   
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
  
 #sustituimos la fecha con formato weno weno 
  data_ERA_ls$time<- time_2

# Buscar todos los archivos DAta...nc  -------------------------------------
  #esto pa' una comprobación, ignoralo¡ 
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
  
  
  
  


# Comprobación u y v component --------------------------------------------
  
  
  u10<- data_ERA_ls$u10n[1,1,]
  v10<- data_ERA_ls$v10n[1,1,]
  wind<-data_ERA_ls$wind[1,1,]
  dir<- data_ERA_ls$dwi[1,1,]
  
  #convertir componentes a dirección y modulo
  wind_abs = sqrt(u10^2 + v10^2)
  wind_dir_rad = atan2(u10/wind_abs, v10/wind_abs) 
  wind_dir_deg1 = wind_dir_rad * 180/pi 
  wind_dir_deg2 = wind_dir_deg1+ 180 
  
  #convertir direccion y modulo a componentes
  u10_1<-sin((dir+180)*pi/180)*wind
  v10_1<- cos((dir+180)*pi/180)*wind
  
  
  
  #mirando en valor abosuluto los modulos de los vectores tenemos que
  # los modulos son mayores para las componentes extraídas a partir de 
  #wind y dwi
  mean(abs(u10))
  mean(abs(u10_1))
  mean(abs(v10))
  mean(abs(v10_1))
  
  #errores
  error_u<-vector()
  error_u_per_1<-vector()
  error_u_per_2<-vector()
  
  for (i in 1:length(u10)) {
    if(sign(u10[i])==sign(u10_1[i])){
      error_u[i]<-abs(u10[i]-u10_1[i])
    }else{
      error_u[i]<- abs(u10[i])+abs(u10_1[i])
    }
    error_u_per_1[i]<- error_u[i]/abs(u10[i])
    error_u_per_2[i]<- error_u[i]/abs(u10_1[i])
    
  }
  
  summary(error_u)
  summary(error_u_per_1)
  summary(error_u_per_2)
  
  
  
  
  error_v<-vector()
  error_v_per_1<- vector()
  error_v_per_2<- vector()
  for (i in 1:length(v10)) {
    if(sign(v10[i])==sign(v10_1[i])){
      error_v[i]<-abs(v10[i]-v10_1[i])
    }else{
      error_v[i]<- abs(v10[i])+abs(v10_1[i])
    }
    
    error_v_per_1[i]<- error_v[i]/abs(v10[i])
    error_v_per_2[i]<- error_v[i]/abs(v10_1[i])
    
  }
  
  summary(error_v)
  summary(error_v_per_1)
  summary(error_v_per_2)
  
  # Comparando con el modulo
  
  error_mod<- vector()
  for (i in 1:length(v10)) {
    error_mod[i] <- abs(wind[i]-sqrt(u10[i]^2+v10[i]^2))
  }
  
  summary(error_mod)
 