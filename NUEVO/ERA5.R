library(here)
source(here::here("NUEVO/Libraries.R"))


HACER_HISTORICO<- TRUE

# CONSTRUIR HISTÓRICO ERA5 ------------------------------------------------
# HAY QUE TENER LOS .NC DE ERA5 SITUADOS EN LA CARPETA /APP_ANEMOMETROS/PYTHON/ERA5
#HAY QUE TENER INSTALADO EL PAQUETE PARALLEL 

#install.packages("parallel")
library(parallel)


if(HACER_HISTORICO){
  ERA5_allfiles<- here::here('python/ERA5/') %>% list.files(full.names = T)
  NC_files<- ERA5_allfiles %>% str_split("/") %>% 
    lapply(function(x){x[length(x)]}) %>% 
    str_remove_all("Data_|.nc") 
  
  RDS_files<- here::here('NUEVO/Data_ERA5/') %>%  list.files(full.names = T)%>% str_split("/") %>% 
    lapply(function(x){x[length(x)]}) %>% 
    str_remove_all("Data_|.RDS") 
  
  
  MAKE_historico_fun<- function(ERA5_file){
    #Importar
    data_ERA<- open.nc(ERA5_file)
    
    #Crear lista
    data_ERA_ls<- read.nc(data_ERA, unpack = TRUE)
    
    #Cerrar ERA 
    close.nc(data_ERA)
    
    #Modificar fecha
    data_ERA_ls<- Formato_fecha_ERA(data_ERA_ls)
    
    #Convertir de lista a data.frame. PROBLEMAS CON LA RAM
    ERA5_df<- ls_to_df_ERA(data_ERA_ls)
    
    #metemos las etiquetas de direccion y redondeamSos los valores
    ERA5_df<-Dirlab_round_ERA(ERA5_df)
    
    file_name<- ERA5_file %>% str_split("/") %>% .[[1]] %>%
      .[length(.)] %>% str_remove(".nc") %>% paste0(.,".RDS")
    
    path_ERA<- here::here("NUEVO/Data_ERA5/")
    if(!dir.exists(path_ERA)){dir.create(path_ERA)}
    
    saveRDS(ERA5_df, file=paste0(path_ERA, file_name))
    rm("data_ERA_ls", "ERA5_df","data_ERA")
    
    
    
    
  }
  mclapply(ERA5_allfiles[!NC_files%in%RDS_files], print,
           mc.cores = getOption("mc.cores", parallel::detectCores()/2))
}


EJECUTAR_PROCESO_OBSOLETO<- FALSE
# Importación de datos ----------------------------------------------------

# [NO EJECUTAR, OBSOLETO] -------------------------------------------------

#OBSOLETO DEBIDO A QUE YA NO USAMOS Rdata's  Y SEGUNDO HAY QUE INCLUIR ERA5 2019
if(EJECUTAR_PROCESO_OBSOLETO){
  if(file.exists(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))){}else{
    
    #Guardar 
    if(!dir.exists(here::here("NUEVO/Data_ERA5"))){dir.create(here::here("NUEVO/Data_ERA5"))}
    
    
    #Importar
    data_ERA_2018<- open.nc(here::here("NUEVO/Data_ERA5/Data_2018.nc"))
    
    #Crear lista
    data_ERA_2018_ls<- read.nc(data_ERA_2018, unpack = TRUE)
    
    #Modificar fecha
    data_ERA_2018_ls<- Formato_fecha_ERA(data_ERA_2018_ls)
    
    #Convertir de lista a data.frame. PROBLEMAS CON LA RAM
    ERA5_df<- ls_to_df_ERA(data_ERA_2018_ls)
    
    
    #Borrar variables excepto ERA5_df
    rm(list = setdiff(ls(),c("ERA5_df","data_ERA_2018_ls",lsf.str())))
    
    #vemos el peso de  toda la información que emplearemos 
    format(object.size(ERA5_df),"Gb")
    
    #metemos las etiquetas de direccion y redondeamos los valores
    ERA5_df<-Dirlab_round_ERA(ERA5_df)
    
    
    save(ERA5_df, file=here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))
    
    
  }
}


# EL PROCESO NUEVO ES EL SIGUIENTE ----------------------------------------
#NÓTESE QUE HAY QUE ESTE PROCESO ESTÁ PENSADO PARA QUE PREVIAMENTE SE HAYA 
# CREADO EL HISTÓRICO ERA5... QUE ESTÁ EN LA SECCIÓN SIGUIENTE
if(!file.exists(here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))){
  Data_2018<- readRDS(here::here('NUEVO/Data_ERA5/Data_2018.RDS'))
  Data_2019<- readRDS(here::here('NUEVO/Data_ERA5/Data_2019.RDS'))
  
  #####ELIMINAMOS VARIABLES QUE NO NOS VALES WIND, DWI Y DIR_DWI
  
  Data_2018$wind<- NULL
  Data_2018$dwi<- NULL
  Data_2018$Dir_dwi<- NULL
  
  ERA5_df<- rbind(Data_2018, Data_2019) %>% .[order(.$Date), ]
  saveRDS(ERA5_df, file = here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))
  
  rm("Data_2018","Data_2019")
}else{ERA5_df<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))}



# CREAMOS VECTOR ERA5 PARA QUANTILE MAPPING -------------------------------
CREAR_VECTOR_ERA5_79_19<- FALSE

if(CREAR_VECTOR_ERA5_79_19){
  
  #COJEMOS LOS RDS DE ERA5
  RDS_ERA5<- here::here('NUEVO/Data_ERA5/') %>% list.files(full.names = TRUE) %>% .[str_detect(.,'[:digit:]{4}')]
  #RDS_ERA5=RDS_ERA5[2]
  Df_ERA<- RDS_ERA5[1] %>% readRDS #Esto no es muy robusto! Para mi RDS_ERA5[1] = "~/App_Anemometros/NUEVO/Data_ERA5/Data_2018.nc
  #He tenido que borrar ese elemento de RDS_ERA5 por que readRDS no lee .nc (obviamente!S)
  
  #HACEMOS UNA TABLA CON LAS DISTANCIAS DE LOS PUNTOS
  Tabla_dist<- Df_ERA %>% group_split(lon,lat)%>% lapply(function(x){y<- cbind(x$lon %>% unique, x$lat %>% unique()) %>% 
    as.data.frame(); colnames(y)<- c("lon","lat"); return(y)}) %>% sapply(., function(x){
      distm(x, c(-2.488504, 43.179389))
    }) 
  
  
  #HACEMOS EL MISMO PROCESO EN BUCLE PARA SACAR TODO EL VECTOR ERA5 DESDE EL 79
  lista_ERA<- list()
  for (i in 1:length(RDS_ERA5)) {
    Df_ERA<- RDS_ERA5[i] %>% readRDS
    Df_ERA_MC<- Df_ERA %>% group_split(lon,lat) %>% .[[which.min(Tabla_dist)]]
    rm(Df_ERA)
    DATOS_PLOT<- Df_ERA_MC[, c("Date", "uv_wind", "uv_dwi")] %>%
      mutate(ERA_binDir= cut(uv_dwi ,
                             breaks =c(0,seq(22.5,337.5,22.5),360, 361), 
                             labels = c(0,seq(22.5,337.5,22.5),0) %>% as.factor()))
    DATOS_PLOT$ERA_binDir<- DATOS_PLOT$ERA_binDir %>% as.character() %>% as.numeric()
    
    lista_ERA[[i]]<- DATOS_PLOT
  }
  saveRDS(lista_ERA, here::here('NUEVO/Data_ERA5/Lista_ERA5_Total.RDS'))
  
}

