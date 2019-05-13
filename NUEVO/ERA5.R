library(here)
source(here::here("NUEVO/Libraries.R"))


MAKE_HISTORICO<- FALSE

# Importación de datos ----------------------------------------------------

if(file.exists(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))){}else{

#Guardar 
if(dir.exists(here::here("NUEVO/Data_ERA5"))){
  
  
}else{
  dir.create(here::here("NUEVO/Data_ERA5"))
}

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

# Trabajo con los datos ---------------------------------------------------


#Para Cargar los datos. Se cargaran en el enviroment con el mismo nombre

load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  





# CONSTRUIR HISTÓRICO ERA5 ------------------------------------------------

if(MAKE_HISTORICO){
  ERA5_allfiles<- here::here('python/ERA5/') %>% list.files(full.names = T)
  
  
  
  for(i in 1:length(ERA5_allfiles)){
    #Importar
    data_ERA<- open.nc(ERA5_allfiles[i])
    
    #Crear lista
    data_ERA_ls<- read.nc(data_ERA, unpack = TRUE)
    
    #Cerrar ERA 
    close.nc(data_ERA)
    
    #Modificar fecha
    data_ERA_ls<- Formato_fecha_ERA(data_ERA_ls)
    
    #Convertir de lista a data.frame. PROBLEMAS CON LA RAM
    ERA5_df<- ls_to_df_ERA(data_ERA_ls)
    
    #metemos las etiquetas de direccion y redondeamos los valores
    ERA5_df<-Dirlab_round_ERA(ERA5_df)
    
    file_name<- ERA5_allfiles[i] %>% str_split("/") %>% .[[1]] %>%
      .[length(.)] %>% str_remove(".nc") %>% paste0(.,".RDS")
    
    path_ERA<- here::here("NUEVO/Data_ERA5/")
    if(!dir.exists(path_ERA)){dir.create(path_ERA)}
    
    saveRDS(ERA5_df, file=paste0(path_ERA, file_name))
    rm("data_ERA_ls", "ERA5_df","data_ERA")
    
    
  }  
  
}



