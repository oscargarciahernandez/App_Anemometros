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
  
  
  
  for(i in 39:length(ERA5_allfiles)){
    #Importar
    data_ERA<- open.nc(ERA5_allfiles[i])
    
    #Crear lista
    data_ERA_ls<- read.nc(data_ERA, unpack = TRUE)
    
    #Cerrar ERA 
    close.nc(data_ERA)
    
    #Modificar fecha
    data_ERA_ls<- Formato_fecha_ERA(data_ERA_ls)
    
    #Convertir de lista a data.frame. PROBLEMAS CON LA RAM
    ERA5_df<- ls_to_df_ERA_2019(data_ERA_ls)
    
    #metemos las etiquetas de direccion y redondeamos los valores
    ERA5_df<-Dirlab_round_ERA_2019(ERA5_df)
    
    file_name<- ERA5_allfiles[i] %>% str_split("/") %>% .[[1]] %>%
      .[length(.)] %>% str_remove(".nc") %>% paste0(.,".RDS")
    
    path_ERA<- here::here("NUEVO/Data_ERA5/")
    if(!dir.exists(path_ERA)){dir.create(path_ERA)}
    
    saveRDS(ERA5_df, file=paste0(path_ERA, file_name))
    rm("data_ERA_ls", "ERA5_df","data_ERA")
    
    
  }  
  
}




# COORDENADA MÁS CERCANA --------------------------------------------------


#Importar TABLA DE REGISTRO
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}

head(t_reg)
ANEMO_SELECCIONADO<- 1
name_ERA5_coord_anem<- paste0("ERA5_coord_",
                             str_replace_all(as.character(t_reg$ID[ANEMO_SELECCIONADO]), " ",""),
                             ".RDS")

#Sacar todas las coordenadas ERA5 y guardarlas en un Rdata
if (file.exists(paste0(here::here("NUEVO/Data_ERA5/"), name_ERA5_coord_anem))) { 
  Coord_ERA5_anemo<- readRDS(paste0(here::here("NUEVO/Data_ERA5/"), name_ERA5_coord_anem))}else{
    
    #Sacar todas las coordenadas ERA5 y guardarlas en un Rdata
    if (file.exists(here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))) { 
      load(here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))}else{
        Coordenadas_era<- unique(Data_ERA[,c("lon","lat")])
        save(Coordenadas_era, file=here::here("NUEVO/Data_ERA5/ERA5_coord.Rdata"))
      }
    
    
    #Sacar coordenadas anemos de la tabla de registro
    Coordenadas_anemos<- as.data.frame(cbind(as.numeric(sub(",",".",as.character(t_reg$lon))),
                                             as.numeric(sub(",",".",as.character(t_reg$lat)))))
    colnames(Coordenadas_anemos)=c("lon","lat")
    
    #Seleccionar anemo
    Coord_anemo<- Coordenadas_anemos[,]
    
    
    #Ordenarlos puntos del ERA de cercanos a lejanos
    Coord_era<- Coordenadas_era[order((Coordenadas_era$lon-Coord_anemo$lon)^2+(Coordenadas_era$lat-Coord_anemo$lat)^2),]
    
    
    #Coger los n mas cercanos
    n=1
    Coord_era=Coord_era[1:n,]
    Coord_ERA5_anemo<- Coord_era
    
    saveRDS(Coord_era, file=paste0(here::here("NUEVO/Data_ERA5/"),
                                      name_ERA5_coord_anem))
  }


# LEER RDS ERA5 HISTORICO -------------------------------------------------

RDS_HISTORICO_ERA<-list.files(here::here('NUEVO/Data_ERA5/'), full.names = T)

for (i in 1:length(RDS_HISTORICO_ERA) ) {
  Data_ERA<- readRDS(RDS_HISTORICO_ERA[i])
  Vmean<- Data_ERA %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) %>% 
    summarise(Vmean= mean(uv_wind, na.rm = T))

  
    
}

######SELECIONAMOS SOLAMENTA LOS DDATOS DE ERA5 MÁS CERCANOS AL ANEMOMETRO


