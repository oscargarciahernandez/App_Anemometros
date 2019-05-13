library(here)
source(here::here("NUEVO/Libraries.R"))


HACER_HISTORICO<- FALSE
SACAR_MEDIAS_PUNTO_MAS_CERCANO<-FALSE

# Importación de datos ----------------------------------------------------

######PROCESO ANTIGUO... PROBABLEMENTE OBSOLETO EN BREVES
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

######## SI HEMOS GENERADO EL HISTÓRICO--- EL MÉTODO NUEVO PARA GENERAR 
# ERA5_DF ES EL SIGUIENTE 

Data_2018<- readRDS(here::here('NUEVO/Data_ERA5/Data_2018.RDS'))
Data_2019<- readRDS(here::here('NUEVO/Data_ERA5/Data_2019.RDS'))

#####ELIMINAMOS VARIABLES QUE NO NOS VALES WIND, DWI Y DIR_DWI

Data_2018$wind<- NULL
Data_2018$dwi<- NULL
Data_2018$Dir_dwi<- NULL

ERA5_df<- rbind(Data_2018, Data_2019) %>% .[order(.$Date), ]
saveRDS(ERA5_df, file = here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))

rm("Data_2018","Data_2019")

# Trabajo con los datos ---------------------------------------------------


# CONSTRUIR HISTÓRICO ERA5 ------------------------------------------------

if(HACER_HISTORICO){
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
if(SACAR_MEDIAS_PUNTO_MAS_CERCANO){
  
  
  
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
  
}



####JUNTAMOS LOS DATOS DEL ANEMO 

#[OBSOLETO] Para Cargar los datos. Se cargaran en el enviroment con el mismo nombre
#load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  

#CARGAMOS LOS DATOS DE ERA5 2018_2019
ERA5_df<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_df.RDS'))


#CARGAMOS ANEMOS
#load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
#COJEMOS ANEMO UNI
#Anemometro_uni<- Anemometros$`0B38DAE79059`

DATOS_ANEMOMETRO_UNI<- readRDS(here::here('NUEVO/Data_calibracion/0B38DAE79059_juntos.rds')) %>% 
  .[,c(5,2,3,4)]
colnames(DATOS_ANEMOMETRO_UNI)<- c("Date", colnames(readRDS(here::here('NUEVO/Data_calibracion/0B38DAE79059_juntos.rds'))[,2:4]))

#CARGAMOS COORDENADAS DEL ANEMO
Coord_ERA5_anemo<- readRDS(here::here('NUEVO/Data_ERA5/ERA5_coord_0B38DAE79059.RDS'))

#CORTAMOS DATOS ERA PARA EL PUNTO MAS CERCANO. MC
ERA5_df_MC<- ERA5_df %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) 
  
DATOS_JUNTOS<- left_join(DATOS_ANEMOMETRO_UNI, ERA5_df_MC, by="Date")


##### AÑADIMOS EL FACTOR K DEBIDO A LA ALTITUD
#Calculo factor K diferencia de modulo entre punto ERA y anemo
zo=3 #[m] Centers of cities with tall buildings - Manwell pag 46, tabla 2.2
z=155 + 3.5*6 + 1.5 #[m] Altura anemo = altitud segun google earth + altura edificio + altura poste anemo
zr=401 + 10 #[m] Altura era = altitud segun google earth + 10m
k=log(z/zo)/log(zr/zo)  #k=U(z)/U(zr)


###HACIENDO MEDIA DE LOS DATOS DE LOS ANEMOS. 
DATOS_JUNTOS %>% .[complete.cases(.),] %>% summarise(Vmean=mean(Mean), Gust_mean=mean(Gust),
                                                     ERA5_mean=mean(uv_wind), ERA5_mean_K=mean(uv_wind*k))



#########HACIENDO MEDIAS DE LOS HISTÓRICOS DE ERA5

# MEDIAS HISTORICAS ERA5 -------------------------------------------------

RDS_HISTORICO_ERA<-list.files(here::here('NUEVO/Data_ERA5/'), full.names = T) %>% 
  .[1:40]

ERA_mean<- vector()
#ERA_mean_k<- vector()

for (i in 1:length(RDS_HISTORICO_ERA) ) {
    ERA_mean[i]<- readRDS(RDS_HISTORICO_ERA[i]) %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) %>% 
    summarise(ERA_mean= mean(uv_wind, na.rm = T))
    #ERA_mean_k[i]<- readRDS(RDS_HISTORICO_ERA[i]) %>% dplyr::filter(., lon==Coord_ERA5_anemo$lon & lat==Coord_ERA5_anemo$lat) %>%summarise(ERA_mean= mean(uv_wind*k, na.rm = T))
              
}
