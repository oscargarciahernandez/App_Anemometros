library(here)
source(here::here("NUEVO/Libraries.R"))


# Importación de datos ----------------------------------------------------

if(file.exists(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))){}else{

#Importar
data_ERA_2018<- open.nc(here::here("python/Data_ERA5/Data_2018.nc"))

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

   
#Guardar 
if(dir.exists(here::here("NUEVO/Data_ERA5"))){
  save(ERA5_df, file=here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  
  
}else{
  dir.create(here::here("NUEVO/Data_ERA5"))
  save(ERA5_df, file=here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  
  
}


}

# Trabajo con los datos ---------------------------------------------------


#Para Cargar los datos. Se cargaran en el enviroment con el mismo nombre

load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  
