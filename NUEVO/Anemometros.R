library(here)
source(here::here("NUEVO/Libraries.R"))



# Importacio de datos -----------------------------------------------------

if(file.exists(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))){}else{ 
  
  #Es necesario crear un loop que descarge los datos
  # de todos los anemometros
  #ahora mismo no lo hace
 
  #listamos los archivos CSV excepto la tabla de registro 
file_names<- list.files(here::here("NUEVO/Data_anemometros/"))
CSV_files<-file_names[str_detect(file_names, ".csv")]
CSV_files<- CSV_files[!str_detect(CSV_files, "TABLA")]


Anem_ID<- str_sub(CSV_files,1,12)
Anemometros<- list()

for (i in 1:length(CSV_files)) {
  path_to_data<- here::here(paste0("NUEVO/Data_anemometros/",CSV_files[i]))
  
  #Leer el CSV con las columnas que nos interesan
  CSV<- read.csv(path_to_data,sep = ";")[,c(1,2,6,10)]
  colnames(CSV)<- c("Date", "Mean","Gust", "Dir")
  
  #Cambiamos a formato POSIXCT y a horario UTC
  CSV<- cambio_to_UTC(CSV)
  
  
  #Ajustamos las etiquetas de dirección del viento
  CSV=poner_dir_en_numerico(CSV)
  
  
  #Guardamos el CSV en una lista con el ID del anemómetro como nombre
  
  Anemometros[[i]]<- CSV
  
}

names(Anemometros)<- Anem_ID

#Si lo de rellenar los huecos de los dataframes acaba siedo algo permanente lo implantamos aqui.

#Guardar


if(dir.exists(here::here("NUEVO/Data_anemometros"))){
  save(Anemometros,
       file=here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))  
  
}else{
  dir.create(here::here("NUEVO/Data_anemometros"))
  save(Anemometros,
       file=here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))  
  
}


#Borramos CSV Y listo
rm(CSV)

}


# Trabajo con los datos ---------------------------------------------------


load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))




# Tabla de registro -------------------------------------------------

#Esta tabla contiene información relevante de los anemometros
# ubicación, descripción que nos pueda servir para orientarnos
# ID y Fecha de inicio de adquisición de datos. 

Tabla_registro<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"),sep = ";")
