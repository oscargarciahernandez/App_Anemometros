library(stringr)
library(rlist)
library(here)
library(purrr)
listas_datos<- list.files(here::here("data"))
for (i in 1:length(listas_datos)) {
 assign(paste0("Datos_",i),list.load(paste0(here::here("data/"),"/",listas_datos[i])))
  
}


funcion_anemo <- function(Tabla_sf){
  library(lubridate)
  
  Tabla <- Tabla_sf 
  #Cambiamos el formato de las columnas numericas
  Tabla$Mean <- as.numeric(as.character(Tabla$Mean))
  Tabla$Max <- as.numeric(as.character(Tabla$Max))
  Tabla$Dir_deg <- as.numeric(as.character(Tabla$Dir_deg))
  
  #cambiamos el formato de la fecha
  Tabla$date_string_hour <- dmy_hms(as.character(Tabla$date_string_hour))
  
  return(as.data.frame(Tabla))
  
}
funcion_pluv <- function(Tabla_sf){
  library(lubridate)
  Tabla <- Tabla_sf 
  
  
  #Cambiamos el formato de las columnas numericas
  Tabla$`rain (mm)` <- as.numeric(as.character(Tabla$`rain (mm)`))
  
  
  #cambiamos el formato de la fecha
  Tabla$date_string_hour <- dmy_hms(as.character(Tabla$date_string_hour))
  
  return(as.data.frame(Tabla))
  
}
funcion_term_hig <- function(Tabla_sf){
  library(lubridate)
  Tabla <- Tabla_sf 
  
  
  #Cambiamos el formato de las columnas numericas
  Tabla[,3]<- as.numeric(as.character(Tabla[,3]))
  Tabla[,4] <- as.numeric(as.character(Tabla[,4]))
  
  
  #cambiamos el formato de la fecha
  Tabla$date_string_hour <- dmy_hms(as.character(Tabla$date_string_hour))
  
  return(as.data.frame(Tabla))
  
}
funcion_anemo <- possibly(funcion_anemo,otherwise = NA)

funcion_pluv <- possibly(funcion_pluv,otherwise = NA)

funcion_term_hig <- possibly(funcion_term_hig,otherwise = NA)

Formatos_columna <- function(lista_datos){
  numero_sensores <- length(lista_datos)
  
  lista_actualizada <- list()
  for (i in 1:numero_sensores) {
    
    #obtenemos la primera letra
    letra_nombre <- str_sub(names(lista_datos[i]),1,1)
    
    #usamos el ya visto ifelse 
    ifelse(
      letra_nombre=="a",tabla_actualizada<- funcion_anemo(lista_datos[[i]][,]),
      ifelse(
        letra_nombre=="p",tabla_actualizada<-funcion_pluv(lista_datos[[i]]),
        ifelse(
          letra_nombre=="t",tabla_actualizada<-funcion_term_hig(lista_datos[[i]]),-31)))
    
    
    #Guardamos en nueva lista
    lista_actualizada[[names(lista_datos[i])]] <- tabla_actualizada
  }
  
  return(lista_actualizada)
  
}

Tabla_con_formato <- Formatos_columna(Datos)
