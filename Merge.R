library(stringr)
library(rlist)
library(here)
library(purrr)

listas_datos<- list.files(here::here("data"))
for (i in 1:length(listas_datos)) {
 assign(paste0("Datos_",i),list.load(paste0(here::here("data/"),"/",listas_datos[i])))
  
}


source(here("Script_cap5.R"))

Datos_1<-Formatos_columna(Datos_1)
Datos_2<-Formatos_columna(Datos_2)
Datos_3<-Formatos_columna(Datos_3)


last_1<- sapply(Datos_1, function(x) if(is.data.frame(x)){ x[1,2]}else{NA})
last_2<- sapply(Datos_2, function(x) if(is.data.frame(x)){ x[1,2]}else{NA})
last_3<- sapply(Datos_3, function(x) if(is.data.frame(x)){ x[1,2]}else{NA})

rbind(last_1,last_2,last_3)


#El problema que encontré es que está la hora cambiada entre data-sets
#el cambio horario parece que afecta a todo el registro 
#valla putada, no entiendo porqué la página web ha hecho esto

Date_new<- lapply(Datos_1,  function(x) if(is.data.frame(x)){x[,2]<-x[,2]-hm("01:00")}else{NA})
for (i in 1:length(Date_new)) {
  if(is.data.frame(Datos_1[[i]])){
    Datos_1[[i]][,2]<- Date_new[[i]]}else{NA}}

lista_datos<- list(Datos_1,Datos_2,Datos_3)
range_datae<-lapply(lista_datos, function(x) sapply(x,function(y) if(is.data.frame(y)){range(y[,2])}else{NA}))


lista_total<- list()
for (i in 1:length(lista_datos[[1]])) {
  if(is.data.frame(lista_datos[[1]][[i]])){
    x<-lista_datos[[1]][[i]]
    y<-lista_datos[[2]][[i]]
    z<-rbind(x[!x[,2]%in%y[,2],],y)
    
    h<-lista_datos[[3]][[i]]
    b<-rbind(z[!z[,2]%in%h[,2],],h)
    final__dataframe<- b[order(b[,2]),]
    lista_total[[i]]<- final__dataframe
  }else{NA}

}


source(here::here('funciones_app_anemometros.R'))
disp<-id_iden(640689911849)


names(lista_total)<- as.character(disp[,2])

path_data <- here::here(paste0("data/Merged_dataset",Sys.Date(),".rdata"))
list.save(lista_total,path_data)




# Maquillando la lista de los datos de los anemometros --------------------


Datos<- list.load(here::here("data/Merged_dataset2018-11-23.rdata"))
Datos_anemos<- Datos[str_detect(names(Datos), "0B")]
Datos_anemos<- lapply(Datos_anemos, function(x) {
  x<- x[,2:6] 
colnames(x)<- c("Date","Mean","Max","Dir_ch","Dir_deg") 
return(x)} )

path_data <- here::here(paste0("data/Datos_Anemometros/Datos_anemometros.rdata"))
list.save(Datos_anemos,path_data)
