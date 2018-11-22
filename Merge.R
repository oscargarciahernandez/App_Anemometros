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



x<- Datos_1$anem1
y<- Datos_2$anem1

z<-which()
z<-merge(x,y, by="s.since_1.1.1970")

length(x[,1])
length(y[,1])
length(z[,1])
