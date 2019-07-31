library(qmap)
library(forecast)
library(magrittr)
library(data.table)
#Preparar los datos con los que trabajar----
#Que anemo usaremos? Elegir
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}
levels(t_reg$ID)  #Que anemos tenemos? Enseña las IDs
#anemo_elegido=levels(t_reg$ID)[1]
anemo_elegido="0B38DAE79059"
datos_juntos <- readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_juntos.rds")))

#Aunque solo doQmap de problemas con NA (fitQmap los tolera) es mas util quitarlos desde el principio
datos_juntos=datos_juntos %>% .[complete.cases(.[,c('Mean',grep('uv_wind',colnames(.),value=T))]),]

#Formato
datos_juntos$Dir=as.numeric(datos_juntos$Dir)
datos_juntos=as.data.table(datos_juntos)
#if is.data.table(datos_juntos):
  #NO FUNCIONA: datos_juntos[a:b, <<nombre_columna>>] = <<algo>>
  #en su lugar: datos_juntos[a:b, <<nombre_columna>> := <<algo>>]
  #NO FUNCIONA: nombre_columna='Mean'  datos_juntos[,nombre_columna := <<algo>>]
  #en su lugar: nombre_columna='Mean'  datos_juntos[,get(nombre_columna) := <<algo>>]

#Quitar columnas innecesarias
datos_juntos[,grep('(Gust)|(Dir)|(lon)|(lat)|(uv_dwi)|(Date\\d)',colnames(datos_juntos),value=T)]=NULL

#Preparar cross_validation----
nPartes=10  #Numero de partes en las que se va a dividir el histórico
(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])/nPartes #Cuanto tiempo tendra cada parte?
for (i in 1:nPartes) {
  datos_juntos[round((i-1)*nrow(datos_juntos)/nPartes):round(i*nrow(datos_juntos)/nPartes),Parte := i]
}

#1. Calibrar cada punto de era con cross-validation----
#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans=list() #Lista de listas.
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (j in 1:nPartes) {
    ftrans=fitQmapQUANT(datos_juntos[Parte != j,Mean],datos_juntos[Parte != j,get(col_era)])
    #lista_ftrans=append(lista_ftrans,values = list(ftrans_i))  esto solo sirve si las columnas uv_wind estan ordenadas
    lista_ftrans[[i]]=append(lista_ftrans[[i]],values = list(ftrans))
  }

}

#Calibrar.
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  for (j in 1:nPartes) {
    datos_juntos[Parte == j,paste0('qm',i) := doQmapQUANT(datos_juntos[Parte == j,get(col_era)],lista_ftrans[[i]][[j]])]
  }
}

#Graficar
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos$uv_wind1,datos_juntos$qm1,leyenda = c("Mean","ERA","Cal"),main = "Punto 1")
}

#Medidas
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Medidas punto ',i))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print('----------------------')
}

