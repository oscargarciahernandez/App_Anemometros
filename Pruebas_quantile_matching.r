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
datos_juntos[,grep('(Gust)|(Dir)|(lon)|(lat)|(Date\\d)',colnames(datos_juntos),value=T)]=NULL

#Preparar cross_validation----
nPartes=10  #Numero de partes en las que se va a dividir el histórico
(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])/nPartes #Cuanto tiempo tendra cada parte?
for (i in 1:nPartes) {
  datos_juntos[round((i-1)*nrow(datos_juntos)/nPartes):round(i*nrow(datos_juntos)/nPartes),Parte := i]
}

#1. Calibrar cada punto de era con cross-validation----

(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])*(1-1/nPartes) #Con cuanto tiempo se va a entrenar?

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

#Graficar punto por punto
cols_qm_dirs=grep('qm\\d+_dirs',colnames(datos_juntos),value=T)
cols_qm=grep('qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% cols_qm_dirs)]
for (cols_qm[j] in cols_qm) {
  i=regmatches(cols_qm[j], regexpr("\\d+", cols_qm[j]))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(paste0('uv_wind',i))],datos_juntos[,get(cols_qm[j])],leyenda = c("Mean",paste0("ERA",i),cols_qm[j]),main = paste0('Punto ',i))
}

#Graficar todos los puntos, pero solo el calibrado y el anemo (codigo hecho para 9 puntos)
plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,
                datos_juntos$qm1,datos_juntos$qm2,datos_juntos$qm3,datos_juntos$qm4,datos_juntos$qm5,datos_juntos$qm6,datos_juntos$qm7,datos_juntos$qm8,datos_juntos$qm9,leyenda = c("Mean","qm1","qm2","qm3","qm4","qm5","qm6","qm7","qm8","qm9"),col = 'black')

#Medidas
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' a pelo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(paste0('Punto ',i,' calibrado vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print('----------------------')
}


#2. 1 + separado por uv_dwi (=direcciones de era)----
#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans_dirs=list() #Lista de listas de listas (toma ya!)

#IDEA. Se podria hacer una matriz 2D de listas (por que los ftras son listas). Ayudaría en algo?

#Por ahora separamos en 4 direcciones y a ver que pasa
direcciones_era = 8 %>% seq(0,by=360/.,length.out = .)
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (dir in direcciones_era) {
    lista_ftrans[[i]][[as.character(dir)]]=list()
    for (j in 1:nPartes) {
      ftrans=fitQmapQUANT(datos_juntos[(Parte != j) & (((get(paste0('uv_dwi',i)) %% 360)-as.numeric(dir))<90.0000001),Mean]
                          ,datos_juntos[(Parte != j) & (((get(paste0('uv_dwi',i)) %% 360)-as.numeric(dir))<90.0000001),get(col_era)])
      #lista_ftrans=append(lista_ftrans,values = list(ftrans_i))  esto solo sirve si las columnas uv_wind estan ordenadas
      lista_ftrans_dirs[[i]][[as.character(dir)]]=append(lista_ftrans_dirs[[i]][[as.character(dir)]],values = list(ftrans))
    }
  }
}

#Calibrar.
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (dir in direcciones_era) {
    lista_ftrans[[i]][[as.character(dir)]]=list()
    for (j in 1:nPartes) {
      ftrans=lista_ftrans_dirs[[i]][[as.character(dir)]][[nPartes]]
      #En vez de usar datatable, es necesario dar este rodeo a la hora cde calibrar por que con datatables vacios (de cuando no se cumplen las condicones) se lia
      posiciones_lineas=which((datos_juntos$Parte==j) & (((datos_juntos[,get(paste0('uv_dwi',i))] %% 360)-as.numeric(dir))<90.0000001))
      datos_juntos[posiciones_lineas,(paste0('qm',i,'_dirs')) := doQmapQUANT(datos_juntos[posiciones_lineas,get(paste0('uv_wind',i))],ftrans)]
    }
  }
}

#Graficar punto por punto
for (cols_qm[j] in grep('qm\\d+_dirs',colnames(datos_juntos),value=T)) {
  i=regmatches(cols_qm[j], regexpr("\\d+", cols_qm[j]))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(paste0('uv_wind',i))],datos_juntos[,get(cols_qm[j])],leyenda = c("Mean",paste0("ERA",i),cols_qm[j]),main = paste0('Punto ',i))
}

#Graficar todos los puntos, pero solo el calibrado y el anemo (codigo hecho para 9 puntos)
plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,
                datos_juntos$qm1,datos_juntos$qm2,datos_juntos$qm3,datos_juntos$qm4,datos_juntos$qm5,datos_juntos$qm6,datos_juntos$qm7,datos_juntos$qm8,datos_juntos$qm9,leyenda = c("Mean","qm1","qm2","qm3","qm4","qm5","qm6","qm7","qm8","qm9"),col = 'black')

#Medidas
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' a pelo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(paste0('Punto ',i,' calibrado por direcciones vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_dirs'))]))
  print('----------------------')
}

#Calibrado sin dirs vs por dirs (1 vs 2)----

cols_qm_dirs=grep('qm\\d+_dirs',colnames(datos_juntos),value=T)
cols_qm=grep('qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% cols_qm_dirs)]

#Graficar punto por punto
#Si los puntos que de era que tienen columna 'qm\\d+' no son los mismos que tienen 'qm_\\d+_dirs'...
if(any((cols_qm %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist %>% order)!=(cols_qm_dirs %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist %>% order))) {
  print('ERROR. Parece que no has calibrado los mismos puntos sin dirs que con dirs. Estas seguro de que has hecho las dos calibraciones?')
}else{
  for (j in 1:length(cols_qm)) {
    i=regmatches(cols_qm[j], regexpr("\\d+", cols_qm[j]))  #numero del punto de era
    plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(paste0('uv_wind',i))],datos_juntos[,get(cols_qm[j])],datos_juntos[,get(cols_qm_dirs[j])],leyenda = c("Mean",paste0('uv_wind',i),cols_qm[j],cols_qm_dirs[j]),main = paste0('Punto ',i))
  } 
}

#Medidas
for (i in (cols_qm %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist)) {
  print(paste0('Punto ',i,' calibrado SIN DIRS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print(paste0('Punto ',i,' calibrado CON DIRS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_dirs'))]))
  print('----------------------')
}

#3. Como el 1 pero invirtiendo los periodos de entrenamiento y validación----

(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])/nPartes #Con cuanto tiempo se va a entrenar?

#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans=list() #Lista de listas.
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (j in 1:nPartes) {
    ftrans=fitQmapQUANT(datos_juntos[Parte == j,Mean],datos_juntos[Parte == j,get(col_era)])
    #lista_ftrans=append(lista_ftrans,values = list(ftrans_i))  esto solo sirve si las columnas uv_wind estan ordenadas
    lista_ftrans[[i]]=append(lista_ftrans[[i]],values = list(ftrans))
  }
  
}

#Calibrar.
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  for (j in 1:nPartes) {
    datos_juntos[Parte != j,paste0('inv_qm',i) := doQmapQUANT(datos_juntos[Parte != j,get(col_era)],lista_ftrans[[i]][[j]])]
  }
}

#Graficar punto por punto
cols_inv_qm_dirs=grep('inv_qm\\d+_dirs',colnames(datos_juntos),value=T)
cols_inv_qm=grep('inv_qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% cols_inv_qm_dirs)]
for (cols_inv_qm[j] in cols_inv_qm) {
  i=regmatches(cols_inv_qm[j], regexpr("\\d+", cols_inv_qm[j]))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(paste0('uv_wind',i))],datos_juntos[,get(cols_inv_qm[j])],leyenda = c("Mean",paste0("ERA",i),cols_inv_qm[j]),main = paste0('Punto ',i))
}

#Graficar todos los puntos, pero solo el calibrado y el anemo (codigo hecho para 9 puntos)
plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,
                datos_juntos$inv_qm1,datos_juntos$inv_qm2,datos_juntos$inv_qm3,datos_juntos$inv_qm4,datos_juntos$inv_qm5,datos_juntos$inv_qm6,datos_juntos$inv_qm7,datos_juntos$inv_qm8,datos_juntos$inv_qm9,leyenda = c("Mean","inv_qm1","inv_qm2","inv_qm3","inv_qm4","inv_qm5","inv_qm6","inv_qm7","inv_qm8","inv_qm9"),col = 'black')

#Medidas
for (col_era in grep('uv_wind',colnames(datos_juntos),value=T)) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' a pelo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(paste0('Punto ',i,' calibrado vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('inv_qm',i))]))
  print('----------------------')
}



#Mucho entrenamiento vs poco entrenamiento (1 vs 3)----

cols_inv_qm=grep('inv_qm\\d+',colnames(datos_juntos),value=T)
cols_qm_dirs=grep('qm\\d+_dirs',colnames(datos_juntos),value=T)
cols_qm=grep('qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% c(cols_inv_qm,cols_qm_dirs))]

#Graficar punto por punto
#Si los puntos que de era que tienen columna 'qm\\d+' no son los mismos que tienen 'qm_\\d+_dirs'...
if(any((cols_qm %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist %>% order)!=(cols_inv_qm %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist %>% order))) {
  print('ERROR. Parece que no has calibrado los mismos puntos sin dirs que con dirs. Estas seguro de que has hecho las dos calibraciones?')
}else{
  for (j in 1:length(cols_qm)) {
    i=regmatches(cols_qm[j], regexpr("\\d+", cols_qm[j]))  #numero del punto de era
    plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(paste0('uv_wind',i))],datos_juntos[,get(cols_qm[j])],datos_juntos[,get(cols_inv_qm[j])],leyenda = c("Mean",paste0('uv_wind',i),cols_qm[j],cols_inv_qm[j]),main = paste0('Punto ',i))
  } 
}

#Medidas
for (i in (cols_qm %>% regmatches(.,gregexpr("\\d+", .)) %>% unlist)) {
  print(paste0('Punto ',i,' calibrado mucho tiempo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print(paste0('Punto ',i,' calibrado poco tiempo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('inv_qm',i))]))
  print('----------------------')
}

