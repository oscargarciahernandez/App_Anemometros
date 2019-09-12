library(qmap)
library(forecast)
library(magrittr)
library(data.table)
library(plotrix)
library(here)
source(here::here('/NUEVO/Libraries.R'), encoding = 'UTF-8', echo=F)


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

#Detectar que columnas de era tenemos
cols_era=grep('uv_wind\\d+',colnames(datos_juntos),value = T)

#Preparar cross_validation (leave one out, mejor entrenamiento posible)----

#En vez de ejecutar todo podemos cargar los resultados
datos_juntos=readRDS(here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))

nPartes=nrow(datos_juntos)  #"Leave-one-out", se entrena el modelo para cada dato calibrado

(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])/nPartes #Cuanto tiempo tendra cada parte?
for (i in 1:nPartes) {
  datos_juntos[round((i-1)*nrow(datos_juntos)/nPartes):round(i*nrow(datos_juntos)/nPartes),Parte := i]
}

#1. Calibrar cada punto de era con cross-validation----

(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])*(1-1/nPartes) #Con cuanto tiempo se va a entrenar?

#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans=list() #Lista de listas.
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (j in 1:nPartes) {
    ftrans=fitQmapQUANT(datos_juntos[Parte != j,Mean],datos_juntos[Parte != j,get(col_era)])
    #lista_ftrans=append(lista_ftrans,values = list(ftrans_i))  esto solo sirve si las columnas uv_wind estan ordenadas
    lista_ftrans[[i]]=append(lista_ftrans[[i]],values = list(ftrans))
  }

}

#Calibrar.
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  for (j in 1:nPartes) {
    datos_juntos[Parte == j,paste0('qm',i) := doQmapQUANT(datos_juntos[Parte == j,get(col_era)],lista_ftrans[[i]][[j]])]
  }
}

#Vamos a guardar esto por que con lo que tarda en hacerse...
if (nPartes==nrow(datos_juntos)) {
  saveRDS(datos_juntos,here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))
}

#Graficar punto por punto
cols_qm_dirs=grep('qm\\d+_dirs',colnames(datos_juntos),value=T)
cols_qm=grep('qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% cols_qm_dirs)]
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(col_era)],datos_juntos[,get(paste0('qm',i))],leyenda = c("Mean",paste0("ERA",i),paste0('qm',i)),main = paste0('Punto ',i))
}

#Graficar todos los puntos, pero solo el calibrado y el anemo (codigo hecho para 9 puntos)
#IDEA. Estaria bien mejorar plot_n_graficos para que pueda aceptar dataframes y/o listas en vez de un vector por argumento de ... Asi se podria hacer funcionar para un numero indeterminado de vectores a graficar
plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,
                datos_juntos$qm1,datos_juntos$qm2,datos_juntos$qm3,datos_juntos$qm4,datos_juntos$qm5,datos_juntos$qm6,datos_juntos$qm7,datos_juntos$qm8,datos_juntos$qm9,leyenda = c("Mean","qm1","qm2","qm3","qm4","qm5","qm6","qm7","qm8","qm9"),col = 'black')

#Medidas
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' a pelo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(paste0('Punto ',i,' calibrado vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print('----------------------')
}

#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado sin dirs'),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i))],add = T,col = 'green')
}


#2. 1 + separado por uv_dwi (=direcciones de era)----
#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans_dirs=list() #Lista de listas de listas (toma ya!)

#IDEA. Se podria hacer una matriz 2D de listas (por que los ftras son listas). Ayudaría en algo?

#Por ahora separamos en 4 direcciones y a ver que pasa
direcciones_era = 32 %>% seq(0,by=360/.,length.out = .)
for (col_era in cols_era) {
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
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans[[i]]=list()
  for (dir in direcciones_era) {
    lista_ftrans[[i]][[as.character(dir)]]=list()
    for (j in 1:nPartes) {
      ftrans=lista_ftrans_dirs[[i]][[as.character(dir)]][[nPartes]]
      #En vez de usar datatable, es necesario dar este rodeo a la hora cde calibrar por que con datatables vacios (de cuando no se cumplen las condicones) se lia
      posiciones_lineas=which((datos_juntos$Parte==j) & (((datos_juntos[,get(paste0('uv_dwi',i))] %% 360)-as.numeric(dir))<90.0000001))
      datos_juntos[posiciones_lineas,(paste0('qm',i,'_dirs')) := doQmapQUANT(datos_juntos[posiciones_lineas,get(col_era)],ftrans)]
    }
  }
}

#Vamos a guardar esto por que con lo que tarda en hacerse...
if (nPartes==nrow(datos_juntos)) {
  saveRDS(datos_juntos,here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))
}

#Graficar punto por punto
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(col_era)],datos_juntos[,get(paste0('qm',i,'_dirs'))],leyenda = c("Mean",paste0("ERA",i),paste0('qm',i,'_dirs')),main = paste0('Punto ',i))
}

#Graficar todos los puntos, pero solo el calibrado y el anemo (codigo hecho para 9 puntos)
#IDEA. Estaria bien mejorar plot_n_graficos para que pueda aceptar dataframes y/o listas en vez de un vector por argumento de ... Asi se podria hacer funcionar para un numero indeterminado de vectores a graficar
plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,
                datos_juntos$qm1,datos_juntos$qm2,datos_juntos$qm3,datos_juntos$qm4,datos_juntos$qm5,datos_juntos$qm6,datos_juntos$qm7,datos_juntos$qm8,datos_juntos$qm9,leyenda = c("Mean","qm1","qm2","qm3","qm4","qm5","qm6","qm7","qm8","qm9"),col = 'black')

#Medidas
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' a pelo vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(col_era)]))
  print(paste0('Punto ',i,' calibrado por direcciones vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_dirs'))]))
  print('----------------------')
}

#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado con dirs'),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i,'_dirs'))],add = T,col = 'green')
}

#Calibrado sin dirs vs por dirs (1 vs 2)----

#cols_qm_dirs=grep('qm\\d+_dirs',colnames(datos_juntos),value=T)
#cols_qm=grep('qm\\d',colnames(datos_juntos),value=T) %>% .[!(. %in% cols_qm_dirs)]

#Graficar punto por punto
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(col_era)],datos_juntos[,get(paste0('qm',i))],datos_juntos[,get(paste0('qm',i,'_dirs'))],leyenda = c("Mean",paste0('uv_wind',i),paste0('qm',i),paste0('qm',i,'_dirs')),main = paste0('Punto ',i))
} 


#Medidas
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' calibrado SIN DIRS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print(paste0('Punto ',i,' calibrado CON DIRS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_dirs'))]))
  print('----------------------')
}


#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado sin dirs (berde) vs con dirs (azul) '),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i))],add = T,col = 'green')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i,'_dirs'))],add = T,col = 'blue')
}


#Preparar cross_validation (solo dos partes, peor entrenamiento posible sin liarla mucho con la cross validation)----

nPartes=2  #"Leave-one-out", se entrena el modelo para cada dato calibrado

(range(datos_juntos$Date)[2]-range(datos_juntos$Date)[1])/nPartes #Cuanto tiempo tendra cada parte?
for (i in 1:nPartes) {
  datos_juntos[round((i-1)*nrow(datos_juntos)/nPartes):round(i*nrow(datos_juntos)/nPartes),Parte := i]
}

#Pruebas potencia----
library(MASS)
fit = fitdistr(datos_juntos[Mean > 0,Mean],"weibull") #No acepta ceros
k<-coef(fit)['shape']
c<-coef(fit)['scale']

summary(datos_juntos$Mean)
histo=datos_juntos$Mean %>% hist(.,seq(0, max(.), by=0.1))
