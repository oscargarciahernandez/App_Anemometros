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
setDT(datos_juntos)

#Cogo solo un punto de era por que se ve que son todos muy parecidos
datos_juntos=datos_juntos[,.(Date,Mean,uv_wind1,uv_dwi1)]
#Aunque solo doQmap de problemas con NA (fitQmap los tolera) es mas util quitarlos desde el principio
datos_juntos=na.omit(datos_juntos)


#if is.data.table(datos_juntos):
  #NO FUNCIONA: datos_juntos[a:b, <<nombre_columna>>] = <<algo>>
  #en su lugar: datos_juntos[a:b, <<nombre_columna>> := <<algo>>]
  #NO FUNCIONA: nombre_columna='Mean'  datos_juntos[,nombre_columna := <<algo>>]
  #en su lugar: nombre_columna='Mean'  datos_juntos[,get(nombre_columna) := <<algo>>]

#Quitar columnas innecesarias
datos_juntos[,grep('(Gust)|(dir)|(lon)|(lat)|(Date\\d)',colnames(datos_juntos),value=T)]=NULL

#Detectar que columnas de era tenemos
cols_era=grep('uv_wind\\d+',colnames(datos_juntos),value = T)

#Preparar cross_validation (leave one out, mejor entrenamiento posible)----

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

#Graficar punto por punto
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

#Aqui se ve que no hay mucha esperanza de que añadir puntos implique añadir info relevante...
setDF(datos_juntos)
cor(datos_juntos[cols_era])
cor(datos_juntos[c(cols_era,'Mean')])
setDT(datos_juntos)


#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans_dirs=list() #Lista de listas de listas (toma ya!)

#IDEA. Se podria hacer una matriz 2D de listas (por que los ftras son listas). Ayudaría en algo?

#Por ahora separamos en 4 direcciones y a ver que pasa
direcciones_era = 4 %>% seq(0,by=360/.,length.out = .)
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans_dirs[[i]]=list()
  for (dir in direcciones_era) {
    lista_ftrans_dirs[[i]][[as.character(dir)]]=list()
    for (j in 1:nPartes) {
      print(paste0('Entrenando parte ',j,' de la direccion ',dir,' del punto ',i))
      ftrans=fitQmapQUANT(datos_juntos[(Parte != j) & (((get(paste0('uv_dwi',i)) %% 360)-as.numeric(dir))<90.0000001),Mean]
                          ,datos_juntos[(Parte != j) & (((get(paste0('uv_dwi',i)) %% 360)-as.numeric(dir))<90.0000001),get(col_era)])
      lista_ftrans_dirs[[i]][[as.character(dir)]]=append(lista_ftrans_dirs[[i]][[as.character(dir)]],values = list(ftrans))
    }
  }
}

#Calibrar.
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  for (dir in direcciones_era) {
    for (j in 1:nPartes) {
      ftrans=lista_ftrans_dirs[[i]][[as.character(dir)]][[nPartes]]
      #En vez de usar datatable, es necesario dar este rodeo a la hora cde calibrar por que con datatables vacios (de cuando no se cumplen las condicones) se lia
      posiciones_lineas=which((datos_juntos$Parte==j) & (((datos_juntos[,get(paste0('uv_dwi',i))] %% 360)-as.numeric(dir))<90.0000001))
      datos_juntos[posiciones_lineas,(paste0('qm',i,'_dirs')) := doQmapQUANT(datos_juntos[posiciones_lineas,get(col_era)],ftrans)]
    }
  }
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
  print(paste0('Punto ',i,' calibrado SIN dirS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print(paste0('Punto ',i,' calibrado CON dirS vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_dirs'))]))
  print('----------------------')
}


#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado sin dirs (verde) vs con dirs (azul) '),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i))],add = T,col = 'green')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i,'_dirs'))],add = T,col = 'blue')
}


#3. Calibrar por estaciones----

#Crear funciones de transferencia (ftrans). #La de cada parte se entrena con las mediciones que NO son de la parte (Parte != i)
lista_ftrans_estaciones=list() #Lista de listas de listas (toma ya!)

#Por ahora separamos en 4 estaciones y a ver que pasa
estaciones = c('invierno','primavera','verano','otono')
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  lista_ftrans_estaciones[[i]]=list()
  for (estacion in estaciones) {
    lista_ftrans_estaciones[[i]][[as.character(estacion)]]=list()
    for (j in 1:nPartes) {
      print(paste0('Entrenando parte ',j,' de ',estacion,' del punto ',i))
      switch (estacion,
        invierno = {
          posiciones_lineas=datos_juntos[,.I[month(Date)>=1 & month(Date)<=3 & Parte != j]]
        },
        primavera = {
          posiciones_lineas=datos_juntos[,.I[month(Date)>=4 & month(Date)<=6 & Parte != j]]
        },
        verano = {
          posiciones_lineas=datos_juntos[,.I[month(Date)>=7 & month(Date)<=9 & Parte != j]]
        },
        otono = {
          posiciones_lineas=datos_juntos[,.I[month(Date)>=10 & month(Date)<=12 & Parte != j]]
        }
      )
      
      ftrans=fitQmapQUANT(datos_juntos[posiciones_lineas,Mean]
                          ,datos_juntos[posiciones_lineas,get(col_era)])
      lista_ftrans_estaciones[[i]][[as.character(estacion)]]=append(lista_ftrans_estaciones[[i]][[as.character(estacion)]],values = list(ftrans))
    }
  }
}

#Calibrar.
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  for (estacion in estaciones) {
    for (j in 1:nPartes) {
      print(paste0('Calibrando parte ',j,' de ',estacion,' del punto ',i))
      ftrans=lista_ftrans_estaciones[[i]][[as.character(estacion)]][[nPartes]]
      #En vez de usar datatable, es necesario dar este rodeo a la hora cde calibrar por que con datatables vacios (de cuando no se cumplen las condicones) se lia
      switch (estacion,
              invierno = {
                posiciones_lineas=datos_juntos[,.I[month(Date)>=1 & month(Date)<=3 & Parte == j]]
              },
              primavera = {
                posiciones_lineas=datos_juntos[,.I[month(Date)>=4 & month(Date)<=6 & Parte == j]]
              },
              verano = {
                posiciones_lineas=datos_juntos[,.I[month(Date)>=7 & month(Date)<=9 & Parte == j]]
              },
              otono = {
                posiciones_lineas=datos_juntos[,.I[month(Date)>=10 & month(Date)<=12 & Parte == j]]
              }
      )
      datos_juntos[posiciones_lineas,(paste0('qm',i,'_estaciones')) := doQmapQUANT(datos_juntos[posiciones_lineas,get(col_era)],ftrans)]
    }
  }
}


#Graficar punto por punto
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(col_era)],datos_juntos[,get(paste0('qm',i,'_estaciones'))],leyenda = c("Mean",paste0("ERA",i),paste0('qm',i,'_estaciones')),main = paste0('Punto ',i))
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
  print(paste0('Punto ',i,' calibrado por estaciones vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_estaciones'))]))
  print('----------------------')
}

#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado con estaciones'),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i,'_estaciones'))],add = T,col = 'green')
}

#Calibrado sin estaciones vs por estaciones (1 vs 3)----

#Graficar punto por punto
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  plot_n_graficos(x=datos_juntos$Date,n=10,datos_juntos$Mean,datos_juntos[,get(col_era)],datos_juntos[,get(paste0('qm',i))],datos_juntos[,get(paste0('qm',i,'_estaciones'))],leyenda = c("Mean",paste0('uv_wind',i),paste0('qm',i),paste0('qm',i,'_estaciones')),main = paste0('Punto ',i))
} 


#Medidas
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  print(paste0('Punto ',i,' calibrado SIN estaciones vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i))]))
  print(paste0('Punto ',i,' calibrado CON estaciones vs anemo'))
  print(accuracy(datos_juntos$Mean,datos_juntos[,get(paste0('qm',i,'_estaciones'))]))
  print('----------------------')
}


#Taylor
for (col_era in cols_era) {
  i=regmatches(col_era, regexpr("\\d+", col_era))  #numero del punto de era
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(col_era)],add = F,main = paste0('Punto ',i,' - calibrado sin estaciones (verde) vs con estaciones (azul) '),col = 'red')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i))],add = T,col = 'green')
  taylor.diagram(ref = datos_juntos$Mean,model = datos_juntos[,get(paste0('qm',i,'_estaciones'))],add = T,col = 'blue')
}



#Guardar & cargar----
if (nPartes==nrow(datos_juntos)) {
  saveRDS(datos_juntos,here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))
}else{
  saveRDS(datos_juntos,here::here(paste0('/NUEVO/Data_calibracion/datos_juntos_',nPartes,'.rds')))
}

if (nPartes==nrow(datos_juntos)) {
  datos_juntos=readRDS(here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))
}else{
  datos_juntos=readRDS(here::here(paste0('/NUEVO/Data_calibracion/datos_juntos_',nPartes,'.rds')))
}

