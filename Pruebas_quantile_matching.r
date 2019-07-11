library(qmap)
library(forecast)

#Preparar los datos con los que trabajar (solo punto mas cercano de era)----
#Que anemo usaremos? Elegir
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}
levels(t_reg$ID)  #Que anemos tenemos? Enseña las IDs
#anemo_elegido=levels(t_reg$ID)[1]
anemo_elegido="0B38DAE79059"
datos_juntos <- readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_juntos.rds")))
datos_juntos=datos_juntos[,1:9] #Por ahora cogemos solo velocidad del punto mas cercano de era

#Aunque solo doQmap de problemas con NA (fitQmap los tolera) es mas util quitarlos desde el principio
datos_juntos=datos_juntos[complete.cases(datos_juntos$Mean)&complete.cases(datos_juntos$uv_wind1),]

datos_juntos$Dir=as.numeric(datos_juntos$Dir)

#PRIMER EJERCICIO. Calibrar todo con todo----
ftrans=fitQmapQUANT(datos_juntos$Mean,datos_juntos$uv_wind1)
calibrado=doQmapQUANT(datos_juntos$uv_wind1,ftrans)

#Hora de analizar los resultados
summary(datos_juntos$Mean)
summary(calibrado)
#Vemos que los quantiles, el max y el min encajan, pero no la media
cor(calibrado,datos_juntos$Mean)
cor(datos_juntos$uv_wind1,datos_juntos$Mean)
cor(calibrado,datos_juntos$uv_wind1)

#Mean absolute error=mean(abs(error)) [m/s]
mean(abs(datos_juntos$Mean-datos_juntos$uv_wind1))
mean(abs(datos_juntos$Mean-calibrado))

#Root Mean Squared Error=sqrt(mean(error^2)) [m/s]
sqrt(mean((datos_juntos$Mean-datos_juntos$uv_wind1)^2))
sqrt(mean((datos_juntos$Mean-calibrado)^2))


plot_n_graficos(x=datos_juntos$Date1,n=14,datos_juntos$Mean,calibrado,datos_juntos$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 1")

#Vemos que el calibrado se tiene valores parecidos a los del anemo, pero progresa como ERA.

rm(ftrans,calibrado)

#SEGUNDO EJERCICIO. Dividir datos por la mitad e intentar calibrar una mitad con la otra----

#Solo columnas 5 (Date1=fecha de era), 2 (Mean=medicion) y 8 (uv_wind1=modelado/era)
mitad_1=datos_juntos[1:(nrow(datos_juntos)/2),c(5,2,8)]
mitad_2=datos_juntos[-(1:(nrow(datos_juntos)/2)),c(5,2,8)]

#Crear funciones de transferencia (ftrans). Permite NAs.
ftrans_mitad_1<-fitQmapQUANT(mitad_1$Mean,mitad_1$uv_wind1)
ftrans_mitad_2<-fitQmapQUANT(mitad_2$Mean,mitad_2$uv_wind1)

#Deducir viento. Cada mitad se calibra con la ftrans sacada de la otra mitad.
calibrado_mitad_1 <- doQmapQUANT(mitad_1$uv_wind1,ftrans_mitad_2)
calibrado_mitad_2 <- doQmapQUANT(mitad_2$uv_wind1,ftrans_mitad_1)

#Hora de analizar los resultados mitad_1
cor(calibrado_mitad_1,mitad_1$Mean)
cor(mitad_1$Mean,mitad_1$uv_wind1)
cor(calibrado_mitad_1,mitad_1$uv_wind1)

#Mean absolute error=mean(abs(error)) [m/s]
mean(abs(mitad_1$Mean-mitad_1$uv_wind1))
mean(abs(mitad_1$Mean-calibrado_mitad_1))

#Root Mean Squared Error=sqrt(mean(error^2)) [m/s]
sqrt(mean((mitad_1$Mean-mitad_1$uv_wind1)^2))
sqrt(mean((mitad_1$Mean-calibrado_mitad_1)^2))


plot_n_graficos(x=mitad_1$Date1,n=7,mitad_1$Mean,calibrado_mitad_1,mitad_1$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 2 mitad_1")

#Hora de analizar los resultados mitad_2
cor(calibrado_mitad_2,mitad_2$Mean)
cor(mitad_2$Mean,mitad_2$uv_wind1)
cor(calibrado_mitad_2,mitad_2$uv_wind1)

#Mean absolute error=mean(abs(error)) [m/s]
mean(abs(mitad_2$Mean-mitad_2$uv_wind1))
mean(abs(mitad_2$Mean-calibrado_mitad_2))

#Root Mean Squared Error=sqrt(mean(error^2)) [m/s]
sqrt(mean((mitad_2$Mean-mitad_1$uv_wind1)^2))
sqrt(mean((mitad_2$Mean-calibrado_mitad_2)^2))


plot_n_graficos(x=mitad_2$Date1,n=7,mitad_2$Mean,calibrado_mitad_2,mitad_2$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 2 mitad_2")

remove(mitad_1,mitad_2,calibrado_mitad_1,calibrado_mitad_2,ftrans_mitad_1,ftrans_mitad_2)

#TERCER EJERCICIO. Dividir datos por la mitad y hacer una calibracion para cada direccion de era.----

#Columnas 5 (Date1=fecha de era), 2 (Mean=medicion), 8 (uv_wind1=modelado/era) y 9 (uv_dwi1=direccion era)
mitad_1=datos_juntos[1:(nrow(datos_juntos)/2),c(5,2,8,9)]
mitad_2=datos_juntos[-(1:(nrow(datos_juntos)/2)),c(5,2,8,9)]

#Vamos a clasificar los datos de esta manera: mirar direccion de era y clasificar en 16 direcciones (como los anemos)
direcciones=seq(0,360,360/16) #Aunque la direccion 0º y 360º sean la misma, por ahora los tratamos ppor separado para facilitar la separacion de las mediciones por direcciones. Luego las juntamos.
mitad_1_por_dirs=list()
mitad_2_por_dirs=list()

for (i in 1:(length(direcciones)-1)) {  #-1 porque no queremos crear un elemento para 360º
  mitad_1_por_dirs[[i]]=data.frame()
  mitad_2_por_dirs[[i]]=data.frame()
}
for (i in 1:nrow(mitad_1)) { #Bucle para clasificar lineas de mitad_1 por direcciones
  (direcciones-mitad_1$uv_dwi1[i]) %>% abs %>% which.min -> x
  if (x==17){x=1} #Si la direccion mas cercana es 360º, a partir de ahora lo contamos como 0º
  #El codigo quedaria mas bonito con rbind, pero esto es mas eficiente (no hay que sobreescribir todo el rato, solo añadir lineas)
  if (length(mitad_1_por_dirs[[x]])==0) {  #Si es la primera linea de la direccion x...
    mitad_1_por_dirs[[x]]=mitad_1[i,] #...ponemos la linea tal cual.
  }else{  #Si no es la primera linea de la direccion x, la añadimos debajo.
    mitad_1_por_dirs[[x]][nrow(mitad_1_por_dirs[[x]])+1,1:ncol(mitad_1)]=mitad_1[i,]
  }
}
for (i in 1:nrow(mitad_2)) { #Bucle para clasificar lineas de mitad_2 por direcciones
  (direcciones-mitad_2$uv_dwi1[i]) %>% abs %>% which.min -> x
  if (x==17){x=1} #Si la direccion mas cercana es 360º, a partir de ahora lo contamos como 0º
  #El codigo quedaria mas bonito con rbind, pero esto es mas eficiente (no hay que sobreescribir todo el rato, solo añadir lineas)
  if (length(mitad_2_por_dirs[[x]])==0) {  #Si es la primera linea de la direccion x...
    mitad_2_por_dirs[[x]]=mitad_2[i,] #...ponemos la linea tal cual.
  }else{  #Si no es la primera linea de la direccion x, la añadimos debajo.
    mitad_2_por_dirs[[x]][nrow(mitad_2_por_dirs[[x]])+1,1:ncol(mitad_2)]=mitad_2[i,]
  }
}
direcciones=direcciones[1:(length(direcciones)-1)] #Ya no nos hace falta el 360º, solo molesta.
names(mitad_1_por_dirs)=direcciones
names(mitad_2_por_dirs)=direcciones

#Crear funciones de transferencia para cada direccion y para cada mitad
lista_ftrans_mitad_1=list()
lista_ftrans_mitad_2=list()
for (i in 1:length(direcciones)) {
  lista_ftrans_mitad_1[[i]]=fitQmapQUANT(mitad_1_por_dirs[[i]]$Mean,mitad_1_por_dirs[[i]]$uv_wind1)
  lista_ftrans_mitad_2[[i]]=fitQmapQUANT(mitad_2_por_dirs[[i]]$Mean,mitad_2_por_dirs[[i]]$uv_wind1)
}
names(lista_ftrans_mitad_1)=direcciones
names(lista_ftrans_mitad_2)=direcciones

#Sacar datos calibrados para cada direccion y para cada mitad
for (i in 1:length(direcciones)) {
  mitad_1_por_dirs[[i]]=doQmapQUANT(mitad_1_por_dirs[[i]]$uv_wind1,lista_ftrans_mitad_2[[i]]) %>% cbind(mitad_1_por_dirs[[i]])
  mitad_2_por_dirs[[i]]=doQmapQUANT(mitad_2_por_dirs[[i]]$uv_wind1,lista_ftrans_mitad_1[[i]]) %>% cbind(mitad_2_por_dirs[[i]])
  colnames(mitad_1_por_dirs[[i]])[1]="Cal"
  colnames(mitad_2_por_dirs[[i]])[1]="Cal"
}

#Reconstruir los datos calibrados de cada mitad como una serie temporal
mitad_1=do.call(rbind,mitad_1_por_dirs) #Juntar las lineas de todas la direcciones en un solo dataframe
mitad_2=do.call(rbind,mitad_1_por_dirs)
mitad_1=mitad_1[order(mitad_1$Date1),]  #Ordenar segun fecha
mitad_2=mitad_2[order(mitad_2$Date1),]
mitad_1=mitad_1[c("Date1","Mean","Cal","uv_wind1","uv_dwi1")] #Reordenar columnas
mitad_2=mitad_2[c("Date1","Mean","Cal","uv_wind1","uv_dwi1")]

#Hora de analizar los resultados mitad_1
cor(mitad_1$Cal,mitad_1$Mean)
cor(mitad_1$Mean,mitad_1$uv_wind1)
cor(mitad_1$Cal,mitad_1$uv_wind1)

#Mean absolute error=mean(abs(error)) [m/s]
mean(abs(mitad_1$Mean-mitad_1$uv_wind1))
mean(abs(mitad_1$Mean-mitad_1$Cal))

#Root Mean Squared Error=sqrt(mean(error^2)) [m/s]
sqrt(mean((mitad_1$Mean-mitad_1$uv_wind1)^2))
sqrt(mean((mitad_1$Mean-mitad_1$Cal)^2))


plot_n_graficos(x=mitad_1$Date1,n=7,mitad_1$Mean,mitad_1$Cal,mitad_2$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 3 mitad_1")

#Hora de analizar los resultados mitad_2
cor(mitad_2$Cal,mitad_2$Mean)
cor(mitad_2$Mean,mitad_2$uv_wind1)
cor(mitad_2$Cal,mitad_2$uv_wind1)

#Mean absolute error=mean(abs(error)) [m/s]
mean(abs(mitad_2$Mean-mitad_2$uv_wind1))
mean(abs(mitad_2$Mean-mitad_2$Cal))

#Root Mean Squared Error=sqrt(mean(error^2)) [m/s]
sqrt(mean((mitad_2$Mean-mitad_2$uv_wind1)^2))
sqrt(mean((mitad_2$Mean-mitad_2$Cal)^2))


plot_n_graficos(x=mitad_2$Date1,n=7,mitad_2$Mean,mitad_2$Cal,mitad_2$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 3 mitad_2")




#Preparar los datos con los que trabajar (pero con todos los puntos de era, haciendo remix)----

#REMIX=para cada instante de era coger el dato de era de aquel punto que tenha mejor correlacion
#con lamdireccion del anemo a esa misma hora.

#El anemo queda ya elegido al principio de este script-
datos_juntos <- readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_juntos.rds")))

#Aunque solo doQmap de problemas con NA (fitQmap los tolera) es mas util quitarlos desde el principio
#Creo que lo de quitar NAs es mejor hacerlo despues de hacer el remix
datos_juntos=datos_juntos[complete.cases(datos_juntos[,grep("uv_wind",colnames(datos_juntos))]),] #Quitar lineas donde era tenga NAs
datos_juntos=datos_juntos[complete.cases(datos_juntos$Mean),] #Quitar lineas donde el anemo tenga NAs

datos_juntos$Dir=as.numeric(datos_juntos$Dir)

#CUARTO EJERCICIO. Dividir datos_remix por la mitad y hacer una calibracion para cada direccion de era.----

#Dividir datos por la mitad. Por ahora vamos con todas las columnas (en un futuro se podria optimizar quitando todas las columnas de fecha repetidas) 
mitad_1_juntos=datos_juntos[1:(nrow(datos_juntos)/2),]
mitad_2_juntos=datos_juntos[-(1:(nrow(datos_juntos)/2)),]

remixear_datos_juntos=function(datos_juntos){
  
  #Esta funcion recibe un dataframe de tipo datos_juntos y escoge los datos necesarios para devolver un datos_remix.
  #EXPLICACION datos_juntos: dataframe que tiene para cada linea los datos de anemo (Date, Mean, Gust y Dir) y de
  #n puntos de era (Date_eran,lonn,latn,uv_windn y uv_dwin, sustituyendo la n final con el numero))
  #EXPLICACION datos_remix: dataframe que tiene para cada linea los datos de anemo (Date, Mean, Gust y Dir) y del
  #punto de era que haya demostrado tener mejor correlacion  con las mediciones de datos_juntos. lat y lon permiten
  #conocer el punto de era con el que se ha completado cada linea.
  
  #Que punto vamos a usar para cada direccion?
  cors_por_dirs=crear_tabla_cors_por_dirs(datos_juntos,añadir_porcentajes = F) #Tabla sin porcentajes!
  apply(cors_por_dirs,1, which.max) #El 1 quiere decir que aplicamos la funcion which.max a cada COLUMNA de la tabla.
  puntos_era_para_cada_dir=apply(cors_por_dirs,1, which.max)  #Esto devuelve un "named int": contiene tanto la direccion como el punto de era mas apropiado.
  
  #Construir datos_remix
  datos_remix=datos_juntos[,c("Date","Mean","Gust","Dir")]
  datos_remix[,c("Date_era","lon","lat","uv_wind","uv_dwi")]=NA
  class(datos_remix[,c("Date_era","lon","lat","uv_wind","uv_dwi")])=class(datos_juntos[,c("Date_era","lon","lat","uv_wind","uv_dwi")])  #Esto es necesario para que luego no nos ponga la fecha en numerico
  for (i in 1:nrow(datos_remix)) {
    punto=puntos_era_para_cada_dir[which(names(puntos_era_para_cada_dir)==datos_juntos$Dir[i])]
    columnas=grep(as.character(punto),colnames(datos_juntos))
    datos_remix[i,c("Date_era","lon","lat","uv_wind","uv_dwi")]=datos_juntos[i,columnas,drop = FALSE]  #Esta linea nos jode las fechas, las pone en numerico
  }
  return(datos_remix)
}

mitad_1=remixear_datos_juntos(mitad_1_juntos)
mitad_2=remixear_datos_juntos(mitad_2_juntos)

#INCOMING bloque de codigo copiado del tercer ej y modificado
#Vamos a clasificar los datos de esta manera: mirar direccion de era y clasificar en 16 direcciones (como los anemos)
direcciones=seq(0,360,360/16) #Aunque la direccion 0º y 360º sean la misma, por ahora los tratamos ppor separado para facilitar la separacion de las mediciones por direcciones. Luego las juntamos.
mitad_1_por_dirs=list()
mitad_2_por_dirs=list()

for (i in 1:(length(direcciones)-1)) {  #-1 porque no queremos crear un elemento para 360º
  mitad_1_por_dirs[[i]]=data.frame()
  mitad_2_por_dirs[[i]]=data.frame()
}
for (i in 1:nrow(mitad_1)) { #Bucle para clasificar lineas de mitad_1 por direcciones
  (direcciones-mitad_1$uv_dwi[i]) %>% abs %>% which.min -> x
  if (x==17){x=1} #Si la direccion mas cercana es 360º, a partir de ahora lo contamos como 0º
  #El codigo quedaria mas bonito con rbind, pero esto es mas eficiente (no hay que sobreescribir todo el rato, solo añadir lineas)
  if (length(mitad_1_por_dirs[[x]])==0) {  #Si es la primera linea de la direccion x...
    mitad_1_por_dirs[[x]]=mitad_1[i,] #...ponemos la linea tal cual.
  }else{  #Si no es la primera linea de la direccion x, la añadimos debajo.
    mitad_1_por_dirs[[x]][nrow(mitad_1_por_dirs[[x]])+1,1:ncol(mitad_1)]=mitad_1[i,]
  }
}
for (i in 1:nrow(mitad_2)) { #Bucle para clasificar lineas de mitad_2 por direcciones
  (direcciones-mitad_2$uv_dwi[i]) %>% abs %>% which.min -> x
  if (x==17){x=1} #Si la direccion mas cercana es 360º, a partir de ahora lo contamos como 0º
  #El codigo quedaria mas bonito con rbind, pero esto es mas eficiente (no hay que sobreescribir todo el rato, solo añadir lineas)
  if (length(mitad_2_por_dirs[[x]])==0) {  #Si es la primera linea de la direccion x...
    mitad_2_por_dirs[[x]]=mitad_2[i,] #...ponemos la linea tal cual.
  }else{  #Si no es la primera linea de la direccion x, la añadimos debajo.
    mitad_2_por_dirs[[x]][nrow(mitad_2_por_dirs[[x]])+1,1:ncol(mitad_2)]=mitad_2[i,]
  }
}
direcciones=direcciones[1:(length(direcciones)-1)] #Ya no nos hace falta el 360º, solo molesta.
names(mitad_1_por_dirs)=direcciones
names(mitad_2_por_dirs)=direcciones

#Crear funciones de transferencia para cada direccion y para cada mitad
lista_ftrans_mitad_1=list()
lista_ftrans_mitad_2=list()
for (i in 1:length(direcciones)) {
  lista_ftrans_mitad_1[[i]]=fitQmapQUANT(mitad_1_por_dirs[[i]]$Mean,mitad_1_por_dirs[[i]]$uv_wind)
  lista_ftrans_mitad_2[[i]]=fitQmapQUANT(mitad_2_por_dirs[[i]]$Mean,mitad_2_por_dirs[[i]]$uv_wind)
}
names(lista_ftrans_mitad_1)=direcciones
names(lista_ftrans_mitad_2)=direcciones

#Sacar datos calibrados para cada direccion y para cada mitad
for (i in 1:length(direcciones)) {
  mitad_1_por_dirs[[i]]=doQmapQUANT(mitad_1_por_dirs[[i]]$uv_wind,lista_ftrans_mitad_2[[i]]) %>% cbind(mitad_1_por_dirs[[i]])
  mitad_2_por_dirs[[i]]=doQmapQUANT(mitad_2_por_dirs[[i]]$uv_wind,lista_ftrans_mitad_1[[i]]) %>% cbind(mitad_2_por_dirs[[i]])
  colnames(mitad_1_por_dirs[[i]])[1]="Cal"
  colnames(mitad_2_por_dirs[[i]])[1]="Cal"
}

#Reconstruir los datos calibrados de cada mitad como una serie temporal
mitad_1=do.call(rbind,mitad_1_por_dirs) #Juntar las lineas de todas la direcciones en un solo dataframe
mitad_2=do.call(rbind,mitad_1_por_dirs)
mitad_1=mitad_1[order(mitad_1$Date_era),]  #Ordenar segun fecha
mitad_2=mitad_2[order(mitad_2$Date_era),]
mitad_1=mitad_1[c("Date_era","Mean","Cal","uv_wind","uv_dwi")] #Reordenar columnas
mitad_2=mitad_2[c("Date_era","Mean","Cal","uv_wind","uv_dwi")]

#OUTCOMING (ver comentario con INCOMING mas arriba)

#Hora de analizar los resultados mitad_1
cor(mitad_1$Cal,mitad_1$Mean)
cor(mitad_1$Mean,mitad_1$uv_wind)
cor(mitad_1$Cal,mitad_1$uv_wind)

accuracy(f = mitad_1$uv_wind,x = mitad_1$Mean)
accuracy(f = mitad_1$Cal,x = mitad_1$Mean)

computeMASE <- function(forecast,train,test,period){
  #Encontrado en:
  #https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments
  
  # forecast - forecasted values
  # train - data used for forecasting .. used to find scaling factor
  # test - actual data used for finding MASE.. same length as forecast
  # period - in case of seasonal data.. if not, use 1
  
  forecast <- as.vector(forecast)
  train <- as.vector(train)
  test <- as.vector(test)
  
  n <- length(train)
  scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)
  
  et <- abs(test-forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt)
  return(meanMASE)
}

computeMASE(mitad_1$uv_wind,mitad_1$uv_wind,mitad_1$Mean,1)
computeMASE(mitad_1$Cal,mitad_1$uv_wind,mitad_1$Mean,1)

plot_n_graficos(x=mitad_1$Date_era,n=7,mitad_1$Mean,mitad_1$Cal,mitad_1$uv_wind,leyenda = c("Mean","Cal","ERA"),main = "Ej 4 mitad_1",col = c("red","green","grey"))

#Hora de analizar los resultados mitad_2
cor(mitad_2$Cal,mitad_2$Mean)
cor(mitad_2$Mean,mitad_2$uv_wind)
cor(mitad_2$Cal,mitad_2$uv_wind)

accuracy(f = mitad_2$uv_wind,x = mitad_2$Mean)
accuracy(f = mitad_2$Cal,x = mitad_2$Mean)




plot_n_graficos(x=mitad_2$Date_era,n=7,mitad_2$Mean,mitad_2$Cal,mitad_2$uv_wind,leyenda = c("Mean","Cal","ERA"),main = "Ej 4 mitad_2")




#EXTRA. Comprobar efectividad del remix----

datos_remix=remixear_datos_juntos(datos_juntos)
#Añadimos las columnas del remix de datos_remix y lo metemos en datos_juntos. Asi trabajamos con un solo dataframe
datos_juntos[,c("Date_era","lon","lat","uv_wind","uv_dwi")]=NA
datos_juntos[,c("Date_era","lon","lat","uv_wind","uv_dwi")]=datos_remix[,c("Date_era","lon","lat","uv_wind","uv_dwi")]
rm(datos_remix)

#Analizar resultados
cors_por_dirs=crear_tabla_cors_por_dirs(datos_juntos = datos_juntos,añadir_porcentajes = F) #remix vs mean  es la columna llamada "10"
puntos_era_para_cada_dir=apply(cors_por_dirs,1, which.max)  #Esto devuelve un "named int": contiene tanto la direccion como el punto de era mas apropiado.
for (i in grep("uv_wind",colnames(datos_juntos))) { #Vector: Numeros de columnas cuyos nombres contengan "uv_wind
  print(paste0(colnames(datos_juntos)[i]," - ",cor(datos_juntos[,i],datos_juntos$Mean)))
  print(accuracy(datos_juntos[,i],datos_juntos$Mean))
  print('')
}

#CONCLUSIONES----

#1) En el caso de los datos de la uni, ERA suele dar valores mas altos o iguales a los
#de los anemos. Por lo tanto, al hacer quantile matching, el calibrado se acerca mucho
#a los anemos cuando ERA es mayor que los anemos, pero cuando ERA se acerca el calibrado
#subestima el viento. Si se pudiese saber cuando es ERA mucho mayor que los anemos y
#cuando parecido, se podría calibrar cada caso independientemente.

#2) Aunque haya estado sacando correlaciones para comprobar los resultados de este
#script, me he dado cuenta de que no es lo ideal: la correacion tiene en cuenta como
#van variando dos vectores mientras que el quantile matching (por lo que tengo
#entendido) cambia el modulo. Por lo que aunque hagamos el mejor quantile matching
#del mundo,la correlacion siempre nos va a decir que los datos calibrados se parecen
#mucho mas a ERA que a las mediciones.

#3) El hacer quantle matching no dejaría obsoleto lo del factor K diferencia de
#modulo entre punto ERA y anemo?

#4) Aunque el quantile matching no acabe de clavarla al %100 tampoco nos debería
#importar mucho, ya que no queremos saber el viento preciso que va a hacer en cada
#momento; solo queremos hacer una estimación de la energia producida en un largo
#periodo de tiempo. Como el quantile  matching hace que las mediciones y los datos
#calibrados tengan modulos parecidos, es perfecto para nosotros, auqneu no sea capaz
#de predecir como va a ser el viento en cada momento.