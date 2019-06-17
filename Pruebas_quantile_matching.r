library(qmap)

#Preparar los datos con los que trabajar----
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
summary(datos_juntos$Mean)
summary(calibrado)
#Vemos que los quantiles, el max y el min encajan, pero no la media
cor(calibrado,datos_juntos$Mean)
cor(datos_juntos$uv_wind1,datos_juntos$Mean)
cor(calibrado,datos_juntos$uv_wind1)
#Vemos que el calibrado se parece mucho  mas a era que al anemo

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

#
calibrado_todo_1 <- doQmapQUANT(datos_juntos$uv_wind1,ftrans_mitad_1)
calibrado_todo_2 <- doQmapQUANT(datos_juntos$uv_wind1,ftrans_mitad_2)


#Hora de analizar los resultados
cor(calibrado_mitad_1,mitad_1$Mean)
cor(mitad_1$Mean,mitad_1$uv_wind1)
cor(calibrado_mitad_1,mitad_1$uv_wind1)

cor(calibrado_mitad_2,mitad_2$Mean)
cor(mitad_2$Mean,mitad_2$uv_wind1)
cor(calibrado_mitad_2,mitad_2$uv_wind1)

remove(mitad_1,mitad_2,calibrado_mitad_1,calibrado_mitad_2,calibrado_todo_1,calibrado_todo_2,ftrans_mitad_1,ftrans_mitad_2)

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

#Hora de analizar los resultados
cor(mitad_1$Cal,mitad_1$Mean)
cor(mitad_1$Mean,mitad_1$uv_wind1)
cor(mitad_1$Cal,mitad_1$uv_wind1)

cor(mitad_2$Cal,mitad_2$Mean)
cor(mitad_2$Mean,mitad_2$uv_wind1)
cor(mitad_2$Cal,mitad_2$uv_wind1)


#CONCLUSIONES----

#1) Por ahora los datos calibrados se parecen mucho a los de era, y deberian parecerse
#mas a los de los anemos. Por otro lado, esto nos confirma que no estamos haciendo
#minguna burrada: la calibracion, po lo menos, parece viento!
#Esto parece indicar que las ftrans no son lo suficientemente "agresivas", por que
#no cambian de manera sustancial los datos de era.

#2) Los datos calibrados se parecen mas a era que a las mediciones, pero este parecido
#es menos acusado cuando se calibra por direcciones. Parece ser que al calibrar por
#direcciones se consiguen funciones de transferencia mas agresivas, que cambian mas
#los datos. Esto parece una señal de que tratar las distintas direcciones por separado
#es el camino a seguir.