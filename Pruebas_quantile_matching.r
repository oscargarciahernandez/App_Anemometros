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

dev.off()
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

dev.off()
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

dev.off()
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

dev.off()
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

dev.off()
plot_n_graficos(x=mitad_2$Date1,n=7,mitad_2$Mean,mitad_2$Cal,mitad_2$uv_wind1,leyenda = c("Mean","Cal","ERA"),main = "Ej 3 mitad_2")



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