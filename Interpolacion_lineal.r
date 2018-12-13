library(rlist)
library(here)
library(lubridate)

#Cargar datos.
datos=list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros.rdata",collapse = NULL))
#Para probar cojo solo el primero de la lista
datos=datos$`0B75FE3A4FB6`
fechainicio<-round_date(range(datos$Date)[1],unit = "hours")
fechafinal<-round_date(range(datos$Date)[2],unit = "hours")
Vector_fechas<-seq(fechainicio,fechafinal, by="hours") #Todas la fechas a las que vamos a redondear

#dataframe_fechas=cbind(Vector_fechas,NA,NA)
#colnames(dataframe_fechas)<-c(instante0,)
#Asignamos el instante al que queremos redondear:mediodia del 27 de noviembre
#instante0=dmy_hms("27/11/2018-12:00:00") #con Vector_fechas esto queda obsoleto

#Buscar los dos datos mas cercanos
datos_antes=as.data.frame(matrix(NA,nrow=length(Vector_fechas),ncol=5)) #Contendra el instante de antes para cada instante de Vector_fechas
colnames(datos_antes)<-colnames(datos)
datos_despues=datos_antes #Por ahora son iguales por que estan vacios
for (i in 1:length(Vector_fechas)) {
  diferencias<-(Vector_fechas[i]-datos$Date)  #QUe diferencia de tiempo hay? Positivos=antes;negativos=despues
  if (sum(diferencias>0)==0 | sum(diferencias<0)==0 ) {  #Si no hay ningun datos anterior o posterior, no interpolamos
    datos_antes[i,]=NA
    datos_despues[i,]=NA
  }else{
    datos_antes[i,]=datos[which(diferencias==min(diferencias[which(diferencias>0)])),]  #Guardamos la que tenga la diferencia positiva mas pequeña
    datos_despues[i,]=datos[which(diferencias==min(diferencias[which(diferencias<0)])),] # +1 porque la de despues era el dato que esta 
  }
  
}

#En este parrafo sobreescribimos llenamos datos_interpolacion
datos_interpolacion=datos_antes #Aprovechamos para dejar esto vacio
for (i in 1:length(Vector_fechas)) {
  if (!is.na(datos_antes[i,1])) { #Solo interpolamos si no hemos guardado NA en el for anterior
    #Interpolar mean
    datos_interpolacion$Mean[i]=(datos_despues$Mean[i]-datos_antes$Mean[i])*as.numeric(Vector_fechas[i]-datos_antes$Date[i])/as.numeric(datos_despues$Date[i]-datos_antes$Date[i])+datos_antes$Mean[i]
    #Interpolar max
    datos_interpolacion$Max[i]=(datos_despues$Max[i]-datos_antes$Max[i])*as.numeric(Vector_fechas[i]-datos_antes$Date[i])/as.numeric(datos_despues$Date[i]-datos_antes$Date[i])+datos_antes$Max[i]
    #Interpolar la direccion
    if (datos_antes$Dir_deg[i]==datos_despues$Dir_deg[i]) { #Si antes y despues no ha cambiado la direccion, pues no hay que hacer nada!
    }else{  #Si resulta que ha cambiado...
      if (abs(datos_antes$Dir_deg[i]-datos_despues$Dir_deg[i])==180) {
        datos_interpolacion$Dir_deg[i]=NA #Si un viento viene de 0º y otro de 180º, no podemos hacer una interpolacion (¿Cual seria, 90 o 270?)
      }
      if (datos_antes$Dir_deg[i]>180) { #Estos dos if solucionan el problema de la interpolacion de grados
        datos_antes$Dir_deg[i]=datos_antes$Dir_deg[i]-360
      }
      if (datos_despues$Dir_deg[i]>180) {
        datos_despues$Dir_deg[i]=datos_despues$Dir_deg[i]-360
      }
      #Hacemos la interpolacion en si
      datos_interpolacion$Dir_deg[i]=(datos_despues$Dir_deg[i]-datos_antes$Dir_deg[i])*as.numeric(Vector_fechas[i]-datos_antes$Date[i])/as.numeric(datos_despues$Date[i]-datos_antes$Date[i])+datos_antes$Dir_deg[i]
      if (datos_interpolacion$Dir_deg[i]<0) { #Si la interpolacion de los grados nos da negativo le sumamos 360º
        datos_interpolacion$Dir_deg[i]=datos_interpolacion$Dir_deg[i]+360
      }  
      datos_interpolacion$Dir_ch[i]=NA #Ya no tiene sentido tener la direccion en character
    }
    #Le ponemos el instante que le corresponde
    datos_interpolacion$Date[i]=Vector_fechas[i]
    }
  
}

