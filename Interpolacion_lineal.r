library(rlist)
library(here)
library(lubridate)

#Cargar datos.
datos=list.load(paste0(here::here(),"/data/Datos_Anemometros/Datos_anemometros.rdata",collapse = NULL))
#Para probar cojo solo el primero de la lista
datos=datos$`0B75FE3A4FB6`

#Asignamos el instante al que queremos redondear:mediodia del 27 de noviembre
instante0=dmy_hms("27/11/2018-12:00:00")

#Buscar los dos datos mas cercanos

for (i in 1:(length(datos$Date)-1)) {
  if (instante0 %within% interval(datos$Date[i+1],datos$Date[i])) {   #Si nuestro instante esta en el intervalo compuesto por el instante i de la lista y e i+1---
    dato_antes=datos[i+1,]  #GUardamos las lineas de datos que nos interesan
    dato_despues=datos[i,]
  break   #Como ya hemos encontrado lo que queriamos, paramos el for
  }
  
}

#En este parrafo creamos y llenamos dato_interpolacion
dato_interpolacion=dato_antes #Esto es simplemente para tenerlo en el mismo formato. (Un dataframe con los mismos nombre de columnas)
#Ahora a sobreescribir:
#Interpolar mean
dato_interpolacion$Mean=(dato_despues$Mean-dato_antes$Mean)*as.numeric(instante0-dato_antes$Date)/as.numeric(dato_despues$Date-dato_antes$Date)+dato_antes$Mean
#Interpolar max
dato_interpolacion$Max=(dato_despues$Max-dato_antes$Max)*as.numeric(instante0-dato_antes$Date)/as.numeric(dato_despues$Date-dato_antes$Date)+dato_antes$Max
#Interpolar la direccion
if (dato_antes$Dir_deg==dato_despues$Dir_deg) { #Si antes y despues no ha cambiado la direccion, pues no hay que hacer nada!
  else  #Si resulta que ha cambiado...
    if (abs(dato_antes$Dir_deg-dato_despues$Dir_deg)==180) {
      dato_interpolacion$Dir_deg=NA #Si un viento viene de 0º y otro de 180º, no podemos hacer una interpolacion (¿Cual seria, 90 o 270?)
      break #Si ocurre esto, no interpolamos los grados
    }
    if (dato_antes$Dir_deg>180) { #Estos dos if solucionan el problema de la interpolacion de grados
      dato_antes$Dir_deg=dato_antes$Dir_deg-360
    }
    if (dato_despues$Dir_deg>180) {
      dato_despues$Dir_deg=dato_despues$Dir_deg-360
    }
    #Hacemos la interpolacion en si
  dato_interpolacion$Dir_deg=(dato_despues$Dir_deg-dato_antes$Dir_deg)*as.numeric(instante0-dato_antes$Date)/as.numeric(dato_despues$Date-dato_antes$Date)+dato_antes$Dir_deg
  if (dato_interpolacion$Dir_deg<0) { #Si la interpolacion de los grados nos da negativo le sumamos 360º
    dato_interpolacion$Dir_deg=dato_interpolacion$Dir_deg+360
  }  
  dato_interpolacion$Dir_ch=NA #Ya no tiene sentido tener la direccion en character
}
#Le ponemos el instante que le corresponde
dato_interpolacion$Date=instante0
