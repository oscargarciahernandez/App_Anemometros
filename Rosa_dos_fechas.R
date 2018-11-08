library(lubridate)
library(here)
library(rlist)

#Descargar datos ----
datos=list.load(here::here("data/Datos_anemometros.rdata"))
  
#Pedir input de fechas y de sensor ----
#De entre que dos fechas se escogeran los datos?
dia_1=readline(prompt = "Dia?")
mes_1=readline(prompt = "Mes?")
año_1=readline(prompt = "Año?")
dia_2=readline(prompt = "Dia?")
mes_2=readline(prompt = "Mes?")
año_2=readline(prompt = "Año?")

#Pedimos input de sensor, de  cual quiere hacer la rosa?
list.names(datos)
sensor=readline("Cual de los sensores?")
while(sum(list.names(datos)==sensor)==0){    #Seguir repitiendo hasta darle bien el sensor
  list.names(datos)
  sensor=readline("Cual de los sensores? Escribelo exactamente igual")
  
}

#Ahora que tenemos el input y los datos vamos a escoger las lineas necesarias
x=which(list.names==sensor)   #Vamos a ver en la lista que numero es el sensor. Utilizaremos x para coger datos mas tarde

  
#Borrar variables sobrantes ----
remove(dia_1,dia_2,mes_1,mes_2,año_1,año_2)   #Borrar variables de fechas pedidas al usuario
remove(sensor)  #Borrar otras variables
