library(qmap)

#transf funtzioa sortu, fobj
#Crear la función de transferencia, fobj

# kalibrazio oinarria, kal_oin
# Base de calibracion, kal_oin

# kalibratu beharrekoa, kal_behar
# Lo que se tiene que calibrar, kal_behar

# kalibratua, kal, periodo guztian
# Calibrado, kal, en todo el periodo

# tarte, trans funtzioa sortzen den periodoa
# tarte, el periodo que se crea en la funcion de transferencia

# guzti, kalibrazio periodo guztia
# guzti, todoel periodo de calibración


fobj<-fitQmapQUANT(kal_oin, kal_behar[tarte] ) 

kal <- doQmapQUANT(kal_behar[guzti],  fobj)  