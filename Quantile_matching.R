library(qmap)

#transf funtzioa sortu, fobj 
# kalibrazio oinarria, kal_oin
# kalibratu beharrekoa, kal_behar
# kalibratua, kal, periodo guztian  
# tarte, trans funtzioa sortzen den periodoa
# guzti, kalibrazio periodo guztia 


fobj<-fitQmapQUANT(kal_oin, kal_behar[tarte] ) 

kal <- doQmapQUANT(kal_behar[guzti],  fobj)  