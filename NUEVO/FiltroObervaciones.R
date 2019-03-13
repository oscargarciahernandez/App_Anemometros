#Script para filtrar las mediciones de anemometros tal y como lo mando Sheilla.
#ATENCION! Todos los (m/s) deberian ser (km/h). Aun asi, no lo he modificado aqui.

#Detecci?n de valores erroneos 


#Definici?n de valores perdidos

#Formato fichero
names(Datosdf)<-c("Fecha","vel","dir")

#Fitros de viento medio
#Nivel 1 -- limites
#Velocidad [0,50] (m/s)
N<-which(Datosdf$vel<=0)
if (length(N)!=0){
  Datosdf$vel[N]<-NA
}
N<-which(Datosdf$vel>50)
if (length(N)!=0){
  Datosdf$vel[N]<-NA
}

#Direccion [0,360]
N<-which(Datosdf$dir>360 | Datosdf$dir<0)
if (length(N)!=0){
  Datosdf$dir[N]<-NA
}

#Nivel 2 -- coherencia temporal del dato 
#Step test:
#Velocidad diferencia con el dato anterior de 30 m/s tanto si la diferencia es + como si es -
i2<-NULL
for (i in 2:dim(Datosdf)[1]){
  difer<-Datosdf[c(c(i-1)),2]-Datosdf[c(c(i)),2]
  if (is.na(difer)==FALSE & abs(difer)>30){
    #Datosdf$vel[i]<-NA
    print(i)
    i2<-cbind(i,i2)
  }
}

#Nivel 4 -- coherencia temporal de la serie
# Velocidad 
# En 1 horas (6 tomas) que la velocidad no varie en 0.1
N2<-c()
for (i in 6:c(dim(Datosdf)[1])){
 if(is.na(Datosdf$vel[i])==FALSE){
     difer<-max(Datosdf[c((i-5):i),2],na.rm=TRUE)-min(Datosdf[c((i-5):i),2],na.rm=TRUE)
     if (is.na(difer)==FALSE & abs(difer)<=0.1){
       #Datosdf$vel[i]<-NA
       N2<-c(N2,i)
     }
 }
}

# Direcci?n
# En 1 hora que la direcci?n no varie en 1
N3<-c()
for (i in 6:c(dim(Datosdf)[1])){
 if(is.na(Datosdf$dir[i])==FALSE){
     difer<-max(Datosdf$dir[(i-5):i],na.rm=TRUE)-min(Datosdf$dir[(i-5):i],na.rm=TRUE)
     if (is.na(difer)==FALSE & abs(difer)<=1){
       N3<-c(N3,i)
     }
 }
}


# Direcci?n
# En 1 hora no varia nada si no tenemos en cuenta los 0s
N4<-c()
for (i in 6:c(dim(Datosdf)[1])){
  if(is.na(Datosdf$dir[i])==FALSE){
    data<-Datosdf$dir[(i-5):i]
    n_data<-which(data==0)
    data<-data[-n_data]
    if(is.na(data)==FALSE){
        difer<-max(data,na.rm=TRUE)-min(data,na.rm=TRUE)
        if (is.na(difer)==FALSE & abs(difer)<=1){
          N4<-c(N4,i)
        }
    }
  }
}

