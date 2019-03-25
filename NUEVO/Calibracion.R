library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros----

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  

#Crear & procesar datos_anemos----

datos_anemos=rellenar_huecos_anemos(Anemometros$`0B38DAE79059`)
#huecos=buscar_huecos_anemos(Anemometros$`0B38DAE79059`)  #Para saber donde nos ha metido NAs la funcion rellenar_huecos_anemos

#Ordenar cronologicamente datos_anemos
datos_anemos=datos_anemos[order(datos_anemos$Date),]

#Encontrar posiciones de mediciones supuestamente erroneas
lista_pos_errores=filtrar_datos(datos_anemos,desglosar = TRUE)
N_errores=unique(c(lista_pos_errores$N1_mean,lista_pos_errores$N2_mean,lista_pos_errores$N3_mean,
                   lista_pos_errores$N1_gust,lista_pos_errores$N2_gust,lista_pos_errores$N3_gust))


#Donde hay NAs? Marcar en rojo mas tarde
N_na=which(rowSums(is.na(datos_anemos[,c(2,3,4)]))>0)

#Ploteos de datos_anemos----

#Plotear de n en n (que datos_anemos este en orden cronologico!)
#Leyenda:
  #Rojo = NA
  #Berde = error segun filtro nivel 1 (valores negativos o muy altos)
  #Azul = error segun filtro nivel 2 (cambios demasiado bruscos)
  #Violeta = error segun filtro nivel 3 (viento sospechosamente estable)
graphics.off()
dev.off()
n=nrow(datos_anemos)/1   #No hace falta redondear, los corchetes [] redondean siempre para abajo
#n=nrow(datos_anemos)   #Para plotear todo junto
for (i in seq(1,nrow(datos_anemos),n)) {
  layout(mat = c(1,2))  #Separar la ventana de plots en dos, una para mean, otro para gust
  #Mean
  plot(datos_anemos$Mean[i:(i+n-1)],x = datos_anemos$Date[i:(i+n-1)],col="grey",type="p",xlab="",ylab="Mean [m/s]")
  points(x = datos_anemos$Date[N_na],y = rep_len(0,length(datos_anemos$Date[N_na])),col="red",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N1_mean],y = datos_anemos$Mean[lista_pos_errores$N1_mean],col="green",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N2_mean],y = datos_anemos$Mean[lista_pos_errores$N2_mean],col="blue",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N3_mean],y = datos_anemos$Mean[lista_pos_errores$N3_mean],col="violet",lwd=1)
  title(main = paste0(datos_anemos$Date[i]," - ",datos_anemos$Date[i+n-1]))
  #Gust
  plot(datos_anemos$Gust[i:(i+n-1)],x = datos_anemos$Date[i:(i+n-1)],col="grey",type="p",xlab="",ylab="Gust [m/s]")
  points(x = datos_anemos$Date[N_na],y = rep_len(0,length(datos_anemos$Date[N_na])),col="red",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N1_gust],y = datos_anemos$Mean[lista_pos_errores$N1_gust],col="green",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N2_gust],y = datos_anemos$Mean[lista_pos_errores$N2_gust],col="blue",lwd=1)
  points(x = datos_anemos$Date[lista_pos_errores$N3_gust],y = datos_anemos$Mean[lista_pos_errores$N3_gust],col="violet",lwd=1)
  }
rm(n)

#Rosas de los vientos
windRose(datos_anemos,ws = "Mean",wd="Dir",paddle = F,key.header = "Mean [m/s]")
windRose(datos_anemos,ws = "Gust",wd="Dir",paddle = F,key.header = "Gust [m/s]")

#Quitar mediciones erroneas, guardar, cargar----
#Llenar de NAs las mediciones consideradas erroneas. No eliminamos la fila; queremos mantener la fechas de los NAs.
datos_anemos[N_errores,c(2,3,4)]=NA
datos_anemos[N_na,c(2,3,4)]=NA    #Esto parece redundante pero viene bien asegurarse

#Quitar los primeros datos de los anemos de la uni, que no sirven de nada
datos_anemos=datos_anemos[-(1:which(as.character(datos_anemos$Date)=="2018-05-21 10:13:42")),]

#Guardar los resultados
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){
  dir.create(here::here("NUEVO/Data_calibracion"))
}
save(datos_anemos,
     file=here::here("NUEVO/Data_calibracion/datos_uni_tratados.Rdata"))

#Cargarlos
load(here::here("NUEVO/Data_calibracion/datos_uni_tratados.Rdata"))

#Tratar datos era----
#Coordenadas que queremos representar

Coordenadas_anemos=data.frame(a=as.numeric(),b=as.numeric())
colnames(Coordenadas_anemos)=c("lat","lon")
Coordenadas_anemos[1,]=c(43.179389,-2.488504)

#Pongo este if por que el comando unique tarda lo suyo, para evitar que se ejecute mas de lo necesario
if (!exists("Coordenadas_era")) {
  Coordenadas_era=unique(ERA5_df[,c(2,3)])
}
#Ordenarlos de cercanos a lejanos
Coordenadas_era=Coordenadas_era[order((Coordenadas_era$lon-Coordenadas_anemos[1,2])^2+(Coordenadas_era$lat-Coordenadas_anemos[1,1])^2),]
#Coger los n mas cercanos
n=1
Coordenadas_era=Coordenadas_era[1:n,]

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
datos_era=ERA5_df[which((ERA5_df$lon==Coordenadas_era$lon)&(ERA5_df$lat==Coordenadas_era$lat)),]

#Guardar datos_era
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){
  dir.create(here::here("NUEVO/Data_calibracion"))
}
save(datos_era,
     file=here::here("NUEVO/Data_calibracion/datos_era.Rdata"))

#Cargarlos
load(here::here("NUEVO/Data_calibracion/datos_era.Rdata"))

#Plotear datos_era----
#Plotear de n en n
graphics.off()
dev.off()
n=nrow(datos_era)/1   #No hace falta redondear, los corchetes [] redondean siempre para abajo
#n=nrow(datos_era)   #Para plotear todo junto
#NUestros datos estan ordenados cronologicamente! 
for (i in seq(1,nrow(datos_era),n)) {
  #layout(c(1,2))
  #uv
  plot(datos_era$uv_wind[i:(i+n-1)],x = datos_era$Date[i:(i+n-1)],type="p",col="grey",xlab="",ylab="uv_wind")
  title(main = paste0(datos_era$Date[i]," - ",datos_era$Date[i+n-1]))
  #wind
  #plot(datos_era$wind[i:(i+n-1)],x = datos_era$Date[i:(i+n-1)],type="p",xlab="",ylab="wind")
}
rm(n)

#Si da error "Error in plot.window(...) : need finite 'ylim' values", probar ploteando con n=nrow(datos_era).

#Rosas de los vientos
windRose(datos_era,ws = "uv_wind",wd="uv_dwi",paddle = F,key.header = "uv_wind")
#windRose(datos_era,ws = "wind",wd="dwi",paddle = F,key.header = "wind")

#Pongo este if por que el comando unique tarda lo suyo, para evitar que se ejecute mas de lo necesario
if (!exists("Coordenadas_era")) {
  Coordenadas_era=unique(ERA5_df[,c(2,3)])
}
#Ordenarlos de cercanos a lejanos
Coordenadas_era=Coordenadas_era[order((Coordenadas_era$lon-Coordenadas_anemos[1,2])^2+(Coordenadas_era$lat-Coordenadas_anemos[1,1])^2),]
#Coger los n mas cercanos
n=4
Coordenadas_era=Coordenadas_era[1:n,]

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
datos_era=ERA5_df[which((ERA5_df$lon==Coordenadas_era$lon)&(ERA5_df$lat==Coordenadas_era$lat)),]


#Comparar anemos con era----

#Para ejecutar esta seccion datos_era y datos_anemos deberian existir en el environment
load(here::here("NUEVO/Data_calibracion/datos_era.Rdata"))
load(here::here("NUEVO/Data_calibracion/datos_uni_tratados.Rdata"))

datos_uni=juntar_datos2(datos_era,datos_anemos)

#Separar por direcciones de anemos
datos_uni_dir=list()
dirs=unique(datos_uni$Dir)      #Que direcciones tenemos en el anemo?
dirs=dirs[-which(is.na(dirs))]  #Quitar NA
dirs=as.numeric(dirs)           #ESTAN EN CHARACTER!  Cambiar a numerico para que se ordenen bien
dirs=dirs[order(dirs)]          #Ordenar de menor a mayor
for (i in 1:length(dirs)) {
  datos_uni_dir[[i]]=datos_uni[which(datos_uni$Dir==dirs[i]),]
}
names(datos_uni_dir)=dirs       #Los elementos de la lista se llamaran como la direcion que les corresponde

#Correlaciones por direcciones
cors_dir=data.frame()  #Aqui iran las correlaciones correspondientes a cada direccion
cors_dir[1:length(datos_uni_dir),1]=names(datos_uni_dir)  #Col 1=las direcciones
for (i in 1:length(datos_uni_dir)) {  
  cors_dir[i,2]=cor(datos_uni_dir[[i]]$uv_wind,datos_uni_dir[[i]]$Mean,"na") #Col 2=las correlaciones
  cors_dir[i,3]=nrow(datos_uni_dir[[i]])*100/sum(unlist(lapply(datos_uni_dir,nrow)))  #Col 3=porcentaje de datos en esa direccion
}
colnames(cors_dir)=c("Dir","cor","%")

windRose(mydata = datos_uni,ws="Mean",wd = "Dir",paddle = F)
windRose(mydata = datos_uni,ws="uv_wind",wd="uv_dwi",paddle = F)
windRose(mydata = datos_uni,ws="Mean",wd = "Dir",ws2="uv_wind",wd2="uv_dwi",paddle = F)



n1=500
n2=nrow(datos_uni)
plot(datos_uni$Date[n1:n2],datos_uni$Mean[n1:n2],type="l")
lines(datos_uni$Date[n1:n2],datos_uni$uv_wind[n1:n2],col="grey")





#cut(datos_uni_dir[[1]]$uv_dwi, breaks = c(0,seq(11.5,349.5,22.5),360), labels = c(as.numeric(names(datos_uni_dir)),0)) 