library(here)
source(here::here("NUEVO/Libraries.R"))

#Cargamos la mega tabla ERA5_df y la lista Anemometros----

load(here::here("NUEVO/Data_anemometros/Anemometros.Rdata"))
load(here::here("NUEVO/Data_ERA5/ERA5_df.Rdata"))  

#Datos_anemos:elegir anemo, crear, rellenar, filtrar----

#Que anemo usaremos? Elegir
if (!exists("t_reg")) {
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}
levels(t_reg$ID)  #Que anemos tenemos? Enseña las IDs
#anemo_elegido=levels(t_reg$ID)[1]
anemo_elegido="0B75FE3A4FB6"
datos_anemos=rellenar_huecos_anemos(Anemometros[[which(names(Anemometros)==as.character(anemo_elegido))]])
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
n=nrow(datos_anemos)/10   #No hace falta redondear, los corchetes [] redondean siempre para abajo
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

#Quitar los primeros datos de los anemos, que no sirven de nada
if (anemo_elegido=="0B38DAE79059") {#Uni
  datos_anemos=datos_anemos[-(1:which(as.character(datos_anemos$Date)=="2018-05-21 10:13:42")),]
}
if (anemo_elegido=="0B75FE3A4FB6") {#Hex
  datos_anemos=datos_anemos[-(1:which(as.character(datos_anemos$Date)=="2018-05-17 09:12:48")),]  #Le ponemos la ultima fecha mala
}

#Guardar los resultados
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){
  dir.create(here::here("NUEVO/Data_calibracion"))
}
saveRDS(datos_anemos,
     file=here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_tratado.rds")))

#Cargar los resultados
datos_anemos=readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_tratado.rds")))

#Tratar datos era, guardar, cargar----
#Coordenadas que queremos representar
if (!exists("t_reg")) {#Cargamos la tabla de registro, para poder extraer coordenadas_anemos de anemo_elegido
  t_reg<- read.csv(here::here("NUEVO/Data_anemometros/TABLA_REGISTRO.csv"), sep=";")
}
Coordenadas_anemos=filter(t_reg,ID==anemo_elegido) %>% select(lon,lat)  #Primero lon, luego lat

#Pongo este if por que el comando unique tarda lo suyo, para evitar que se ejecute mas de lo necesario
if (!exists("Coordenadas_era")) {
  Coordenadas_era=unique(ERA5_df[,c(2,3)])
  Coordenadas_era=arrange(Coordenadas_era,lon,lat)  #Asegurarse orden correcto para distm
}
#Ordenarlos de cercanos a lejanos
#Para que distm funcione bien, en las columnas primero lon, luego lat, justo lo contrario de era
Coordenadas_era=Coordenadas_era[order(distm(Coordenadas_era, Coordenadas_anemos, fun = distHaversine)[,1]),]
#Coger los n mas cercanos
n=9
Coordenadas_era=Coordenadas_era[1:n,]
rm(n)

#De todo ERA5_df, coger solo los datos relativos a los puntos de Coordendas_era
datos_era=filter(ERA5_df,lat %in% Coordenadas_era$lat,lon %in% Coordenadas_era$lon)
#Coger solo columnas que nos interesan
datos_era=select(datos_era,Date,"lon",lat,"uv_wind",uv_dwi) #Select acepta nombres de columnas tanto con comillas como sin

#Reorganizar datos_era con juntar_datos. Primero los mas cercanos al anemo
#Por ahora los juntamos en datos_era2
datos_era2=filter(datos_era,lon==Coordenadas_era$lon[1],lat==Coordenadas_era$lat[1])
colnames(datos_era2)[1:ncol(datos_era2)]=paste0(colnames(datos_era2)[1:ncol(datos_era2)],"1")
for (i in 2:nrow(Coordenadas_era)) {
  datos_era2=juntar_datos(datos_era2,
                          filter(datos_era,lon==Coordenadas_era$lon[i],lat==Coordenadas_era$lat[i]),
                          nombres_col_fechas = c("Date1","Date"),
                          coletillas = c("",as.character(i)))
}
#Vamos a dar el cambiazo
datos_era=datos_era2
rm(datos_era2,Coordenadas_anemos,Coordenadas_era)

#Guardar datos_era
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){
  dir.create(here::here("NUEVO/Data_calibracion"))
}
saveRDS(datos_era,
        file=here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_era.rds")))

#Cargarlos
datos_era=readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_era.rds")))

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
datos_anemos=readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_tratado.rds")))
datos_era=readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_era.rds")))

#Le añadimos los datos del anemo a datos_era
datos_juntos=juntar_datos(datos_era,datos_anemos,nombres_col_fechas = c("Date1","Date"))
#Queda mas guapo columnas de anemo a la izquierda y lo de era a la derecha no? (Modo chungo de reordenarlo, pero bueno)
datos_juntos=datos_juntos[,c((ncol(datos_juntos)+1-ncol(datos_anemos)):ncol(datos_juntos),1:(ncol(datos_juntos)-ncol(datos_anemos)))]

#Guardar datos_juntos
if(!dir.exists(here::here("NUEVO/Data_calibracion"))){dir.create(here::here("NUEVO/Data_calibracion"))}
saveRDS(datos_juntos,
     file=here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_juntos.rds")))

#Cargar
datos_juntos=readRDS(here::here(paste0("NUEVO/Data_calibracion/",anemo_elegido,"_juntos.rds")))

#Comparar anemo con todos los puntos
cors_anemo_vs_puntos=c()
col_mean=grep("Mean",colnames(datos_juntos)) #Numeros de columna cuyo nombre contenga "Mean"
for (i in grep("uv_wind",colnames(datos_juntos))) { #Vector: Numeros de columnas cuyos nombres contengan "uv_wind
  cors_anemo_vs_puntos[length(cors_anemo_vs_puntos)+1]=cor(datos_juntos[,i],datos_juntos[,col_mean],"na")
}
rm(col_mean)

#Comparar anemo con todos los puntos por direcciones
cors_anemo_vs_puntos=data.frame()
col_mean=grep("Mean",colnames(datos_juntos)) #Numeros de columna cuyo nombre contenga "Mean"
dirs=unique(na.omit(datos_juntos$Dir))
dirs=dirs[order(as.numeric(dirs))]
kont_i=1
#Bucle para correlaciones
for (i in grep("uv_wind",colnames(datos_juntos))) { #Vector: Numeros de columnas cuyos nombres contengan "uv_wind
  kont_j=1
  for (j in dirs) {
    cors_anemo_vs_puntos[kont_j,kont_i]=cor(datos_juntos[which(datos_juntos$Dir==j),i],datos_juntos[which(datos_juntos$Dir==j),col_mean],"na")
  kont_j=kont_j+1
  }
  kont_i=kont_i+1
}
#Bucle para porcentajes
porcentajes_dir=c()
for (i in dirs) {
  porcentajes_dir[i]=nrow(filter(datos_juntos,Dir==i))*100/nrow(datos_juntos)  #Col 3=porcentaje de datos en esa direccion
}
cors_anemo_vs_puntos[,ncol(cors_anemo_vs_puntos)+1]=porcentajes_dir
colnames(cors_anemo_vs_puntos)=c(1:(ncol(cors_anemo_vs_puntos)-1),"%")
row.names(cors_anemo_vs_puntos)=dirs
rm(col_mean,kont_i,kont_j,porcentajes_dir)

#Separar por direcciones de anemos (creo que esto queda obsoleto)
datos_juntos_dir=list()
dirs=unique(datos_juntos$Dir)      #Que direcciones tenemos en el anemo?
dirs=dirs[-which(is.na(dirs))]  #Quitar NA
dirs=as.numeric(dirs)           #ESTAN EN CHARACTER!  Cambiar a numerico para que se ordenen bien
dirs=dirs[order(dirs)]          #Ordenar de menor a mayor
for (i in 1:length(dirs)) {
  datos_juntos_dir[[i]]=datos_juntos[which(datos_juntos$Dir==dirs[i]),]
}
names(datos_juntos_dir)=dirs       #Los elementos de la lista se llamaran como la direcion que les corresponde

#Correlaciones por direcciones (creo que esto queda obsoleto)
cors_dir=data.frame()  #Aqui iran las correlaciones correspondientes a cada direccion
cors_dir[1:length(datos_juntos_dir),1]=names(datos_juntos_dir)  #Col 1=las direcciones
for (i in 1:length(datos_juntos_dir)) {  
  cors_dir[i,2]=cor(datos_juntos_dir[[i]]$uv_wind,datos_juntos_dir[[i]]$Mean,"na") #Col 2=las correlaciones
  cors_dir[i,3]=nrow(datos_juntos_dir[[i]])*100/sum(unlist(lapply(datos_juntos_dir,nrow)))  #Col 3=porcentaje de datos en esa direccion
}
colnames(cors_dir)=c("Dir","cor","%")

windRose(mydata = datos_juntos,ws="Mean",wd = "Dir",paddle = F)
windRose(mydata = datos_juntos,ws="uv_wind",wd="uv_dwi",paddle = F)
windRose(mydata = datos_juntos,ws="Mean",wd = "Dir",ws2="uv_wind",wd2="uv_dwi",paddle = F)



n1=500
n2=nrow(datos_juntos)
plot(datos_juntos$Date[n1:n2],datos_juntos$Mean[n1:n2],type="l")
lines(datos_juntos$Date[n1:n2],datos_juntos$uv_wind[n1:n2],col="grey")

#Calculo factor K diferencia de modulo entre punto ERA y anemo
zo=3 #[m] Centers of cities with tall buildings - Manwell pag 46, tabla 2.2
z=155 + 3.5*6 + 1.5 #[m] Altura anemo = altitud segun google earth + altura edificio + altura poste anemo
zr=401 + 10 #[m] Altura era = altitud segun google earth + 10m
k=log(z/zo)/log(zr/zo)  #k=U(z)/U(zr)


plot(x=datos_juntos$Date,y=datos_juntos$Mean,col="red",type="l")
lines(x=datos_juntos$Date,y=k*datos_juntos$uv_wind)

layout(c(1,2))
plot(x=datos_juntos$Date[2000:2500],y=datos_juntos$Mean[2000:2500],col="red",type="l")
lines(x=datos_juntos$Date[2000:2500],y=k*datos_juntos$uv_wind[2000:2500])
plot(x=datos_juntos$Date[2000:2500],y=datos_juntos$Dir[2000:2500])

plot(x=datos_juntos_dir$`247.5`$Date[200:500],y=datos_juntos_dir$`247.5`$Mean[200:500],col="red",type="l")
lines(x=datos_juntos_dir$`247.5`$Date[200:500],y=datos_juntos_dir$`247.5`$uv_wind[200:500],col="grey")
lines(x=datos_juntos_dir$`247.5`$Date[200:500],y=k*datos_juntos_dir$`247.5`$uv_wind[200:500],col="black")

#cut(datos_juntos_dir[[1]]$uv_dwi, breaks = c(0,seq(11.5,349.5,22.5),360), labels = c(as.numeric(names(datos_juntos_dir)),0)) 