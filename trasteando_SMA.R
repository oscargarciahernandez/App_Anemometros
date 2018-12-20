library(TTR)

corr_1<- vector()
corr_2<- vector()
for (i in 1:4) {
  a<- Datos_calibracion_uni[[i]]$Mean
  b<- Datos_calibracion_uni[[i]]$wind_abs
  c<- SMA(a,n=21)
  corr_1[i]<-cor(a,b)
  cor(c,b)
  
  z<-cbind(c,b)
  z<- z[complete.cases(z),]
  
  corr_2[i]<- cor(z[,1],z[,2])
  
  
}

correlacion<- cbind(corr_1,corr_2)
correlacion
a<- Datos_calibracion_uni[[i]]$Mean
b<- Datos_calibracion_uni[[i]]$wind_abs
c<- SMA(a,n=3)
cor(a,b)
cor(c,b)

z<-cbind(c,b)
z<- z[complete.cases(z),]

corr<- cor(z[,1],z[,2])

plot(a[1:400], type = "l", ylim = c(0,12))
lines(b[1:400], col="red")

plot(a[1:400], type = "l", ylim = c(0,5))
lines(SMA(a,n=20)[1:400], col="red")


plot(b[1:400], type = "l", ylim = c(0,12))
lines(SMA(a,n=10)[1:400]*2, col="red")