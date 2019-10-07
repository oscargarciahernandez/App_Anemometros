library(here)
source(here::here('/NUEVO/Libraries.R'), encoding = 'UTF-8', echo=F)

#Modelado curva de potencia----

#Ajuste polinomico P ~ a*U^3 + b*U^2 + c*U + d

sacar_curva_P=function(Ustart,Unom,Uoff,Pnom) {
  #Ustart<U<Unom:
    #P ~ a*U^3 + b*U^2 + c*U + d
    #P' ~ 3*a*U^2 + 2*b*U + C
  
  #Sistema de ecuaciones para Ustart<U<Unom:
    #P(U=Ustart) = 0
    #P(U=Unom) = Pnom
    #P'(U=Ustart) = 0
    #P'(U=Unom) = 0
  
  require(data.table)
  A=matrix(data = c(Ustart^3,Ustart^2,Ustart,1,
                    Unom^3,Unom^2,Unom,1,
                    3*Ustart^2,2*Ustart,1,0,
                    3*Unom^2,2*Unom,1,0)
           ,ncol=4,byrow = T)
  
  Y=matrix(data = c(0,Pnom,0,0),
           nrow = 4)
  
  coeffs=solve(A,Y)
  coeffs=as.vector(coeffs)  #solve nos lo da como matriz 4 x 1, ponemos como vector 1 x 4
  names(coeffs)=c('a','b','c','d')
  
  curva_P=data.table(U=seq(from=0,to=Uoff,by=0.1))
  curva_P[U<=Unom,P := coeffs['a']*U^3 + coeffs['b']*U^2 + coeffs['c']*U + coeffs['d']]
  curva_P[U>Unom,P := Pnom]
  curva_P[U<Ustart,P := 0]
  return(curva_P)
}

curva_Bornay13=sacar_curva_P(Ustart = 3,Unom = 12,Uoff = 30,Pnom = 1500) %>% setDT

ggplot()+
  geom_line(aes(x=curva_Bornay13$U,y=curva_Bornay13$P))
#Cargar datos_juntos----
datos_juntos=readRDS(here::here('/NUEVO/Data_calibracion/datos_juntos_leave_one_out.rds'))

calcular_energia=function(datos,curva_P){}

#Suponiendo que la curva de potencia esta en vatios y que los datos son horarios...
variable_U='Mean'
resolucion_U=mean(diff(curva_Bornay13$U))
datos_juntos[,diff_Date := as.numeric(c(diff(Date),as.difftime(tim = 1,units = 'hours')))]

for (i in 1:nrow(curva_Bornay13)) {
  #Energia en kwh
  a=datos_juntos[(curva_Bornay13$U[i]-resolucion_U/2)<get(variable_U) &
                   get(variable_U)<(curva_Bornay13$U[i]+resolucion_U/2),
                 diff_Date*curva_Bornay13$P[i]/3600/1000] %>% sum
  curva_Bornay13[i,E_kwh := a]
}

ggplot()+
  geom_line(aes(x=curva_Bornay13$U,y=curva_Bornay13$P),size=0.4)+
  geom_histogram(data=datos_juntos,
                 aes(x=Mean),
                 binwidth = 0.1,
                 alpha=0.2,
                 fill="blue",
                 col="blue")

#Weibull a partir de datos_juntos----
library(MASS)
fit = fitdistr(datos_juntos[Mean > 0,Mean],"weibull") #No acepta ceros
k<-coef(fit)['shape']
c<-coef(fit)['scale']

y=dweibull(x = seq(0,max(datos_juntos[Mean > 0,Mean]),by=0.1),shape = k,scale = c)

summary(datos_juntos$Mean)
histo=datos_juntos[Mean > 0,Mean] %>% hist(.,seq(0, max(.), by=0.1))

#Manera de calcular el coeficiente para escalar la curva de weibull
shape_factor=sum(histo$counts)*max(histo$breaks)/length(histo$breaks)

ggplot()+
  geom_histogram(mapping = aes(x=Mean),data = datos_juntos[Mean>0,],binwidth = 0.1,alpha=0.4,fill="blue",col="blue")+
  geom_line(aes(x=x,y=y*shape_factor))



