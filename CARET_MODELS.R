####################  LEEME ##############
#LOS PARÁMETROS QUE PRIMERO DEBEMOS AJUSTAR SON LOS VALORES DE LAS 
# MOVING AVERAGES Y EL DESFASE. 
# TODOS LOS VALORES LOS PONDREMOS EN HORAS. 
# TODAS LAS GRÁFICAS Y MODELOS SE ESTÁN GUARDANDO AUTOMÁTICAMENTE EN 
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/DN_AP --> MODELOS PARA PREDECIR APORTACION CON DIFERENCIA DE NIVEL
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/WRF_DN --> MODELOS PARA PREDECIR DIFERENCIA DE NIVEL

# LA PREDICCIÓN DE APORTACION FINAL SERÁ UNA COMBINACIÓN DE AMBOS MODELOS, ES DECIR
# CON LLUVIA WRF PREDECIMOS DIFERENCIA NIVEL Y CON LA DIFERENCIA NIVEL CALCULAMOS APORTACION
# ESTAS GRÁFICAS COMPARATIVAS SE GUARDARÁN EN 
# ~/MB/DATA/PARQUES/BELESAR/MODELOS/WRF_AP --> GRÁFICAS FINALES DE PREDICCIÓN DE APORTACIÓN


####################  PARÁMETROS PARA MODIFICAR LAS VARIABLES ##############

#LIMPIEZA DE ENVIRONMENT ANTES DE EMPEZAR... CUIDANDO LA RAM
rm(list = setdiff(ls(), lsf.str()))

####### MOVING AVERAGES
#ESTOS PARÁMETROS SIRVEN PARA "ELIMINAR RUIDO"... UN MOVING AVERAGE MAYOR IMPLICA 
# SEÑALES MAS "SUAVES" CON MENOS RUIDO, MÁS FÁCIL DE PREDECIR, PERO PROBABLEMENTE MÁS 
# ALEJADA DE LA REALIDAD. 

#### MOVING AVERAGE SOBRE APORTACION
SMA_APORTACION<- 12

#### MOVING AVERAGE SOBRE DIFERENCIA DE NIVEL 
SMA_DIFF_NIVEL<- 12

#### MOVING AVERAGE SOBRE LLUVIA WRF
SMA_LLUVIA_WRF<- 48


#### MOVING AVERAGE SOBRE LLUVIA OBS
SMA_LLUVIA_OBS<- 48

####### LAG SOBRE LAS VARIABLES 
#ESTO ES: CON CUANTO RETRASO VOY A METER LA VARIABLE AL MODELO PREDICTIVO
# EN PRINCIPIO LAS VARIABLES CON LAS VAMOS A JUGAR SON LAS SIGUIENTES...
# LLUVIA PREDICHA POR WRF -X -->X SON HORAS
# DIFERENCIA DEL NIVEL  -X -->X SON HORAS


#### DESFASE LLUVIA WRF (LAG)
LAG_LLUVIA_WRF<- 24

#### DESFASE LLUVIA OBSERVADA (LAG)
LAG_LLUVIA_OBS<- 24


### DESFASE   DIFERENCIA DE NIVEL 
LAG_DIFF_NIVEL<- 24 

####################  PARÁMETROS PARA MODIFICAR LOS MODELOS ##############
# BÁSICAMENTE AQUÍ ELEGIREMOS EL MODELO QUE QUEREMOS EMPLEAR Y CON CUANTO "ESFUERZO"
# INTENTARÁ OBTENER EL MEJOR RESULTADO...

# CADA MODELO TIENE SUS PROPIOS PARÁMETROS (ALGUNOS NO) QUE SE PUEDEN PONER MANUALMENTE.
# SI NO PONEMOS PARÁMETROS EL MODELO SE EJECUTA CON UN PARÁMETRO POR DEFECTO, PERO, 
#PODEMOS DECIRLE QUE PRUEBE DIFERENTES PARÁMETROS... CON "TUNELENGTH"
# SI PONEMOS TUNELENGTH 20, PROBARÁ CON 20 PARÁMETROS DIFERENTES Y DEVOLVERÁ EL MEJOR VALOR


# EN CUANTO A LOS MODELOS. CARET TIENEN 238 ALGORITMOS DE PREDICCIÓN. PERO, HAY ALGUNOS ALGORITMOS
# QUE SON EXCLUSIVOS PARA CLASIFICACIÓN---- NUESTRO MODELO ES UN MODELO REGRESIVO O CONTINUO,
# POR LO TANTO TODOS ESTOS MODELOS NO NOS VALEN. A CONTINUACIÓN PONGO LOS MODELOS QUE A NOSOTROS 
# NOS INTERESAN; 

#REGRESION 
#  ANFIS , blasso , blassoAveraged , bridge , brnn , cubist , DENFIS , enet , FIR.DM , 
#   foba , FS.HGD , GFS.FR.MOGUL , GFS.LT.RS , GFS.THRIFT , glm.nb , HYFIS , icr , krlsPoly , 
#   krlsRadial , lars , lars2 , lasso , leapBackward , leapForward , leapSeq , lm , lmStepAIC ,
#     M5 , M5Rules , neuralnet , nnls , pcr , penalized , ppr , qrf , qrnn , relaxo , ridge ,
#    rlm , rqlasso , rqnc , rvmLinear , rvmPoly , rvmRadial , SBC , spikeslab , superpc , WM


#REGRESION-CLASIFICACION
# "avNNet , bag , bagEarth , bagEarthGCV , bam , bartMachine , bayesglm , blackboost , 
#   BstLm , bstSm , bstTree , cforest , ctree , ctree2 , dnn , earth , elm , evtree , 
#    extraTrees , gam , gamboost , gamLoess , gamSpline , gaussprLinear , gaussprPoly , 
#  gaussprRadial , gbm_h2o , gbm , gcvEarth , glm , glmboost , glmnet_h2o , glmnet , 
# glmStepAIC , kernelpls , kknn , knn , logicBag , logreg , mlp , mlpKerasDecay , 
# mlpKerasDropout , mlpML , mlpSGD , mlpWeightDecay , mlpWeightDecayML , monmlp , 
# msaenet , mxnet , mxnetAdam , nnet , nodeHarvest , null , parRF , partDSA , pcaNNet , 
#  pls , plsRglm , randomGLM , ranger , rbf , rbfDDA , Rborist , rf , rfRules , rpart , 
#  rpart1SE , rpart2 , RRF , RRFglobal , simpls , spls , svmBoundrangeString , svmExpoString , 
# svmLinear , svmLinear2 , svmLinear3 , svmPoly , svmRadial , svmRadialCost , svmRadialSigma , 
#   svmSpectrumString , treebag , widekernelpls , xgbDART , xgbLinear , xgbTree , xyf"

# PÁGINA WEB PARA TENER INFO DE CADA ALGORITMO
# https://topepo.github.io/caret/available-models.html



#IMPORTANTE: EL METODO SIEMPRE ENTRE COMILLAS
METODO<- "rqlasso"

#TUNELENGH: CUIDADO CON ESTO, AUMENTA MUCHO LA NECESIDAD DE COMPUTACIÓN. 
TUNELENGTH<- 1



####################  PARÁMETROS ORDENADOR ##############
#CARET PERMITE LA COMPUTACION EN PARALELO... PARA ELLO TENDREMOS QUE SABER EL NUMERO DE 
# CORES/NÚCLES DE NUESTRO ORDENADOR
# EN LINUX PONEMOS ~$ nproc POR TERMINAL Y NOS DEVUELVE EL NUMERO DE PROCESADORES
# ESTO ES POQUITÍN PELIGROSO PORQUE NO PODEMOS PONER A COMPUTAR LA MÁQUINA EMPLEANDO 
# TODOS SUS NUCLES... RECOMIENDAN PONER 1 MENOS... PERO POR SEGURIDAD SE PUEDAN PONER 
# 2 MENOS DE LOS QUE DISPONE EL ORDENADOR. 


NCORES<- 2

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################




library(caret)
library(doMC)
library(here)
source(here::here('NUEVO/Libraries.R'))

registerDoMC(cores = NCORES)



#IMPORTANDO DATOS 

DATA_FOLDERS<- list.dirs(here::here('NUEVO/Data_calibracion/'), recursive = F)

DATOS_JUNTOS<- DATA_FOLDERS[1] %>% list.files(full.names = T) %>% 
  .[str_detect(., "ERA5")] %>% readRDS()


DATOS_JUNTOS_LISTA<- DATOS_JUNTOS %>% group_split(ERAlon,ERAlat)

#CALCULAMOS DISTANCIAS ENTRE ERA5 Y ANEMOMETRO
DISTANCIA_ERA5_ANEMO<- DATOS_JUNTOS_LISTA %>% sapply(function(x){distm(c(x$ERAlon[1], x$ERAlat[1]), c(x$lon[1],x$lat[1]))})


#SACAMOS EL PUNTO MÁS CERCANO Y TRABAJAMOS DESDE AQUÍ. 
PUNTO_MC<- DATOS_JUNTOS_LISTA[[which.min(DISTANCIA_ERA5_ANEMO)]]  %>%
  mutate(ERA_binDir= cut(ERAWD ,
                         breaks =c(0,seq(22.5,337.5,22.5),360, 361), 
                         labels = c(0,seq(22.5,337.5,22.5),0) %>% as.factor()))
PUNTO_MC$ERA_binDir<- PUNTO_MC$ERA_binDir %>% as.character() %>% as.numeric()
PUNTO_MC$WD_N<- ifelse(PUNTO_MC$WD_N==155, 157.5, PUNTO_MC$WD_N) 

# SEGUNDA PARTE: PREDICCIÓN DE DIFERENCIA DE NIVEL ------------------------

PORCENTAJE_ENTRENAMIENTO<- 0.8
LOGIC_TRAIN<- ifelse(1:nrow(PUNTO_MC)%in%(1:round(nrow(PUNTO_MC)*PORCENTAJE_ENTRENAMIENTO)), TRUE,FALSE)



train_data<- PUNTO_MC[LOGIC_TRAIN, ]
prediccion_data<- PUNTO_MC[!LOGIC_TRAIN, ]


MODELO_1<- tryCatch({train(difnivel_SMA ~ WRF_SMA_lag,
                                 data=train_data,
                                 method=METODO,
                                 tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})





modelo_WRF_DN2<- tryCatch({train(difnivel_SMA ~ WRF_SMA_lag + WRF_SMA,
                                 data=train_data,
                                 method=METODO,
                                 tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})

modelo_WRF_DN3<- tryCatch({ train(difnivel_SMA ~ WRF_SMA_lag * WRF_SMA,
                                  data=train_data,
                                  method=METODO,
                                  tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})


modelo_WRF_DN4<- tryCatch({ train(difnivel_SMA ~ WRF_SMA_lag + DN_SMA_lag,
                                  data=train_data,
                                  method=METODO,
                                  tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})
modelo_WRF_DN5<- tryCatch({  train(difnivel_SMA ~ WRF_SMA_lag  * DN_SMA_lag,
                                   data=train_data,
                                   method=METODO,
                                   tuneLength=TUNELENGTH)}, 
                          error=function(e){
                            cat("Fallo modelo")
                            return("empty")})
modelo_WRF_DN6<- tryCatch({ train(difnivel_SMA ~ WRF_SMA_lag + WRF_SMA * DN_SMA_lag,
                                  data=train_data,
                                  method=METODO,
                                  tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})
modelo_WRF_DN7<- tryCatch({train(difnivel_SMA ~ WRF_SMA_lag * WRF_SMA + DN_SMA_lag,
                                 data=train_data,
                                 method=METODO,
                                 tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})
modelo_WRF_DN8<- tryCatch({train(difnivel_SMA ~ WRF_SMA_lag + WRF_SMA + DN_SMA_lag,
                                 data=train_data,
                                 method=METODO,
                                 tuneLength=TUNELENGTH)}, 
                          error=function(e){cat("Fallo modelo")
                            return("empty")})


#############GUARDAMOS MODELOS 
Nombre_archivo<- paste(METODO,SMA_APORTACION,SMA_DIFF_NIVEL,
                          SMA_LLUVIA_WRF,LAG_DIFF_NIVEL,
                          LAG_LLUVIA_WRF, TUNELENGTH, sep = "_")

DN_AP_path<- here::here('Data/Parques/Belesar/Modelos/DN_AP/')
WRF_DN_path<- here::here('Data/Parques/Belesar/Modelos/WRF_DN/')
WRF_AP_path<- here::here('Data/Parques/Belesar/Modelos/WRF_AP/')

if(!dir.exists(DN_AP_path))dir.create(DN_AP_path)
if(!dir.exists(WRF_DN_path))dir.create(WRF_DN_path)
if(!dir.exists(WRF_AP_path))dir.create(WRF_AP_path)

saveRDS(modelo_DN_AP, file = paste0(DN_AP_path,Nombre_archivo,".RDS"))

saveRDS(modelo_WRF_DN1, file = paste0(WRF_DN_path,Nombre_archivo,"_1.RDS"))
saveRDS(modelo_WRF_DN2, file = paste0(WRF_DN_path,Nombre_archivo,"_2.RDS"))
saveRDS(modelo_WRF_DN3, file = paste0(WRF_DN_path,Nombre_archivo,"_3.RDS"))
saveRDS(modelo_WRF_DN4, file = paste0(WRF_DN_path,Nombre_archivo,"_4.RDS"))
saveRDS(modelo_WRF_DN5, file = paste0(WRF_DN_path,Nombre_archivo,"_5.RDS"))
saveRDS(modelo_WRF_DN6, file = paste0(WRF_DN_path,Nombre_archivo,"_6.RDS"))
saveRDS(modelo_WRF_DN7, file = paste0(WRF_DN_path,Nombre_archivo,"_7.RDS"))
saveRDS(modelo_WRF_DN8, file = paste0(WRF_DN_path,Nombre_archivo,"_8.RDS"))



##############GRAFICAMOS MODELO DIFERENCIA NIVEL A APORTACION 
prediccion_data<- prediccion_data_DN_A
ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$aport, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$aport_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Aportacion [m³/s]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= prediccion_data),
                x=Date), 
            col="red", lty=2)+
  theme_light()
ggsave(paste0(DN_AP_path,Nombre_archivo,".png"), 
       dpi = 200, 
       device = "png")


##############GRAFICAMOS MODELO LLUVIA WRF A DIFERENCIA NIVEL 
PORCENTAJE_ENTRENAMIENTO<- 0.8
LOGIC_TRAIN<- ifelse(1:nrow(Tabla_3)%in%(1:round(nrow(Tabla_3)*PORCENTAJE_ENTRENAMIENTO)), TRUE,FALSE)



train_data<- Tabla_3[LOGIC_TRAIN, ]
prediccion_data<- Tabla_3[!LOGIC_TRAIN, ]


ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$diff_nivel, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$difnivel_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Variacion nivel [msnm]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN1)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN1, newdata= prediccion_data)),
                x=Date), 
            col="red", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN2)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN2, newdata= prediccion_data)),
                x=Date), 
            col="green", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN3)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN3, newdata= prediccion_data)),
                x=Date), 
            col="forestgreen", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN4)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN4, newdata= prediccion_data)),
                x=Date), 
            col="blue", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN5)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN5, newdata= prediccion_data)),
                x=Date), 
            col="cyan", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN6)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN6, newdata= prediccion_data)),
                x=Date), 
            col="gold", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN7)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN7, newdata= prediccion_data)),
                x=Date), 
            col="gold3", lty=2)+
  geom_line(aes(y=ifelse(length(modelo_WRF_DN8)==1, rep(0,nrow(prediccion_data)),
                         predict(modelo_WRF_DN8, newdata= prediccion_data)),
                x=Date), 
            col="gold4", lty=2)+
  theme_light()

ggsave(paste0(WRF_DN_path,Nombre_archivo,".png"), 
       dpi = 200,
       device = "png")



##############GRAFICAMOS MODELO LLUVIA WRF A DIFERENCIA NIVEL PERO LA VERSIÓN CORREGIDA

alfa1<- 0-predict(modelo_WRF_DN1, newdata= prediccion_data)[1] 
alfa2<- 0-predict(modelo_WRF_DN2, newdata= prediccion_data)[1] 
alfa3<- 0-predict(modelo_WRF_DN3, newdata= prediccion_data)[1] 
alfa4<- 0-predict(modelo_WRF_DN4, newdata= prediccion_data)[1] 
alfa5<- 0-predict(modelo_WRF_DN5, newdata= prediccion_data)[1] 
alfa6<- 0-predict(modelo_WRF_DN6, newdata= prediccion_data)[1] 
alfa7<- 0-predict(modelo_WRF_DN7, newdata= prediccion_data)[1] 
alfa8<- 0-predict(modelo_WRF_DN8, newdata= prediccion_data)[1] 

alfa0<- 0-prediccion_data$difnivel_SMA[1]

ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$diff_nivel+alfa0, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$difnivel_SMA+alfa0, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Variacion nivel [msnm]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo_WRF_DN1, newdata= prediccion_data)+alfa1,
                x=Date), 
            col="red", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN2, newdata= prediccion_data)+alfa2,
                x=Date), 
            col="green", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN3, newdata= prediccion_data)+alfa3,
                x=Date), 
            col="forestgreen", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN4, newdata= prediccion_data)+alfa4,
                x=Date), 
            col="blue", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN5, newdata= prediccion_data)+alfa5,
                x=Date), 
            col="cyan", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN6, newdata= prediccion_data)+alfa6,
                x=Date), 
            col="gold", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN7, newdata= prediccion_data)+alfa7,
                x=Date), 
            col="gold3", lty=2)+
  geom_line(aes(y=predict(modelo_WRF_DN8, newdata= prediccion_data)+alfa8,
                x=Date), 
            col="gold4", lty=2)+
  theme_light()
ggsave(paste0(WRF_DN_path,Nombre_archivo,"_corrected.png"), 
       dpi = 200, 
       device = "png")


##############GRAFICAMOS MODELO LLUVIA WRF A APORTACIÓN REMONTANDONOS AL HISTÓRICO
Tabla_4<- Tabla_1
Tabla_4[,c("Vol","Temp", "porcentaje")]<- NULL
Tabla_4<- Tabla_4[complete.cases(Tabla_4),]
Tabla_4$aport_SMA<- SMA(Tabla_4$aport, SMA_APORTACION)
Tabla_4$difnivel_SMA<- SMA(Tabla_4$diff_nivel, SMA_DIFF_NIVEL)
Tabla_4$WRF_SMA<- SMA(Tabla_4$prep_hourly, SMA_LLUVIA_WRF)
Tabla_4$WRF_SMA_lag<- lag(Tabla_4$WRF_SMA, LAG_LLUVIA_WRF)
Tabla_4$DN_SMA_lag<- lag(Tabla_4$difnivel_SMA, LAG_DIFF_NIVEL)

prediccion_data<- Tabla_4[Tabla_4$Date> ymd("2019/01/25"), ]

#AP_prediction<- predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN,newdata = prediccion_data)))


ggplot(data = prediccion_data)+
  geom_line(aes(y=prediccion_data$aport, 
                x=prediccion_data$Date), 
            alpha=0.5)+
  geom_line(aes(y=prediccion_data$aport_SMA, 
                x=prediccion_data$Date), 
            alpha=0.8)+
  ylab("Aportacion [m³/s]")+
  xlab(paste(range(prediccion_data$Date), collapse = "\n"))+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN1, newdata= prediccion_data))),
                x=Date), 
            col="red", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN2, newdata= prediccion_data))),
                x=Date), 
            col="green", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN3, newdata= prediccion_data))),
                x=Date), 
            col="forestgreen", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN4, newdata= prediccion_data))),
                x=Date), 
            col="blue", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN5, newdata= prediccion_data))),
                x=Date), 
            col="cyan", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN6, newdata= prediccion_data))),
                x=Date), 
            col="gold", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN7, newdata= prediccion_data))),
                x=Date), 
            col="gold3", lty=2)+
  geom_line(aes(y=predict(modelo_DN_AP, newdata= data.frame(difnivel_SMA=predict(modelo_WRF_DN8, newdata= prediccion_data))),
                x=Date), 
            col="gold4", lty=2)+
  theme_light()   
ggsave(paste0(WRF_AP_path,"_WRFDN_",Nombre_archivo,
              "_DNAP_",Nombre_archivo,".png"),
       dpi = 200,
       device = "png")

