library(caret)
library(dplyr)
library(ggplot2)
library(glue)
library(ModelMetrics)
#library(OpenMPController) # for Kaggle backend
library(readr)
library(vtreat)
library(xgboost)

' 
PARAMERTROS PARA AJUSTAR 

LO QUE SE HARA SERÁ USAR UN LEARNING RATE ALTO PARA AJUSTAR LOS HYPERPARAMETROS 
LO CUAL ES COMPUTACIONALMENTE COSTOSO Y LUEGO CON LOS HIPERPARAMETROS AJUSTADOS 
USAMOS UN LEARNING RATE MAS BAJO

nrounds: Number of trees, default: 100
max_depth: Maximum tree depth, default: 6
eta: Learning rate, default: 0.3
gamma: Used for tuning of Regularization, default: 0
colsample_bytree: Column sampling, default: 1
min_child_weight: Minimum leaf weight, default: 1
subsample: Row sampling, default: 1
'


# IMPORT DATA -------------------------------------------------------------

DATA_ALL<- here::here('NUEVO/Data_calibracion/0B38DAE79059/ERA5_2019-06-01.RDS') %>% readRDS()

MAX_COR_POINT<- DATA_ALL %>% group_by(ERAlon, ERAlat) %>% group_split() %>% 
  sapply(function(x){
    cor(x$WS_N,x$ERAWS, use = 'complete.obs')
  }) %>% which.max()


DATA_ONE_LOCATION<- DATA_ALL %>% group_by(ERAlon, ERAlat) %>% group_split() %>% .[[MAX_COR_POINT]]

TEST_TRAIN_FACTOR<- 1.1
N_DATOS<-DATA_ONE_LOCATION %>% nrow()
DATA_TRAIN<- DATA_ONE_LOCATION[1:((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)),]
DATA_TEST<- DATA_ONE_LOCATION[((N_DATOS/TEST_TRAIN_FACTOR) %>% round(0)):N_DATOS,]

'
NO VAMOS A DIVIDIR EL DATASET DE ENTRENAMIENTO, MIRAREMOS DIRECTAMENTE NUESTRA 
PRECISION SOBRE EL DATASET DE TEST

DATA_TRAIN_HOLDOUT <- dplyr::sample_frac(DATA_TRAIN, 0.2)
hid <- as.numeric(rownames(DATA_TRAIN_HOLDOUT))
DATA_TRAIN <- DATA_TRAIN[-hid, ]
'
# XGBOOST CON PARAMETROS PREDETERMINADOS ----------------------------------

input_x<- DATA_TRAIN[,c("ERAWS","ERAWD" )]
input_y<- DATA_TRAIN[,"WS_N"]$WS_N

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)

linear_base <- lm(paste0("WS_N ~ ",
                         paste(c("ERAWS","ERAWD" ), collapse = ' + ')),
                  data = DATA_TRAIN)



# AFINAMOS EL XGBOOST -----------------------------------------------------
VARIABLES_AJUSTE<- list(c('ERAWS', 'ERAWD'))
for(i in 1:length(VARIABLES_AJUSTE)){
  
  
  VARIABLES_MODELO<- VARIABLES_AJUSTE[[i]]
  
  tryCatch({
    
    PATH_MODELOS<- here::here('/XGBoost_Modelos/')
    if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS, recursive = TRUE)}
    
    NOMBRE_BASE<- paste(VARIABLES_MODELO, collapse = '_')
    
    if(file.exists(paste0(PATH_MODELOS, NOMBRE_BASE,'xgbTune.RDS'))){
      print(paste('YA EXISTE', NOMBRE_BASE))
    }else{
      input_x<- DATA_TRAIN[,VARIABLES_MODELO]
      input_y<- DATA_TRAIN[,"WS_N"]$WS_N
      
      nroundsmin<- 50
      nroundsmax<- 1000
      
      
      etamin<- 0.05
      etamax<- 0.5
      
      
      while (TRUE) {
        k<- 0
        VECTOR_ETA<- seq(from = etamin, to = etamax, length.out = 10) %>% round(3)
        VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
        tune_grid <- expand.grid(
          nrounds = VECTOR_NROUNDS,
          eta = VECTOR_ETA,
          max_depth = c(2, 3, 4, 5, 6),
          gamma = 0,
          colsample_bytree = 1,
          min_child_weight = 1,
          subsample = 1
        )
        
        tune_control <- caret::trainControl(
          method = "cv", # cross-validation
          number = 3, # with n folds 
          #index = createFolds(tr_treated$Id_clean), # fix the folds
          verboseIter = FALSE, # no training log
          allowParallel = TRUE # FALSE for reproducible results 
        )
        
        xgb_tune <- caret::train(
          x = input_x,
          y = input_y,
          trControl = tune_control,
          tuneGrid = tune_grid,
          method = "xgbTree",
          verbose = TRUE
        )
        
        # helper function for the plots
        tuneplot <- function(x, probs = .90) {
          ggplot(x) +
            coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
            theme_bw()
        }
        
        #tuneplot(xgb_tune)
        
        
        if(xgb_tune$bestTune$nrounds==nroundsmin){
          nroundsmin= nroundsmin - nroundsmin/1.5
          nroundsmax= nroundsmax - nroundsmax/1.5
          k<- 1
        }
        if(xgb_tune$bestTune$nrounds==nroundsmax){
          nroundsmin= nroundsmin + nroundsmin/1.5
          nroundsmax= nroundsmax + nroundsmax/1.5
          k<- 1
        }
        if(xgb_tune$bestTune$eta==etamin){
          etamin= etamin - etamin/1.5
          etamax= etamax - etamax/1.5
          k<- 1
        }
        if(xgb_tune$bestTune$eta==etamax){
          etamin= etamin + etamin/1.5
          etamax= etamax + etamax/1.5
          k<- 1
        }
        
        if(k==0){
          print(xgb_tune$bestTune)
          break
        }
        
        
      }
      
      
      # AJUSTAMOS DEPTH y CHILD WEIGH ---------------------------------------------------------
      
      nroundsmin<- 50
      nroundsmax<- 1000
      
      mchildweightmin<- 1
      mchildweightmax<- 10
      
      
      while (TRUE) {
        k<- 0
        VECTOR_MCW<- seq(from = mchildweightmin, to = mchildweightmax, length.out = 10) %>% round()
        VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
        
        tune_grid2 <- expand.grid(
          nrounds = VECTOR_NROUNDS,
          eta = xgb_tune$bestTune$eta,
          max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                             c(xgb_tune$bestTune$max_depth:4),
                             xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
          gamma = 0,
          colsample_bytree = 1,
          min_child_weight = VECTOR_MCW,
          subsample = 1
        )
        
        xgb_tune2 <- caret::train(
          x = input_x,
          y = input_y,
          trControl = tune_control,
          tuneGrid = tune_grid2,
          method = "xgbTree",
          verbose = TRUE
        )
        
        #tuneplot(xgb_tune2)
        xgb_tune2$bestTune
        
        
        if(xgb_tune2$bestTune$nrounds==nroundsmin){
          nroundsmin= nroundsmin - nroundsmin/1.5
          nroundsmax= nroundsmax - nroundsmax/1.5
          k<- 1
        }
        if(xgb_tune2$bestTune$nrounds==nroundsmax){
          nroundsmin= nroundsmin + nroundsmin/1.5
          nroundsmax= nroundsmax + nroundsmax/1.5
          k<- 1
        }
        if(xgb_tune2$bestTune$min_child_weight==mchildweightmin){
          mchildweightmin= mchildweightmin - mchildweightmin/1.5
          mchildweightmax= mchildweightmax - mchildweightmax/1.5
          k<- 1
        }
        if(xgb_tune2$bestTune$min_child_weight==mchildweightmax){
          mchildweightmin= mchildweightmin + mchildweightmin/1.5
          mchildweightmax= mchildweightmax + mchildweightmax/1.5
          k<- 1
        }
        
        if(k==0){
          print(xgb_tune2$bestTune)
          break
        }
        
        
      }
      
      
      
      # AJUSTAMOS COLUMN AND ROW SAMPLING ---------------------------------------
      
      nroundsmin<- 50
      nroundsmax<- 1000
      
      colsamplemin<- 0.1
      colsamplemax<- 1
      
      subsamplemin<- 0.1
      subsamplemax<- 1
      
      
      while (TRUE) {
        k<- 0
        VECTOR_COLSAMPLE<- seq(from = colsamplemin, to = colsamplemax, length.out = 7) %>% round(3)
        VECTOR_SUBSAMPLE<- seq(from = subsamplemin, to= subsamplemax , length.out = 7) %>% round(3)
        VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 5) %>% round()
        
        
        tune_grid3 <- expand.grid(
          nrounds = VECTOR_NROUNDS,
          eta = xgb_tune$bestTune$eta,
          max_depth = xgb_tune2$bestTune$max_depth,
          gamma = 0,
          colsample_bytree = VECTOR_COLSAMPLE,
          min_child_weight = xgb_tune2$bestTune$min_child_weight,
          subsample = VECTOR_SUBSAMPLE
        )
        
        xgb_tune3 <- caret::train(
          x = input_x,
          y = input_y,
          trControl = tune_control,
          tuneGrid = tune_grid3,
          method = "xgbTree",
          verbose = TRUE
        )
        
        ##tuneplot(xgb_tune3, probs = .95)
        xgb_tune3$bestTune
        
        if(xgb_tune3$bestTune$colsample_bytree ==colsamplemin){
          colsamplemin= colsamplemin - colsamplemin/1.5
          colsamplemax= colsamplemax - colsamplemax/1.5
          k<- 1
        }
        if(xgb_tune3$bestTune$colsample_bytree==colsamplemax){
          colsamplemin= colsamplemin + colsamplemin/1.5
          colsamplemax= colsamplemax + colsamplemax/1.5
          k<- 1
        }
        if(xgb_tune3$bestTune$subsample==subsamplemin){
          subsamplemin= subsamplemin - subsamplemin/1.5
          subsamplemax= subsamplemax - subsamplemax/1.5
          k<- 1
        }
        if(xgb_tune3$bestTune$subsample==subsamplemax){
          subsamplemin= subsamplemin + subsamplemin/1.5
          subsamplemax= subsamplemax + subsamplemax/1.5
          k<- 1
        }
        
        if(k==0){
          print(xgb_tune3$bestTune)
          break
        }else{
          print('CAMBIANDO PARAMETROS')
        }
        
        
      }
      
      
      
      
      
      # AJUSTAMOS GAMMA ---------------------------------------------------------
      nroundsmin<- 50
      nroundsmax<- 1000
      
      gammamin<- 0.001
      gammamax<- 1
      
      while (TRUE) {
        k<- 0
        VECTOR_GAMMA<-  seq(from = gammamin, to = gammamax, length.out = 10) %>% round(3)
        VECTOR_NROUNDS<- seq(from = nroundsmin, to = nroundsmax, length.out = 10) %>% round()
        
        
        tune_grid4 <- expand.grid(
          nrounds = VECTOR_NROUNDS,
          eta = xgb_tune$bestTune$eta,
          max_depth = xgb_tune2$bestTune$max_depth,
          gamma = VECTOR_GAMMA,
          colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
          min_child_weight = xgb_tune2$bestTune$min_child_weight,
          subsample = xgb_tune3$bestTune$subsample
        )
        
        xgb_tune4 <- caret::train(
          x = input_x,
          y = input_y,
          trControl = tune_control,
          tuneGrid = tune_grid4,
          method = "xgbTree",
          verbose = TRUE
        )
        
        #tuneplot(xgb_tune4)
        
        if(xgb_tune4$bestTune$gamma==gammamin){
          subsamplemin= subsamplemin - subsamplemin/1.5
          subsamplemax= subsamplemax - subsamplemax/1.5
          k<- 1
        }
        if(xgb_tune4$bestTune$gamma==gammamax){
          subsamplemin= subsamplemin + subsamplemin/1.5
          subsamplemax= subsamplemax + subsamplemax/1.5
          k<- 1
        }
        
        if(k==0){
          print(xgb_tune4$bestTune)
          break
        }
        
        
      }
      
      
      
      
      
      
      
      # REDUCIMOS EL LEARNING RATE ----------------------------------------------
      
      tune_grid5 <- expand.grid(
        nrounds = seq(from = 100, to = 10000, by = 100),
        eta = seq(0.01, xgb_tune$bestTune$eta, length.out = 5),
        max_depth = xgb_tune2$bestTune$max_depth,
        gamma = xgb_tune4$bestTune$gamma,
        colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
        min_child_weight = xgb_tune2$bestTune$min_child_weight,
        subsample = xgb_tune3$bestTune$subsample
      )
      
      xgb_tune5 <- caret::train(
        x = input_x,
        y = input_y,
        trControl = tune_control,
        tuneGrid = tune_grid5,
        method = "xgbTree",
        verbose = TRUE
      )
      
      #tuneplot(xgb_tune5)
      
      
      
      
      
      # fitting the model -------------------------------------------------------
      
      final_grid <- expand.grid(
        nrounds = xgb_tune5$bestTune$nrounds,
        eta = xgb_tune5$bestTune$eta,
        max_depth = xgb_tune5$bestTune$max_depth,
        gamma = xgb_tune5$bestTune$gamma,
        colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
        min_child_weight = xgb_tune5$bestTune$min_child_weight,
        subsample = xgb_tune5$bestTune$subsample
      )
      
      xgb_model <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = final_grid,
        method = "xgbTree",
        verbose = TRUE
      )
      
      saveRDS(xgb_model, paste0(PATH_MODELOS, NOMBRE_BASE,'_xgbTune.RDS'))
    }
    
  }, error= function(e){
    print('FALLO AJUSTANTO')
  })
}

# PONEMOS A PRUEBALOS MODELOS ---------------------------------------------
holdout_x <- select(DATA_TEST, -WS_N)
holdout_y <- DATA_TEST$WS_N


library(forecast)
TABLA_ACCURACY<- data.frame(matrix(ncol =7))
TABLA_ACCURACY[1,]<- c('Original',
                       accuracy(DATA_TEST$ERAWS, DATA_TEST$WS_N),
                       cor(DATA_TEST$ERAWS, 
                           DATA_TEST$WS_N, use = 'complete.obs'))

TABLA_ACCURACY[2,]<- c('Original_corregidoz1',
                       accuracy(DATA_TEST$ERAWS_CORRECTED, DATA_TEST$WS_N),
                       cor(DATA_TEST$ERAWS, 
                           DATA_TEST$WS_N, use = 'complete.obs'))
TABLA_ACCURACY[3,]<- c('Original_corregidoz2',
                       accuracy(DATA_TEST$ERAWS_CORRECTED_ZO1, DATA_TEST$WS_N),
                       cor(DATA_TEST$ERAWS, 
                           DATA_TEST$WS_N, use = 'complete.obs'))

TABLA_ACCURACY[4,]<- c('XGBoost_base',
                       accuracy(predict(xgb_base, newdata = holdout_x), 
                                DATA_TEST$WS_N),
                       cor(predict(xgb_base, newdata = holdout_x),
                           DATA_TEST$WS_N, use = 'complete.obs'))


TABLA_ACCURACY[5,]<- c('Linear_base',
                       accuracy(predict(linear_base, newdata = holdout_x), 
                                DATA_TEST$WS_N),
                       cor(predict(linear_base, newdata = holdout_x), 
                           DATA_TEST$WS_N, use = 'complete.obs'))


TABLA_ACCURACY[6,]<- c('XGBoost_tune',
                       accuracy(predict(xgb_model, newdata = holdout_x), 
                                DATA_TEST$WS_N),
                       cor(predict(xgb_model, newdata = holdout_x), 
                           DATA_TEST$WS_N, use = 'complete.obs'))


colnames(TABLA_ACCURACY)<- c('NAME', 'ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'CORR')


# REPRESENTAMOS EL DIAGRAMA DE TAYLOR -------------------------------------
library(plotrix)
library(viridis)
taylor.diagram(as.vector(DATA_TEST$WS_N), 
               as.vector(DATA_TEST$ERAWS), col= viridis(6)[1])

taylor.diagram(as.vector(DATA_TEST$WS_N), 
               as.vector(DATA_TEST$ERAWS_CORRECTED), 
               add=TRUE, col= viridis(6)[2])
taylor.diagram(as.vector(DATA_TEST$WS_N), 
               as.vector(DATA_TEST$ERAWS_CORRECTED_ZO1),
               add = TRUE, col= viridis(6)[3])
taylor.diagram(predict(xgb_model,
                       newdata = holdout_x), 
               DATA_TEST$WS_N,
               add = TRUE, col= viridis(6)[4])
taylor.diagram(predict(xgb_base,
                       newdata = holdout_x), 
               DATA_TEST$WS_N,
               add = TRUE, col= viridis(6)[5])
taylor.diagram(predict(linear_base,
                       newdata = holdout_x), 
               DATA_TEST$WS_N,
               add = TRUE, col= viridis(6)[6])
lpos<-2*sd(DATA_TEST$WS_N)
# add a legend
legend(lpos+ 0.1,lpos+0.2,legend=c('Sin calibracion', 'XGBoost afinado',
                               'XGBoost sin afinar', 'Modelo linear'),
       pch=19,col=viridis(4)[1:4])


# PLOT REPRESENTATIVO -----------------------------------------------------
DATOS_PLOT<- DATA_TEST[1:200,]

ggplot(data= DATOS_PLOT)+ 
  geom_line(aes(x= Date, y = ERAWS), colour=viridis(6)[1] )+
  geom_line(aes(x= Date, y = ERAWS_CORRECTED),colour=viridis(6)[2])+
  geom_line(aes(x= Date, y = ERAWS_CORRECTED_ZO1),colour=viridis(6)[3])+
  geom_line(aes(x= Date, y = predict(xgb_model,
                                     newdata = DATOS_PLOT)),colour=viridis(6)[4])+
  geom_line(aes(x= Date, y = predict(xgb_base,
                                      newdata = DATOS_PLOT)),colour=viridis(6)[5])+
  geom_line(aes(x= Date, y = predict(linear_base,
                                     newdata = DATOS_PLOT)),colour=viridis(6)[6])+
  geom_line(aes(x= Date, y= WS_N), colour= 'red', alpha= 0.5, size= 1.2)+
  theme_light()

