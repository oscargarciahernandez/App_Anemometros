library(here)
source(here::here('NUEVO/Libraries.R'))
library(caret)
library(dplyr)
library(ggplot2)
library(glue)
library(ModelMetrics)
#library(OpenMPController) # for Kaggle backend
library(readr)
library(vtreat)
library(xgboost)
library(DescTools)

# IMPORTAR DATA -----------------------------------------------------------
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

# TIME SERIES CROOSS VALIDATION -------------------------------------------
#AQUI UN TIO QUE EXPLICA MUY BIEN LO QUE HACE LO DE TIMESLICES
#https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

# DE AQUI SACO LA MANERA DE HACER LOS TIMESLICES Y LOS MODELOS QUE VOY A PRBAR A CONTINUACION
#https://rpubs.com/crossxwill/time-series-cv


# TIME SERIES CROOSS VALIDATION -------------------------------------------
#AQUI UN TIO QUE EXPLICA MUY BIEN LO QUE HACE LO DE TIMESLICES
#https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

# DE AQUI SACO LA MANERA DE HACER LOS TIMESLICES Y LOS MODELOS QUE VOY A PRBAR A CONTINUACION
#https://rpubs.com/crossxwill/time-series-cv

#HEMOS PUESTO DO PARALLEL A FALSE, PORQUE PASAN COSAS RARAS 
# ES COMO SI SE RALLARA Y NO HCICIERA LOS MODELOS
library(doParallel)
registerDoParallel(cores=10)

HORIZONTE<- 240
INITIAL_WINDOW<- (((nrow(DATA_TRAIN)/100)*95) %>% round(0))
FIXWINDOW<- TRUE


myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = INITIAL_WINDOW,
                              horizon = HORIZONTE,
                              fixedWindow = FIXWINDOW,
                              allowParallel = FALSE)

#VEMOS LAS CARACTERÍSTICAS DE LA CROSS-VALIDATIOON
VECTOR_TIMESLICES<-createTimeSlices(1:nrow(DATA_TRAIN), 
                 initialWindow = INITIAL_WINDOW,
                 horizon = HORIZONTE,
                 fixedWindow = FIXWINDOW)

VECTOR_TIMESLICES$train %>% length
VECTOR_TIMESLICES$train %>% sapply(length)

VECTOR_TIMESLICES$test %>% length
VECTOR_TIMESLICES$test %>% sapply(length)


#SELECIONAMOS LOS INPUTS DE LOS MODELOS
INPUT_VARIABLES<- DATA_TRAIN %>% colnames() %>% .[str_detect(., 'ERAWS|ERAWD')]
input_x<- DATA_TRAIN[,INPUT_VARIABLES]
input_y<- DATA_TRAIN[,"WS_N"]$WS_N

#CUANTOS MODELOS SE VAN PROBAR...
tuneLength.num<- 1

glmnet.mod <- train(x = input_x,
                    y = input_y,
                    method = "glmnet",
                    family = "gaussian",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num)

pois.mod <- train(x = input_x,
                  y = input_y,
                  method = "glmnet",
                  family = "poisson",
                  trControl = myTimeControl,
                  tuneLength=tuneLength.num)

lm.mod <- train(x = input_x,
                y = input_y,
                method = "lm",
                trControl = myTimeControl,
                tuneLength=tuneLength.num)

earth.mod <- train(x = input_x,
                   y = input_y,
                   method = "earth",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

earth.pois.mod <- train(x = input_x,
                        y = input_y,
                        method = "earth",
                        glm=list(family=poisson),
                        trControl = myTimeControl,
                        tuneLength=tuneLength.num)

gam.mod <- train(x = input_x,
                 y = input_y,
                 method = "gam",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num)

rpart.mod <- train(x = input_x,
                   y = input_y,
                   method = "rpart",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

party.mod <- train(x = input_x,
                   y = input_y,
                   method = "ctree",
                   trControl = myTimeControl,
                   tuneLength=tuneLength.num)

rf.mod <- train(x = input_x,
                y = input_y,
                method = "rf",
                trControl = myTimeControl,
                tuneLength=tuneLength.num)


gbm.mod <- train(x = input_x,
                 y = input_y,
                 method = "gbm",
                 distribution="poisson",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

xgb.mod <- train(x = input_x,
                 y = input_y,
                 method = "xgbTree",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

MODEL_LISTS<- list(glmnet = glmnet.mod,
                   glmnet.pois = pois.mod,
                   lm = lm.mod,
                   earth=earth.mod,
                   earth.pois=earth.pois.mod,
                   #gbm=gbm.mod,
                   gam=gam.mod,
                   rf=rf.mod,
                   rpart=rpart.mod,
                   party=party.mod,
                   xgboost= xgb.mod)
PATH_MODELOS<- here::here('XGBoost_Modelos/0B38DAE79059/')
if(!dir.exists(PATH_MODELOS)){dir.create(PATH_MODELOS)}

saveRDS(MODEL_LISTS, paste0(PATH_MODELOS, 'LISTA_MODELOS_WS.RDS'))

resamps <- resamples(MODEL_LISTS)
resamps

ss <- summary(resamps)

knitr::kable(ss[[3]]$Rsquared)
knitr::kable(ss[[3]]$RMSE)
knitr::kable(ss[[3]]$MAE)



library(lattice)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")
dotplot(resamps, metric = "MAE")

dotplot(resamps, metric = "Rsquared")

