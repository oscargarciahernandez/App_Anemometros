library(here)
library(rlist)

datos=list.load(here::here("data/Datos_anemometros.rdata"))
anem1=datos$anem1
anem1$Mean=as.numeric(as.character(anem1$Mean))
