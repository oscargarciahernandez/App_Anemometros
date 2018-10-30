library(here)
library(rlist)
library(lubridate)

datos=list.load(here::here("data/Datos_anemometros.rdata"))
anem1=datos$anem1

anem1$Mean=as.numeric(as.character(anem1$Mean))
anem1$date_string_hour=dmy_hms(as.character(anem1$date_string_hour))

nombres_nuevos=colnames(anem1)
nombres_nuevos[3]="ws"
nombres_nuevos[6]="wd"
colnames(anem1)=nombres_nuevos

windRose(anem1,int = 0.5,angle = 22.5,
         breaks = 8,paddle = F, annotate = F,
         key.position = "right", statistic = "prop.mean")