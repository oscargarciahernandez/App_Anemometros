library(RSelenium)
library(here)
library(stringr)
library(lubridate)
phoneid<-640689911849


# Función_Sensors_ID's ----------------------------------------------------


#Función para obtener todos los id's de los sensores
#linkeados a nuestro telefono
Get_sensor_ID<- function(phoneid){
  
  
  url<- paste0("https://measurements.mobile-alerts.eu/Home/SensorsOverview?phoneid=",phoneid)
  rD<- rsDriver(browser = "firefox",verbose = FALSE) 
  
  remDr<- rD$client      
  remDr$navigate(url) 
  
  sensor_elem<-unlist(lapply(remDr$findElements(using = 'class name',
                                                value ="sensor-component"),
                             function(x) x$getElementText()))
  sensor_elem<- sensor_elem[str_detect(sensor_elem,"ID")]
  sensor_elem<- str_remove(sensor_elem,"ID\n")
  
  return(sensor_elem)
  remDr$close()
  
}

sensor_ids<- Get_sensor_ID(phoneid)

##Obtenemos Id's de los anemometros
anemo_ID<- sensor_ids[str_detect(str_sub(sensor_ids,1,2), "0B")]



# Función Get_Data --------------------------------------------------------


Get_sensor_Data<- function(sensorID){
  
  url=paste0("https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=",sensorID,"&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts")
  rD<- rsDriver(browser = "firefox",verbose = FALSE) 
  remDr<- rD$client      
  remDr$navigate(url) 
 
  
  #♣ Introducimos fecha en los cajetines y refresh¡ 
  fechainicio="11/08/2018 6:57 PM"
  fechafinal="11/15/2018 6:57 PM"   
  cajatexto_fechainicio=remDr$findElement(using = 'css selector', value = "#from")  
  cajatexto_fechafinal=remDr$findElement(using = 'css selector', value = "#to")    
  cajatexto_fechainicio$clearElement()  
  cajatexto_fechafinal$clearElement()  
  cajatexto_fechainicio$sendKeysToElement(list(fechainicio))  
  cajatexto_fechafinal$sendKeysToElement(list(fechafinal))   
  
 
  
  
  boton_refrescar<- remDr$findElement(using = 'css selector',
                                    value = "button.btn:nth-child(9)")$clickElement()
  
  
  
  #Buscamos el boton el boton de 250 elements por página y pulsamos
  boton_page_size<- remDr$findElement(using = 'css selector',
                                    value = "label.btn:nth-child(3)")$clickElement()
  
  
  #Buscamos el boton refresh y pulsamos
  boton_ref_pagesize<- remDr$findElement(using = 'css selector',
                                         value = "#pagesizebutton")$clickElement()
  
  
  
  
  
  
  
  
  tabla<- remDr$findElement(using = 'css selector', 
                          value = ".table > tbody:nth-child(2)")$getElementText() #
  
 
 #Buscamos última página buscando el boton de Skip-to-last
  #De esta manera podemos saber cuantas páginas 
  #habrá que recorrer con el bucle. 
  
  #Este if es capaz de diferenciar cuantas páginas hay
  #independientemente del número de pagina. 
  
  text_pag<-unlist(remDr$findElement(using = "css selector", 
                                     value=".pagination")$getElementText())
  if(str_detect(text_pag, pattern = "»»")){
    url_last<- unlist(remDr$findElement(using = "css selector", value=".PagedList-skipToLast > a:nth-child(1)")$getElementAttribute('href'))
    numero_pags<- str_remove(str_extract(url_last,
                                         pattern = "page=[:digit:]+"),
                             "page=")
  }else{
    
    numero_pags<- max(as.numeric(unlist(str_extract_all(text_pag,pattern = "[:digit:]+"))))
  }
 
  numero_pags

   
  
  
  split_newline<- str_split(tabla[[1]],pattern = "\n")      
  split_newline_matrix<- t(rbind(unlist(split_newline)))    
  lista_datos<-str_split(split_newline_matrix,pattern = " ") 
  matriz_datos<-matrix(unlist(lista_datos),ncol = 8,byrow = TRUE)      
  dataframe_datos<-as.data.frame(matriz_datos)                         
  dataframe_datos[,1]<-mdy_hms(paste(dataframe_datos[,1],dataframe_datos[,2],dataframe_datos[,3],sep = " "))  
  dataframe_datos<-dataframe_datos[,c(1,4,6,8)]                
  dataframe_datos[,4]<-as.character(dataframe_datos[,4])
  for (i in 1:length(dataframe_datos[,4])) {
    yy<-dataframe_datos[,4][i]
    dataframe_datos[i,4]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
                                                         ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                                                                           ifelse(yy=="East", 90, 
                                                                                                  ifelse(yy=="East-southeast", 110,
                                                                                                         ifelse(yy=="Southeast", 135,
                                                                                                                ifelse(yy=="South-Southeast", 155, 
                                                                                                                       ifelse(yy=="South", 180, 
                                                                                                                              ifelse(yy=="South-southwest", 200, 
                                                                                                                                     ifelse(yy=="Southwest", 225,
                                                                                                                                            ifelse(yy=="West-southwest", 245, 
                                                                                                                                                   ifelse(yy=="West", 270, 
                                                                                                                                                          ifelse(yy=="West-northwest",290,
                                                                                                                                                                 ifelse(yy=="Northwest", 315,
                                                                                                                                                                        ifelse(yy=="North-northwest", 335, 
                                                                                                                                                                               ifelse(yy=='-31',NA)))))))))))))))))
  }
  colnames(dataframe_datos)<- c("Date","Mean","Max","Dir_deg")   
  
  return(dataframe_datos)
  
}