library(RSelenium)
library(here)
library(stringr)
library(lubridate)
#Dejo link tutorial de este paquete:
#https://www.computerworld.com/article/2971265/application-development/how-to-drive-a-web-browser-with-r-and-rselenium.html
#Para ir probando pongo por ahora solo el link del Anem Hexagono. Cuando se vea que funciona habria k generalizarlo para poder usarlo en cualquier sensor
url="https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
#directorio_descarga=paste(here::here(),"/Objetivo_cvs",sep = "")
#directorio_perfil=paste(here::here(),"/Objetivo_cvs/perfil",sep = "")
#fprof=makeFirefoxProfile(list(browser.download.dir = directorio_descarga))
#browser.helperApps.neverAsk.saveToDisk = 'application/zip'
rD=rsDriver(browser = "firefox")  #No se muy bien que hace pero es necesario para crear el navegador. Firefox mejor que chrome no?
#,extraCapabilities = fprof
#Se deberia haber creado una ventana de firefox. Si da un error tipo "noseque puerto ya esta en uso" usar las lineas de codigo de mas abajo: remDr$close() y rD$server$stop()
remDr=rD$client      #el objeto remDr es el que nos va a hacer todo el curro de ir a las paginas, interactuar con ellas ...
remDr$navigate(url)   #Nuestra ventana de firefox se mete en el link k le hemos puesto en la variable url
#Truco para ir usando el navegador: Si escribimos  navegador$ y tabulamos nos salen las posibles opciones (como el navigate de arriba). Las opciones vienen explicadas en el link que he `puesto arriba`

#Buscamos las cajas del texto donde se escriben las fechas----
fechainicio="11/08/2018 6:57 PM"
fechafinal="11/15/2018 6:57 PM"   #Cuando el programa este acabado las fechas deberian ser input de la funcion actualizadora
cajatexto_fechainicio=remDr$findElement(using = 'css selector', value = "#from")  #Encontar las cajetilla de la primera fecha
cajatexto_fechafinal=remDr$findElement(using = 'css selector', value = "#to")     #Encontrar la de la segunda
cajatexto_fechainicio$clearElement()  #Vaciar la primera cajetilla
cajatexto_fechafinal$clearElement()  #Vaciar la segunda cajetilla
cajatexto_fechainicio$sendKeysToElement(list(fechainicio))  #Escribir fechainicio en la primera cajetilla
cajatexto_fechafinal$sendKeysToElement(list(fechafinal))    #Escribir fechafinal en la segunda cajetilla

#Tambien buscamos el boton de exportar
#boton_exportar=remDr$findElement(using = 'css selector',value = "button.btn:nth-child(10)")
#boton_exportar$clickElement()

#Tambien buscamos el boton de refrescar
boton_refrescar=remDr$findElement(using = 'css selector',value = "button.btn:nth-child(9)")
boton_refrescar$clickElement()

#De aqui para abajo pruebas que voy haciendo por si nos da por hacer Rselenium + webscrapping
tabla=remDr$findElement(using = 'css selector', 
                        value = ".table > tbody:nth-child(2)")$getElementText() #

split_newline<- str_split(tabla[[1]],pattern = "\n")      #Separar tabla en filas
split_newline_matrix<- t(rbind(unlist(split_newline)))    #Convertirlo en matriz
lista_datos=str_split(split_newline_matrix,pattern = " ") #Pasarlo a lista separando por espacios
matriz_datos=matrix(unlist(lista_datos),ncol = 8,byrow = TRUE)      #Mira la lista pot filas y lo mete en 8 columnas
dataframe_datos=as.data.frame(matriz_datos)                         #Pasar a data.frame
dataframe_datos[,1]=mdy_hms(paste(dataframe_datos[,1],dataframe_datos[,2],dataframe_datos[,3],sep = " "))  #Meter en la primera columna el dia y la hora en formato fecha             
dataframe_datos=dataframe_datos[,c(1,4,6,8)]                #Coge solo las columnas que nos interesan (m/s fuera)
dataframe_datos[,4]=as.character(dataframe_datos[,4])
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
colnames(dataframe_datos)=c("Date","Mean","Max","Dir_deg")   #Nombrar columnas

#Cerrar el server y la ventana
remDr$close()
rD$server$stop()
