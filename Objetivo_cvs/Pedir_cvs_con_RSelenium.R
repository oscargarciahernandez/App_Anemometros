library(RSelenium)
library(here)
#Dejo link tutorial de este paquete:
#https://www.computerworld.com/article/2971265/application-development/how-to-drive-a-web-browser-with-r-and-rselenium.html
#Para ir probando pongo por ahora solo el link del Anem Hexagono. Cuando se vea que funciona habria k generalizarlo para poder usarlo en cualquier sensor
url="https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
#directorio_descarga=paste(here::here(),"/Objetivo_cvs",sep = "")
#directorio_perfil=paste(here::here(),"/Objetivo_cvs/perfil",sep = "")
fprof=makeFirefoxProfile(list(browser.download.dir = directorio_descarga))
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

#Cerrar el server y la ventana
remDr$close()
rD$server$stop()

#De aqui para abajo pruebas que voy haciendo por si nos da por hacer Rselenium + webscrapping
tabla=remDr$findElement(using = 'css selector', value = ".table > tbody:nth-child(2)")$getElementText()

