library(RSelenium)
#Dejo link tutorial de este paquete:
#https://www.computerworld.com/article/2971265/application-development/how-to-drive-a-web-browser-with-r-and-rselenium.html
#Para ir probando pongo por ahora solo el link del Anem Hexagono. Cuando se vea que funciona habria k generalizarlo para poder usarlo en cualquier sensor
url="https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
a=rsDriver(browser = "firefox")  #No se muy bien que hace pero es necesario para crear el navegador. Firefox mejor que chrome no?
#Se deberia haber creado una ventana de firefox. Si da un error tipo "noseque puerto ya esta en uso" abrir y cerrar Rstudio
navegador=a$client      #el objeto navegador es el que nos va a hacer todo el curro de ir a las paginas, interactuar con ellas ...
navegador$navigate(url)   #Nuestra venta de firefox se mete en el link k le hemos puesto en la variable url
#Truco para ir usando el navegador: Si escribimos  navegador$ y tabulamos nos salen las posibles opciones (como el navigate de arriba). Las opciones vienen explicadas en el link que he `puesto arriba`

#Buscamos las cajas del texto donde se escriben las fechas----
fechainicio="11/08/2018 6:57 PM"
fechafinal="11/15/2018 6:57 PM"   #Cuando el programa este acabado las fechas deberian ser input de la funcion actualizadora
cajatexto_fechainicio=navegador$findElement(using = 'css selector', value = "#from")  #Encontar las cajetilla de la primera fecha
cajatexto_fechafinal=navegador$findElement(using = 'css selector', value = "#to")     #Encontrar la de la segunda
cajatexto_fechainicio$clearElement()  #Vaciar la primera cajetilla
cajatexto_fechafinal$clearElement()  #Vaciar la segunda cajetilla
cajatexto_fechainicio$sendKeysToElement(list(fechainicio))  #Escribir fechainicio en la primera cajetilla
cajatexto_fechafinal$sendKeysToElement(list(fechafinal))    #Escribir fechafinal en la segunda cajetilla

#Antes de darle al boton de exportar tenemos que cambiar unos ajustes de nuestra ventana de firefox
#El problema es que si al exportar nos sale la mitica ventana de Abrir archivo/Guardar archivo
#Selenium no puede leerlo por que es una ventana de windows, no de firefox
#Cambiamos los ajustes para que se descargue directamente al darle al boton

#Tambien buscamos el boton de exportar
boton_exportar=navegador$findElement(using = 'css selector',value = "button.btn:nth-child(10)")
boton_exportar$clickElement()

