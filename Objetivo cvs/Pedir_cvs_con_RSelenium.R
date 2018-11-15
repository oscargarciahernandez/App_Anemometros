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


