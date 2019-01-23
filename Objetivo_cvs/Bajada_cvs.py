#EXPLICACIÓN
#Este script contiene las funciones necesarias para bajar los cvs

#INSTRUCCIONES DE USO
#

def cambiar_path_a_objetivo_cvs():
       #Esta funcion se asegura de que el directorio de trabajo sea
       #~/Objetivo_cvs, y asi evitamos ir dejando los cvs en carpetas que no
       #deberían.
       
       import re
       import os
       
       path_viejo=os.getcwd()
       pattern=re.compile('.+'                          #Cualquier cosa...
                          '\\\App_Anemometros')         #... hasta esto (inclusive)
       try:
              path_app=re.findall(pattern,path_viejo)[0]
               #Le ponemos [0] porque re.findall() crea una lista de strings,
               #y nosotros queremos el primer (y único) string
              path_objetivo_cvs=path_app + '\Objetivo_cvs'
              os.chdir(path_objetivo_cvs)  
       except IndexError:   #Dará este error si re.findall() no devuelve nada.
              print('ERROR.Directorio de trabajo incorrecto. Cambia el\n'
                    'directorio de trabajo de python a ~\App_anemometros y\n'
                    'vuelve a intentarlo')
               
def conseguir_ids():
       #Esta funcion muestra la id de cada sensor
       
       import requests
       import re
       
       url="https://measurements.mobile-alerts.eu/Home/SensorsOverview?phoneid=640689911849"
       codigo_html=requests.get(url).text
       pattern=re.compile('<a href="/Home/MeasurementDetails\?deviceid='
                         '.+'
                         '</a>')
       lista=re.findall(pattern,codigo_html)
       #Cada elemento de esta lista es un string correspondiente a un  sensor.
       #Contiene la referencia necesariampara construir la url de ese sensor,
       #su id y su nombre. Vamos a extraer cada cosa.
       
       #Cosechar la id de cada elemento de la lista
       ids=[None] * len(lista)
       pattern=re.compile('<a href="/Home/MeasurementDetails\?deviceid='
                         '.+'
                         '&amp;vendorid=')
       for i in range(0,len(lista)):
              ids[i]=re.findall(pattern,lista[i])[0][44:-14]
              
       #Cosechar el nombre de cada elemento de la lista
       nombres=[None] * len(lista)
       pattern=re.compile('&amp;appbundle=eu.mobile_alerts.mobilealerts">'
                         '.+'
                         '</a>')
       for i in range(0,len(lista)):
              nombres[i]=re.findall(pattern,lista[i])[0][46:-4]
              
       #Ahora que tenemos las ids y los nombres, vamos a escribirlos en la consola
       for i in range(0,len(lista)):
              print('\"'+ids[i]+'\" - \"'+nombres[i]+'\"')
       
       
fechainicio="11/08/2018 6:57 PM"
fechafinal="11/15/2018 6:57 PM"
id_sensor="0B75FE3A4FB6"

def bajar_cvs(fechainicio,fechafinal,id_sensor):
       #Esta funcion baja el .zip correspondiente a las fechas y url de sensor dadas,
       #y lo descomprime
       
       from selenium import webdriver
       import re
       import requests
       
       cambiar_path_a_objetivo_cvs()

       #Crear y ajustar perfil de firefox.
       profile = webdriver.FirefoxProfile()
       profile.set_preference('browser.download.folderList', 2) # custom location
       profile.set_preference('browser.download.manager.showWhenStarting', False)
       profile.set_preference('browser.download.dir', os.getcwd())
       profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/zip')
       
       #Vamos a mirar el codigo html de la página web donde nos salen todos los
       #sensore, y ver cual corressponde a id_sensor
       url="https://measurements.mobile-alerts.eu/Home/SensorsOverview?phoneid=640689911849"
       codigo_html=requests.get(url).text
       patter=re.compile('<a href="/Home/MeasurementDetails?deviceid='
                         +id_sensor)
      
       driver=webdriver.Firefox(profile)    #Firefox lehio zombia abian jarri
       driver.get(url)     #Horra joan
       
       #Buscamos las cajas del texto donde se escriben las fechas
       cajatexto_fechainicio=driver.find_element_by_css_selector("#from")  #Encontar las cajetilla de la primera fecha
       cajatexto_fechafinal=driver.find_element_by_css_selector("#to")     #Encontrar la de la segunda
       cajatexto_fechainicio.clear()  #Vaciar la primera cajetilla
       cajatexto_fechafinal.clear()   #Vaciar la segunda cajetilla
       cajatexto_fechainicio.send_keys(fechainicio)  #Escribir fechainicio en la primera cajetilla
       cajatexto_fechafinal.send_keys(fechafinal)    #Escribir fechafinal en la segunda cajetilla
       boton_exportar=driver.find_element_by_css_selector("button.btn:nth-child(10)")
       boton_exportar.click()
       driver.close()
