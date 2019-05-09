#EXPLICACI0N
#Este script contiene las funciones necesarias para bajar los cvs


def conseguir_path_app_anemometros():
       #Devuelve el path de App_anemometros
       #Para que funciones tienes que estar en una de las carpetas de la repo.
       #Funciona en cualquier ordenador.
       import re
       import os
       
       path_viejo=os.getcwd()
       pattern=re.compile('.+'                          #Cualquier cosa...
                          '\\\App_Anemometros')         #... hasta esto (inclusive)
       try:
              path_app=re.findall(pattern,path_viejo)[0]
               #Le ponemos [0] porque re.findall() crea una lista de strings,
               #y nosotros queremos el primer (y único) string
       except IndexError:   #Dará este error si re.findall() no devuelve nada.
              print('ERROR.Directorio de trabajo incorrecto. Cambia el\n'
                    'directorio de trabajo de python a ~\App_anemometros y\n'
                    'vuelve a intentarlo')
       return(path_app)
               
def conseguir_ids():
       #ids=conseguir_ids()
       #id_sensor=conseguir_ids()[n]
       #Esta funcion muestra la id de cada sensor
       #Devuelve el vetor ids
       
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
              ids[i]=re.findall(pattern,lista[i])[0][43:-14]
              
       #Cosechar el nombre de cada elemento de la lista
       nombres=[None] * len(lista)
       pattern=re.compile('&amp;appbundle=eu.mobile_alerts.mobilealerts">'
                         '.+'
                         '</a>')
       for i in range(0,len(lista)):
              nombres[i]=re.findall(pattern,lista[i])[0][46:-4]
              
       #Ahora que tenemos las ids y los nombres, vamos a escribirlos en la consola
       for i in range(0,len(lista)):
              print('['+str(i)+'] \"'+ids[i]+'\" - \"'+nombres[i]+'\"')
       return(ids)
       
       
#fechainicio="11/08/2018 6:57 PM"
#fechafinal="11/15/2018 6:57 PM"

def bajar_cvs(fechainicio,fechafinal,id_sensor,driver):
       #Esta funcion baja el .zip correspondiente a las fechas y url de sensor dadas,
       #y lo descomprime
       #id_sensor puede ser tanto la id en si, o el numero del sensor (el que
       #marca la funcion conseguir_ids())
       
       import re
       import requests
       
       #Vamos a mirar el codigo html de la pagina web donde nos salen todos los
       #sensores, y ver que url le corressponde a id_sensor
       url_menu="https://measurements.mobile-alerts.eu/Home/SensorsOverview?phoneid=640689911849"
       codigo_html=requests.get(url_menu).text
       if isinstance(id_sensor,int):
              pattern=re.compile('<a href="/Home/MeasurementDetails\?deviceid='
                                 '.+'
                                 '&amp;vendorid='
                                 '.[^\"]+')
              final_url_sensor=re.findall(pattern,codigo_html)[id_sensor][9:]
       if isinstance(id_sensor,str):       
              pattern=re.compile('<a href="/Home/MeasurementDetails\?deviceid='
                                 +id_sensor+
                                 '&amp;vendorid='
                                 '.[^\"]+')
              final_url_sensor=re.findall(pattern,codigo_html)[0][9:]
       
       #La codificacion de caracteres debe dar algun problema por que en el
       #codigo_html el final_url_sensor sale con "amp;" (sin comillas), pero
       #la url de verdad no tiene. En este parrafo lo filtro.
       final_url_sensor=re.sub('amp;',"",final_url_sensor)
       
       url_sensor="https://measurements.mobile-alerts.eu"+final_url_sensor      
       
       driver.get(url_sensor)     
       
       #Buscamos las cajas del texto donde se escriben las fechas
       cajatexto_fechainicio=driver.find_element_by_css_selector("#from")  #Encontar las cajetilla de la primera fecha
       cajatexto_fechafinal=driver.find_element_by_css_selector("#to")     #Encontrar la de la segunda
       cajatexto_fechainicio.clear()  #Vaciar la primera cajetilla
       cajatexto_fechafinal.clear()   #Vaciar la segunda cajetilla
       cajatexto_fechainicio.send_keys(fechainicio)  #Escribir fechainicio en la primera cajetilla
       cajatexto_fechafinal.send_keys(fechafinal)    #Escribir fechafinal en la segunda cajetilla
       boton_exportar=driver.find_element_by_css_selector("button.btn:nth-child(11)")
       boton_exportar.click()

def crear_driver():
       #Esta funcion crea el objeto driver de selenium. Tiene el directorio de
       #descarga en ~\App_anemometros\NUEVO\Data_anemometros\.
       from selenium import webdriver
       
       path_data_anemometros=conseguir_path_app_anemometros() + '\\NUEVO\Data_anemometros'

       #Crear y ajustar perfil de firefox.
       profile = webdriver.FirefoxProfile()
       profile.set_preference('browser.download.folderList', 2) # custom location
       profile.set_preference('browser.download.manager.showWhenStarting', False)
       profile.set_preference('browser.download.dir', path_data_anemometros)
       profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/zip')
       
       driver=webdriver.Firefox(profile)
       return(driver)
       
#PRUEBAS. Lo que hay de aqui para bajo hace lo mismo que Descarga_datos_unzip.py
#pero integrado con las funciones de bajada de datos definidas  encima de esto.
import csv
import datetime
import os
import numpy as np
import zipfile

path_registro=conseguir_path_app_anemometros() + '\\NUEVO\Data_anemometros\TABLA_REGISTRO.csv'
with open(path_registro,'rt') as csvfile:        #He cambiado 'rb' por 'rt'. Daba error.
    data= list(csv.reader(csvfile,delimiter=';'))


path_data_anemometros=conseguir_path_app_anemometros() + '\\NUEVO\Data_anemometros'
os.chdir(path_data_anemometros)
driver=crear_driver()

for x in [1,2,3]:
    id_sensor= data[x][0]    #Aqui tenias puesto phone_id pero no es correcto y
    #puede crear confusiones (mezclaba la id del telefono, que es igual para
    #todos los sensores, con la id de cada sensor)
    fecha_ini=data[x][1]

    fecha_ahora= datetime.datetime.now()
    fecha_fin=fecha_ahora.strftime("%d/%m/%Y") 
    
    bajar_cvs(fecha_ini,fecha_fin,id_sensor,driver)
    
driver.close()

x=os.listdir(path_data_anemometros)       

#Descomprimir todos los zips
for i in np.arange(0,len(x),step=1):
  y=x[i]
  if '.zip' not in y:
    pass
  else:
    path=os.path.abspath(y)
    zip_ref= zipfile.ZipFile(path)
    zip_ref.extractall(os.getcwd())
    zip_ref.close()
      
#Borrar los zips
for i in np.arange(0,len(x),step=1):
  y=x[i]
  if '.zip' not in y:
    pass
  else:
    path=os.path.abspath(y)
    os.remove(path)