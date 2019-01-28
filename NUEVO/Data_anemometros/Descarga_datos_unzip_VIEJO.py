from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located
import os
import csv
import datetime
import os
import numpy as np
import zipfile

path_registro='C:\Users\Oscar\Documents\App_Anemometros\NUEVO\Data_anemometros\TABLA_REGISTRO.csv'
with open(path_registro,'rb') as csvfile:
    data= list(csv.reader(csvfile,delimiter=';'))



profile = webdriver.FirefoxProfile()
profile.set_preference('browser.download.folderList', 2) 
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference('browser.download.dir', os.getcwd())
profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/zip')

path=os.getcwd()
driver=webdriver.Firefox(profile)
for x in [1,2,3]:
    phone_id= data[x][0]
    fecha_ini=data[x][1]

    fecha_ahora= datetime.datetime.now()
    fecha_fin=fecha_ahora.strftime("%d/%m/%Y") 



    
    url1='https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid='
    url2='&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts'
    url3= url1+ phone_id + url2
    driver.get(url3)     

      
    cajatexto_fechainicio=driver.find_element_by_css_selector("#from")  
    cajatexto_fechafinal=driver.find_element_by_css_selector("#to")     
    cajatexto_fechainicio.clear()  
    cajatexto_fechafinal.clear()   
    cajatexto_fechainicio.send_keys(fecha_ini)  
    cajatexto_fechafinal.send_keys(fecha_fin)    

    boton_exportar=driver.find_element_by_css_selector("button.btn:nth-child(10)")
    boton_exportar.click()
    
driver.close()



x=os.listdir(os.getcwd())

for i in np.arange(0,len(x),step=1):
  y=x[i]
  if '.zip' not in y:
    pass
  else:
    path=os.path.abspath(y)
    zip_ref= zipfile.ZipFile(path)
    zip_ref.extractall(os.getcwd())
    zip_ref.close()
      

for i in np.arange(0,len(x),step=1):
  y=x[i]
  if '.zip' not in y:
    pass
  else:
    path=os.path.abspath(y)
    os.remove(path)
