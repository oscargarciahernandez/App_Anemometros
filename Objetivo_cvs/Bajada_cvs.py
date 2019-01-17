from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located
import os
#Profila egin eta ajustatu (deskarga karpeta zehaztu, lehioa agertu gabe deskargatu)
profile = webdriver.FirefoxProfile()
profile.set_preference('browser.download.folderList', 2) # custom location
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference('browser.download.dir', os.getcwd()+'\Objetivo_cvs')
profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'application/zip')

path=os.getcwd()+'\Objetivo_cvs'
driver=webdriver.Firefox(profile)    #Firefox lehio zombia abian jarri
url="https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid=0B75FE3A4FB6&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
driver.get(url)     #Horra joan

#Buscamos las cajas del texto donde se escriben las fechas
fechainicio="11/08/2018 6:57 PM"
fechafinal="11/15/2018 6:57 PM"   #Cuando el programa este acabado las fechas deberian ser input de la funcion actualizadora
cajatexto_fechainicio=driver.find_element_by_css_selector("#from")  #Encontar las cajetilla de la primera fecha
cajatexto_fechafinal=driver.find_element_by_css_selector("#to")     #Encontrar la de la segunda
cajatexto_fechainicio.clear()  #Vaciar la primera cajetilla
cajatexto_fechafinal.clear()   #Vaciar la segunda cajetilla
cajatexto_fechainicio.send_keys(fechainicio)  #Escribir fechainicio en la primera cajetilla
cajatexto_fechafinal.send_keys(fechafinal)    #Escribir fechafinal en la segunda cajetilla

boton_exportar=driver.find_element_by_css_selector("button.btn:nth-child(10)")
boton_exportar.click()
driver.close()


