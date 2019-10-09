#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 20 20:04:58 2019

@author: oscar
"""
import os
import shutil
import subprocess

def FIND_FILES_IN_FOLDER(PATH):
    '''
    A ESTA FUNCION LE DAS EL PATH DONDE QUIERES BUSCAR ARCHIVOS Y TE DEVLEVE
    UNA LISTA CON TODOS LOS ARCHIVOS Y EL PATH ENTERO    
    '''    
    LISTA_FILES= []
    for root, dirs, files in os.walk(PATH, topdown=False):
       for name in files:
          LISTA_FILES.append(os.path.join(root, name))    
    return LISTA_FILES

def CREATE_FOLDER(PATH):
    if not os.path.exists(PATH):
            os.makedirs(PATH)
            
            
def FIND_DOMAIN_FOLDER(PATH):
    '''
    ESTA FUNCION LISTA CARPETAS DENTRO DE LA CARPETA SEÑALADA    
    '''    
    LISTA_FOLDERS= []
    for root, dirs, files in os.walk(PATH, topdown=False):
       for name in dirs:
          LISTA_FOLDERS.append(os.path.join(root, name))
    
    return LISTA_FOLDERS

def MOVE_FILES(FILES, TO_PATH):
    '''
    FUNCION PARA MOVER ARCHIVOS DE UN LADO A OTRO
    HAY QUE METERLE EL ARCHIVO CON EL PATH ENTERO
    '''
    if not os.path.exists(TO_PATH):
        os.makedirs(TO_PATH)
    for file in FILES:         
        shutil.move(file, TO_PATH + '/' + file.split('/')[-1])

def COPY_FILES(FILES, TO_PATH):
    '''
    FUNCION PARA COPIAR LOS ARCHIVOS DE UN LADO A OTRO
    HAY QUE DAR EL PATH ENTERO DE LOS ARCHIVSO Y EL PATH DE DESTINO
    '''
    if not os.path.exists(TO_PATH):
        os.makedirs(TO_PATH)
    for file in FILES:         
        shutil.copy(file, TO_PATH + '/' + file.split('/')[-1])
        
       
def COMMANDO_POR_CONSOLA(COMMANDO, PATH_COMANDO):
    '''
    FUNCION PARA EJECUTAR UN COMMANDO POR CONSOLA
    HAY QUE DECIR EL COMMANDO QUE QUEREMOS Y EL PATH DONDE SE VA A 
    EJECUTAR DE ESTA MANERA ELIMINAMOS LAS NECESIDAD DE cd /home/ ....
    '''
    #EJECUTAMOS EL MODELO 
    procces= subprocess.Popen(COMMANDO,stdout= subprocess.PIPE, cwd= PATH_COMANDO, shell=True)
    output, error = procces.communicate()
    return output        
            

