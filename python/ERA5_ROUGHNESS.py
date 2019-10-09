import matplotlib.pyplot as plt
from netCDF4 import Dataset
from mpl_toolkits.basemap import Basemap
from wrf import to_np, getvar, smooth2d, latlon_coords
import os
import re
from math import sin, cos, sqrt, atan2, radians
import pandas as pd
import numpy as np
from FOLDER_FILES_UTILS import MOVE_FILES, FIND_FILES_IN_FOLDER, CREATE_FOLDER, COMMANDO_POR_CONSOLA
import datetime


'''
IMPORTAMOS LOS DATOS QUE VAMOS A PLOTEAR DIRECTAMENTE DESDE LA CARPETA DEL 
MODELO Y TRANSFORMAMOS

'''
PATH_OUTPUT= 'MAPS/'
if not os.path.exists(PATH_OUTPUT):
    os.makedirs(PATH_OUTPUT)

PATH_WRFPRD= '/home/oscar/App_Anemometros/python/ERA5/ERA5_rugosidad/'
NETCDFS_2PM= [item for item in os.listdir(PATH_WRFPRD) if 'Z0' in item]

#for NETCDF in NETCDFS_2PM: 
NETCDF= NETCDFS_2PM[0]

wrf_out_file = PATH_WRFPRD + NETCDF
    # Open the NetCDF file
ncfile = Dataset(wrf_out_file)

# Get the sea level pressure
SRFC_ROUGNESS = ncfile.variables['fsr']


LON = ncfile.variables['longitude'][:]

LAT = ncfile.variables['latitude'][:]





XX, YY  = np.meshgrid(LON, LAT)

MAP = Basemap(llcrnrlon=LON.min(),
    llcrnrlat=LAT.min(),
    urcrnrlon=LON.max(),
    urcrnrlat=LAT.max(),
    lon_0= (LON.max()-(LON.max()- LON.min())/2),
    lat_0= (LAT.max()-(LAT.max()- LAT.min())/2),
    resolution= 'h',
    projection = 'lcc')

x , y = MAP(XX, YY)

lon = -2.488504
lat = 43.179389
lonm, latm= np.meshgrid(lon, lat)
levels = np.arange(0,2, 0.01)



for NETCDF in [item for item in NETCDFS_2PM if '2019' in item]:
    wrf_out_file = PATH_WRFPRD + NETCDF
        # Open the NetCDF file
    ncfile = Dataset(wrf_out_file)
    
    # Get the sea level pressure
    SRFC_ROUGNESS = ncfile.variables['fsr']
    
    ORIGIN= datetime.datetime.strptime( '1900-01-01 00:00:00', '%Y-%m-%d %H:%M:%S')
    time= ncfile.variables['time'][:]
    
    
    for i in np.arange(1,len(time)):
        DATE = ORIGIN + datetime.timedelta(hours= int(time[i]))
        DATE_STR = DATE.strftime('%Y-%m-%d %H')
        if os.path.isfile(PATH_OUTPUT + 'SRFC_COLORMESH_2019' + DATE_STR.replace(' ','') + '.png'):
            pass
        else:

            data= SRFC_ROUGNESS[i,:,:]
            
            fig, ax = plt.subplots(figsize=(10,10),facecolor = 'white')
            ax.axis('off')
            
            MAP.drawcoastlines()
            MAP.pcolormesh(x, y,data)
            
            
            
            x1,y1 = MAP(lonm, latm)
            MAP.plot(x1, y1, 'ro', markersize=10)
            
            MAP.plot(x, y, 'go', markersize=1)
            
            
            plt.colorbar(orientation ='horizontal', pad=0.005)
            plt.title('SURFACE ROUGHNESS \n' +  DATE_STR)
            
            plt.savefig(PATH_OUTPUT + 'SRFC_COLORMESH_2019' + DATE_STR.replace(' ',''),
                        dpi= 100, transparent =False,
                        facecolor = fig.get_facecolor() )
            plt.close()
            
            fig, ax = plt.subplots(figsize=(10,10),facecolor = 'white')
            ax.axis('off')
            
            MAP.drawcoastlines()
            MAP.contourf(x, y,
                         data, levels= levels)
            
            
            
            x1,y1 = MAP(lonm, latm)
            MAP.plot(x1, y1, 'ro', markersize=10)
            
            MAP.plot(x, y, 'go', markersize=1)
            
            
            plt.colorbar(orientation ='horizontal', pad=0.005)
            plt.title('SURFACE ROUGHNESS \n' +  DATE_STR)
            
            plt.savefig(PATH_OUTPUT + 'SRFC_CONTOURF_2019' + DATE_STR.replace(' ',''),
                        dpi= 100, transparent =False,
                        facecolor = fig.get_facecolor() )
            plt.close()




'''
COMANDOS A INTEGRAR PARA HACER UN GIF C COLORMAP SHOWCASEON PAUSA ENTRE LOOPS
'''
PATH_COMANDO= PATH_OUTPUT
COMMANDO= 'ffmpeg -pattern_type glob -r 96 -i "SRFC_CONTOURF_20192019*.png" -c:v libx264 -vf fps=25 -pix_fmt yuv420p SRFC_CONTOURF2019.mp4'
COMMANDO_POR_CONSOLA(COMMANDO, PATH_COMANDO)

COMMANDO= 'ffmpeg -pattern_type glob -r 96 -i "SRFC_COLORMESH_20192019*.png" -c:v libx264 -vf fps=25 -pix_fmt yuv420p SRFC_COLORMESH2019.mp4'
COMMANDO_POR_CONSOLA(COMMANDO, PATH_COMANDO)