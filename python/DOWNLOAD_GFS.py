#!/usr/bin/env python
#################################################################
# Python Script to retrieve 8 online Data files of 'ds084.1',
# total 1.64G. This script uses 'requests' to download data.
#
# Highlight this script by Select All, Copy and Paste it into a file;
# make the file executable and run it on command line.
#
# You need pass in your password as a parameter to execute
# this script; or you can set an environment variable RDAPSWD
# if your Operating System supports it.
#
# Contact rpconroy@ucar.edu (Riley Conroy) for further assistance.
#################################################################


import sys, os
import requests
import numpy as np

def check_file_status(filepath, filesize):
    sys.stdout.write('\r')
    sys.stdout.flush()
    size = int(os.stat(filepath).st_size)
    percent_complete = (size/filesize)*100
    sys.stdout.write('%.3f %s' % (percent_complete, '% Completed'))
    sys.stdout.flush()

# Try to get password
if len(sys.argv) < 2 and not 'RDAPSWD' in os.environ:
    try:
        import getpass
        input = getpass.getpass
    except:
        try:
            input = raw_input
        except:
            pass
    pswd = input('Password: ')
else:
    try:
        pswd = sys.argv[1]
    except:
        pswd = os.environ['RDAPSWD']

url = 'https://rda.ucar.edu/cgi-bin/login'
values = {'email' : 'garcia96.oscar@gmail.com', 'passwd' : 'garcia96', 'action' : 'login'}
# Authenticate
session = requests.Session()
session.trust_env = False
ret = requests.post(url,data=values)
if ret.status_code != 200:
    print('Bad Authentication')
    print(ret.text)
    exit(1)
dspath = 'http://rda.ucar.edu/data/ds084.1/'

year= 2019

month= np.arange(1,3)
day= np.arange(1,32)
horas= [0,3,6,9,12,15,18,21,24]

filelist=[]
#for mes in np.arange(0,len(month)):
mes=0
for dia in np.arange(0, len(day)):
    for hour in np.arange(0,len(horas)):
        x=format(year, '04d')+'/'+format(year, '04d')+format(month[mes], '02d')+format(day[dia], '02d')+ \
        '/gfs.0p25.'+format(year, '04d')+format(month[mes], '02d')+format(day[dia], '02d')+'00.f'+\
        format(horas[hour], '03d')+'.grib2'
        filelist.append(x)



# EXAMPLE -----   '2019/20190101/gfs.0p25.2019010100.f000.grib2'

for file in filelist:
    filename=dspath+file
    file_base = os.path.basename(file)
    print('Downloading',file_base)
    req = session.get(filename, cookies = ret.cookies, allow_redirects=True, stream=True)
    filesize = int(req.headers['Content-length'])
    with open(file_base, 'wb') as outfile:
        chunk_size=1048576
        for chunk in req.iter_content(chunk_size=chunk_size):
            outfile.write(chunk)
            if chunk_size < filesize:
                check_file_status(file_base, filesize)
    check_file_status(file_base, filesize)
    print()



