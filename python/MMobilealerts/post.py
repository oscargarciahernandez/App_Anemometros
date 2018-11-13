import requests
import datetime
import hashlib

url='http://www.data199.com:8080/api/v1/dashboard'
headers= {"User-Agent" : "remotemonitor/248 CFNetwork/758.2.8 Darwin/15.0.0",
"Accept-Language" : "en-us",
"Content-Type": "application/x-www-form-urlencoded; charset=utf-8",
"Host" : "www.data199.com:8080"}

devicetoken = 'empty'
vendorid = '032B8F08E708&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f'
phoneid = '640689911849'
version = '1.37'
build = '136'
executable = 'Mobile Alerts'
bundle = 'eu.mobile_alerts.mobilealerts'
lang = 'en'

request = "devicetoken=%s&vendorid=%s&phoneid=%s&version=%s&build=%s&executable=%s&bundle=%s&lang=%s" % (devicetoken,vendorid,phoneid,version,build,executable,bundle,lang)
request += '&timezoneoffset=%d' % 60		
request += '&timeampm=%s' % ('true')		
request += '&usecelsius=%s' % ('true')		
request += '&usemm=%s' % ('true')			
request += '&speedunit=%d' % 0,			
request += '&timestamp=%s' % datetime.datetime.utcnow().strftime("%s")	

requestMD5 = request + 'asdfaldfjadflxgeteeiorut0ß8vfdft34503580'	
requestMD5 = requestMD5.replace('-','')
requestMD5 = requestMD5.replace(',','')
requestMD5 = requestMD5.replace('.','')
requestMD5 = requestMD5.lower()

m = hashlib.md5()      
m.update(requestMD5)
hexdig = m.hexdigest()

request += '&requesttoken=%s' % hexdig

request += '&deviceids=%s' % ','.join(sensors)








