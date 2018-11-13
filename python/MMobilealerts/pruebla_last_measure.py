import requests
import datetime
import hashlib


url='http://www.data199.com:8080/api/v1/dashboard'
headers= {"User-Agent" : "remotemonitor/248 CFNetwork/758.2.8 Darwin/15.0.0",
"Accept-Language" : "en-us",
"Content-Type": "application/x-www-form-urlencoded; charset=utf-8",
"Host" : "www.data199.com:8080"}




devicetoken = 'empty'				# defaults to "empty"
vendorid = 'BE60BB85-EAC9-4C5B-8885-1A54A9D51E29'	# iOS vendor UUID (returned by iOS, any UUID will do). Launch uuidgen from the terminal to generate a fresh one.
phoneid = 'Unknown'					# Phone ID - probably generated by the server based on the vendorid (this string can be "Unknown" and it still works)
version = '1.21'					# Info.plist CFBundleShortVersionString
build = '248'						# Info.plist CFBundleVersion
executable = 'Mobile Alerts'		# Info.plist CFBundleExecutable
bundle = 'de.synertronixx.remotemonitor'	# [[NSBundle mainBundle] bundleIdentifier]
lang = 'en'							# preferred language

request = "devicetoken=%s&vendorid=%s&phoneid=%s&version=%s&build=%s&executable=%s&bundle=%s&lang=%s" % (devicetoken,vendorid,phoneid,version,build,executable,bundle,lang)
request += '&timezoneoffset=%d' % 60		# local offset to UTC time
request += '&timeampm=%s' % ('true')		# 12h vs 24h clock
request += '&usecelsius=%s' % ('true')		# Celcius vs Fahrenheit
request += '&usemm=%s' % ('true')			# mm va in
request += '&speedunit=%d' % 0				# wind speed (0: m/s, 1: km/h, 2: mph, 3: kn)
request += '&timestamp=%s' % datetime.datetime.utcnow().strftime("%s")	# current UTC timestamp

requestMD5 = request + 'asdfaldfjadflxgeteeiorut0�8vfdft34503580'	# SALT for the MD5
requestMD5 = requestMD5.replace('-','')
requestMD5 = requestMD5.replace(',','')
requestMD5 = requestMD5.replace('.','')
requestMD5 = requestMD5.lower()

m = hashlib.md5()
m.update(requestMD5)
hexdig = m.hexdigest()

request += '&requesttoken=%s' % hexdig

request += '&deviceids=0B75FE3A4FB6,0B38DAE79059,03254ACF1E27,08610F383C7B,08734DCAC206,0B1FDF90050B,032B8F08E708'
#request += '&measurementfroms=%s' % ('0,' * len(sensors))
#request += '&measurementcounts=%s' % ('50,' * len(sensors))
