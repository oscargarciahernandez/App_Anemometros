library(httr)

link="http://www.data199.com:8080/api/v1/dashboard"   #El link al que le haremos el post request

cuerpo=list(devicetoken = 'empty',				
            vendorid = 'BE60BB85-EAC9-4C5B-8885-1A54A9D51E29',	
            phoneid = 'Unknown',					
            version = '1.21',					
            build = '248',						
            executable = 'Mobile Alerts',		
            bundle = 'de.synertronixx.remotemonitor',	
            lang = 'en',							
            
            request = "devicetoken=%s&vendorid=%s&phoneid=%s&version=%s&build=%s&executable=%s&bundle=%s&lang=%s" % (devicetoken,vendorid,phoneid,version,build,executable,bundle,lang),
            request += '&timezoneoffset=%d' % 60,		
            request += '&timeampm=%s' % ('true'),		
            request += '&usecelsius=%s' % ('true'),		
            request += '&usemm=%s' % ('true'),			
            request += '&speedunit=%d' % 0,				
            request += '&timestamp=%s' % datetime.datetime.utcnow().strftime("%s"),	
            
            requestMD5 = request + 'asdfaldfjadflxgeteeiorut0ß8vfdft34503580',	
            requestMD5 = requestMD5.replace('-',''),
            requestMD5 = requestMD5.replace(',',''),
            requestMD5 = requestMD5.replace('.',''),
            requestMD5 = requestMD5.lower(),
            
            m = hashlib.md5(),
            m.update(requestMD5),
            hexdig = m.hexdigest(),
            
            request += '&requesttoken=%s' % hexdig,
            
            request += '&deviceids=%s' % ','.join(sensors),
            #request += '&measurementfroms=%s' % ('0,' * len(sensors)),
            #request += '&measurementcounts=%s' % ('50,' * len(sensors)))

POST(url=link)
  