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

POST(url=link)"
  
#### c("0B38DAE79059","03254ACF1E27","08610F383C7B","08734DCAC206","0B1FDF90050B","032B8F08E708")
##phoneid= "640689911849"



###################
# Con este script somos capaces de acceder al último valor registrado en la web

library(httr)

url<- "https:/www.data199.com/api/pv1/device/lastmeasurement"
body<- list(deviceids= "0B38DAE79059,08610F383C7B,08734DCAC206,0B1FDF90050B")

POST(url,body = body,verbose(),encode = "form")

########################
add_headers(User-Agent= "remotemonitor/248 CFNetwork/758.2.8 Darwin/15.0.0",
            Accept-Language= "en-us",
            Content-Type= "application/x-www-form-urlencoded; charset=utf-8",
            Host= "www.data199.com:8080")

url1<- "http://www.data199.com/api/v1/dashboard"
body1<- list(devicetoken = 'empty',			
             vendorid = 'f193c634-2611-475b-ba7a-27b0ead33c6f',	
             phoneid = 'Unknow',					
             version = '1.37',				
             build = '136',						
             executable = 'Mobile-Alerts',		
             bundle = 'de.synertronixx.remotemonitor',	
             lang = 'en',	
             deviceids= "0B38DAE79059,08610F383C7B,08734DCAC206,0B1FDF90050B")

POST(url1,
     body = body,verbose(),
     encode = "form",
     user_agent("remotemonitor/248 CFNetwork/758.2.8 Darwin/15.0.0"))
