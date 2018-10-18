library(tidyverse)
library(httr)
library(XML)
library(stringr)
library(openair)
library(chron)
library(lubridate)
library(rlist)

id_iden<- function(phoneid){
  p<- "https://measurements.mobile-alerts.eu/Home/SensorsOverview?phoneid="
  p1<- paste0(p,phoneid)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  
  posprueba<- length(p4)
  nodeprueba<-as.character(p4[posprueba])
  
  p4<-toString(p4)
  
  
  ids<-str_extract_all(p4,"(?<=deviceid=)[[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]]") 
  xx<- length(ids[[1]])
  
  rr<-matrix(-31, ncol = 3, nrow = xx)
  for(i in 1:xx){
    
    
    ff<- str_locate(ids[[1]][i], "B")
    if(is.na(ff[1])){
      ff[1]<-0
      
    }else{
      ff<-ff}
    
    
    yy<- suppressWarnings(as.numeric(str_sub(ids[[1]][i], 2,2)))
    if(is.na(yy)){yy<-0}else{yy<-yy}
    
    
    
    if(ff[1]==2){
      rr[i,1]<- paste0("anem",i)
      rr[i,2]<- ids[[1]][i]
      rr[i,3]<-1}else{
        
        if(yy==8){
          rr[i,1]<- paste0("pluv",i)
          rr[i,2]<- ids[[1]][i]
          rr[i,3]<-2
        }else{
          rr[i,1]<- paste0("term_hig",i)
          rr[i,2]<- ids[[1]][i]
          rr[i,3]<-3
          
        }
        
        
      }
  }
  
  rr<-as.data.frame(rr)
  colnames(rr)<- c("Disp", "id","code")
  return(rr)
}

data_M_A<-function(id){
  
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  
  #######Valores por página
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  ### (1526901420)-21/5/2018 13:17:00  
  ### inicio de recopilación de datos
  xx<- str_locate(id, "B")
  if(is.na(xx[1])){
    xx[1]<-0
  }else{
    xx<-xx
  }
  
  
  if(xx[1]==2){
    
    
    U_dt<- matrix(-31, ncol = 10, nrow = vpp*p)
    k<-1
    enla1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
    for (j in 1:p) {
      enlace<- paste0(enla1,"page=",j,"&deviceid=",id,link2)
      request2<- POST(enlace, encode = "form")
      request_list2<- htmlParse(request2, asText = TRUE)
      rr<-getNodeSet(request_list2, "//tbody")
      rr1<- xmlToDataFrame(rr)
      rr1<-t(rr1)
      rr2<-str_extract_all(rr1, boundary("word"))
      
      
      for (i in 1:length(rr2)) {
        month<- as.numeric(rr2[[i]][1])
        day<- as.numeric(rr2[[i]][2])
        year<- as.numeric(rr2[[i]][3])
        hora<- as.numeric(rr2[[i]][4])
        min<- as.numeric(rr2[[i]][5])
        sec<- as.numeric(rr2[[i]][6])
        merid<- as.character(rr2[[i]][7])
        wind<-as.numeric(rr2[[i]][8])
        gust<-as.numeric(rr2[[i]][11])
        dir<-paste(rr2[[i]][14],rr2[[i]][15], sep = "-")
        
        xx<- cbind(month, day, year, hora, min, sec, merid, wind, gust, dir)
        U_dt[k,]<- xx
        k<- k+1
      }
    }
    
    
    colnames(U_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "Wd mean", "wd gust", "Dir")
    U_dt<- as.data.frame(U_dt)
    
    
    
    date_string<- matrix(-31, nrow  = length(U_dt[,1]))
    namesss<- matrix(-31, nrow = length(U_dt[,1]))
    for (i in 1: length(U_dt[,1])) {
      
      fecha1<- paste0( U_dt$Dia[i],'/',
                       U_dt$Mes[i],'/',
                       U_dt$anno[i])
      
      fecha2<-paste0(U_dt$Hora[i], ':',
                     U_dt$Min[i],':',
                     U_dt$Sec[i], ' ', U_dt$Merid[i] )
      
      fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                      format =  "%H:%M:%S")      
      
      fecha<- paste0(fecha1,' ',fecha2)
      date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
      namesss[i]<-as.character(fecha)
    }
    
    
    date_string<- as.data.frame(date_string)
    namesss<- as.data.frame(namesss)
    
    U_dt<- cbind(date_string, namesss, U_dt$`Wd mean`, U_dt$`wd gust`, U_dt$Dir)
    colnames(U_dt)<- c("s/since_1/1/1970", "date_string_hour","Mean","Max","Dir_ch")
    
    U_dt$Dir_ch<-str_remove(U_dt$Dir_ch, pattern = '-NA')
    dir_degrees_U<-matrix(-31, nrow = length(U_dt$Dir))
    
    for (i in 1:length(U_dt$Dir)) {
      yy<-U_dt$Dir[i]
      dir_degrees_U[i]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
                                                       ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                                                                         ifelse(yy=="East", 90, 
                                                                                                ifelse(yy=="East-southeast", 110,
                                                                                                       ifelse(yy=="Southeast", 135,
                                                                                                              ifelse(yy=="South-Southeast", 155, 
                                                                                                                     ifelse(yy=="South", 180, 
                                                                                                                            ifelse(yy=="South-southwest", 200, 
                                                                                                                                   ifelse(yy=="Southwest", 225,
                                                                                                                                          ifelse(yy=="West-southwest", 245, 
                                                                                                                                                 ifelse(yy=="West", 270, 
                                                                                                                                                        ifelse(yy=="West-northwest",290,
                                                                                                                                                               ifelse(yy=="Northwest", 315,
                                                                                                                                                                      ifelse(yy=="North-northwest", 335, 
                                                                                                                                                                             ifelse(yy=='-31',NA)))))))))))))))))
    }
    dir_degrees_U<- as.data.frame( dir_degrees_U)
    U_dt<- cbind(U_dt, dir_degrees_U)
    colnames(U_dt)<- c("s/since_1/1/1970", "date_string_hour","Mean","Max","Dir_ch","Dir_deg")
    Dataset_XX<- list(U_dt)
    return(Dataset_XX)
    
  } else{
    yy<- as.numeric(str_sub(id, 2,2))
    
    if(yy==8){
      
      
      
      
      
      pl_dt<- matrix(-31, ncol = 8, nrow = vpp*p)
      k<-1
      for (j in 1:p) {
        enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
        request2<- POST(enlace, encode = "form")
        request_list2<- htmlParse(request2, asText = TRUE)
        rr<-getNodeSet(request_list2, "//tbody")
        rr1<- xmlToDataFrame(rr)
        rr1<-t(rr1)
        rr2<-str_extract_all(rr1, boundary("word"))
        
        
        for (i in 1:length(rr2)) {
          month<- as.numeric(rr2[[i]][1])
          day<- as.numeric(rr2[[i]][2])
          year<- as.numeric(rr2[[i]][3])
          hora<- as.numeric(rr2[[i]][4])
          min<- as.numeric(rr2[[i]][5])
          sec<- as.numeric(rr2[[i]][6])
          merid<- as.character(rr2[[i]][7])
          rain<-as.numeric(rr2[[i]][8])
          
          
          xx<- cbind(month, day, year, hora, min, sec, merid, rain)
          pl_dt[k,]<- xx
          k<- k+1
        }
      }
      
      
      colnames(pl_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "rain (mm)")
      pl_dt<- as.data.frame(pl_dt)
      
      date_string<- matrix(-31, nrow  = length(pl_dt[,1]))
      namesss<- matrix(-31, nrow = length(pl_dt[,1]))
      for (i in 1: length(pl_dt[,1])) {
        
        fecha1<- paste0( pl_dt$Dia[i],'/',
                         pl_dt$Mes[i],'/',
                         pl_dt$anno[i])
        
        fecha2<-paste0(pl_dt$Hora[i], ':',
                       pl_dt$Min[i],':',
                       pl_dt$Sec[i], ' ', pl_dt$Merid[i] )
        
        fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                        format =  "%H:%M:%S")      
        
        fecha<- paste0(fecha1,' ',fecha2)
        date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
        namesss[i]<-as.character(fecha)
      }
      
      
      date_string<- as.data.frame(date_string)
      namesss<- as.data.frame(namesss)
      
      pl_dt<- cbind(date_string, namesss, pl_dt$rain)
      colnames(pl_dt)<- c("s/since_1/1/1970", "date_string_hour","rain (mm)")
      
      Dataset_XX1<- pl_dt
      return(Dataset_XX1)
      
    }else{
      #################termo-higrometro
      
      th_lk<-"deviceid=032B8F08E708&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
      th_dt<- matrix(-31, ncol = 9, nrow = vpp*p)
      k<-1
      for (j in 1:p) {
        enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
        request2<- POST(enlace, encode = "form")
        request_list2<- htmlParse(request2, asText = TRUE)
        rr<-getNodeSet(request_list2, "//tbody")
        rr1<- xmlToDataFrame(rr)
        rr1<-t(rr1)
        rr2<-str_extract_all(rr1, boundary("word"))
        
        
        for (i in 1:length(rr2)) {
          month<- as.numeric(rr2[[i]][1])
          day<- as.numeric(rr2[[i]][2])
          year<- as.numeric(rr2[[i]][3])
          hora<- as.numeric(rr2[[i]][4])
          min<- as.numeric(rr2[[i]][5])
          sec<- as.numeric(rr2[[i]][6])
          merid<- as.character(rr2[[i]][7])
          temp<-as.numeric(rr2[[i]][8])
          humd<-as.numeric(rr2[[i]][10])
          
          
          xx<- cbind(month, day, year, hora, min, sec, merid, temp, humd)
          th_dt[k,]<- xx
          k<- k+1
        }
      }
      
      
      colnames(th_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "temp (ºC)", "humeda(%)")
      th_dt<- as.data.frame(th_dt)
      
      date_string<- matrix(-31, nrow  = length(th_dt[,1]))
      namesss<- matrix(-31, nrow = length(th_dt[,1]))
      for (i in 1: length(th_dt[,1])) {
        
        fecha1<- paste0( th_dt$Dia[i],'/',
                         th_dt$Mes[i],'/',
                         th_dt$anno[i])
        
        fecha2<-paste0(th_dt$Hora[i], ':',
                       th_dt$Min[i],':',
                       th_dt$Sec[i], ' ', th_dt$Merid[i] )
        
        fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                        format =  "%H:%M:%S")      
        
        fecha<- paste0(fecha1,' ',fecha2)
        date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
        namesss[i]<-as.character(fecha)
      }
      
      
      date_string<- as.data.frame(date_string)
      namesss<- as.data.frame(namesss)
      
      th_dt<- cbind(date_string, namesss, th_dt$temp, th_dt$hum)
      colnames(th_dt)<- c("s/since_1/1/1970", "date_string_hour","temp (ºC)","Humedad (%)")
      
      
      
      Dataset_XX2<- th_dt
      return(Dataset_XX2)
    }
    
    
  }
  
  
  
  
}### en principio esta funcion ya no la uso
clean_data<- function(dirtyframe, fechainicio){
  #Establecer fecha de inicio
  
  start_meas<-unclass(as.POSIXct(paste0(fechainicio," 00:00:00"),format="%d/%m/%Y %H:%M:%S"))
  start_meas<- start_meas[1]
  
  condit_U<- dirtyframe[,1]>start_meas
  cl_data<-as.data.frame(dirtyframe[condit_U, ])
  
  #Eliminar casos que contengan  NA's
  return(cl_data[complete.cases(cl_data), ])
  
}
anemos<-function(id){
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  U_dt<- matrix(-31, ncol = 10, nrow = vpp*p)
  k<-1
  enla1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  for (j in 1:p) {
    enlace<- paste0(enla1,"page=",j,"&deviceid=",id,link2)
    request2<- POST(enlace, encode = "form")
    request_list2<- htmlParse(request2, asText = TRUE)
    rr<-getNodeSet(request_list2, "//tbody")
    rr1<- xmlToDataFrame(rr)
    rr1<-t(rr1)
    rr2<-str_extract_all(rr1, boundary("word"))
    
    
    for (i in 1:length(rr2)) {
      month<- as.numeric(rr2[[i]][1])
      day<- as.numeric(rr2[[i]][2])
      year<- as.numeric(rr2[[i]][3])
      hora<- as.numeric(rr2[[i]][4])
      min<- as.numeric(rr2[[i]][5])
      sec<- as.numeric(rr2[[i]][6])
      merid<- as.character(rr2[[i]][7])
      wind<-as.numeric(rr2[[i]][8])
      gust<-as.numeric(rr2[[i]][11])
      dir<-paste(rr2[[i]][14],rr2[[i]][15], sep = "-")
      
      xx<- cbind(month, day, year, hora, min, sec, merid, wind, gust, dir)
      U_dt[k,]<- xx
      k<- k+1
    }
  }
  
  
  colnames(U_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "Wd mean", "wd gust", "Dir")
  U_dt<- as.data.frame(U_dt)
  
  
  
  date_string<- matrix(-31, nrow  = length(U_dt[,1]))
  namesss<- matrix(-31, nrow = length(U_dt[,1]))
  for (i in 1: length(U_dt[,1])) {
    
    fecha1<- paste0( U_dt$Dia[i],'/',
                     U_dt$Mes[i],'/',
                     U_dt$anno[i])
    
    fecha2<-paste0(U_dt$Hora[i], ':',
                   U_dt$Min[i],':',
                   U_dt$Sec[i], ' ', U_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  U_dt<- cbind(date_string, namesss, U_dt$`Wd mean`, U_dt$`wd gust`, U_dt$Dir)
  colnames(U_dt)<- c("s/since_1/1/1970", "date_string_hour","Mean","Max","Dir_ch")
  
  U_dt$Dir_ch<-str_remove(U_dt$Dir_ch, pattern = '-NA')
  dir_degrees_U<-matrix(-31, nrow = length(U_dt$Dir))
  
  for (i in 1:length(U_dt$Dir)) {
    yy<-U_dt$Dir[i]
    dir_degrees_U[i]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
                                                     ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                                                                       ifelse(yy=="East", 90, 
                                                                                              ifelse(yy=="East-southeast", 110,
                                                                                                     ifelse(yy=="Southeast", 135,
                                                                                                            ifelse(yy=="South-Southeast", 155, 
                                                                                                                   ifelse(yy=="South", 180, 
                                                                                                                          ifelse(yy=="South-southwest", 200, 
                                                                                                                                 ifelse(yy=="Southwest", 225,
                                                                                                                                        ifelse(yy=="West-southwest", 245, 
                                                                                                                                               ifelse(yy=="West", 270, 
                                                                                                                                                      ifelse(yy=="West-northwest",290,
                                                                                                                                                             ifelse(yy=="Northwest", 315,
                                                                                                                                                                    ifelse(yy=="North-northwest", 335, 
                                                                                                                                                                           ifelse(yy=='-31',NA)))))))))))))))))
  }
  dir_degrees_U<- as.data.frame( dir_degrees_U)
  U_dt<- cbind(U_dt, dir_degrees_U)
  colnames(U_dt)<- c("s/since_1/1/1970", "date_string_hour","Mean","Max","Dir_ch","Dir_deg")
  Dataset_XX<- list(U_dt)
  return(Dataset_XX)
  
  
}
pluvs<-function(id){
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  pl_dt<- matrix(-31, ncol = 8, nrow = vpp*p)
  k<-1
  for (j in 1:p) {
    enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
    request2<- POST(enlace, encode = "form")
    request_list2<- htmlParse(request2, asText = TRUE)
    rr<-getNodeSet(request_list2, "//tbody")
    rr1<- xmlToDataFrame(rr)
    rr1<-t(rr1)
    rr2<-str_extract_all(rr1, boundary("word"))
    
    
    for (i in 1:length(rr2)) {
      month<- as.numeric(rr2[[i]][1])
      day<- as.numeric(rr2[[i]][2])
      year<- as.numeric(rr2[[i]][3])
      hora<- as.numeric(rr2[[i]][4])
      min<- as.numeric(rr2[[i]][5])
      sec<- as.numeric(rr2[[i]][6])
      merid<- as.character(rr2[[i]][7])
      rain<-as.numeric(rr2[[i]][8])
      
      
      xx<- cbind(month, day, year, hora, min, sec, merid, rain)
      pl_dt[k,]<- xx
      k<- k+1
    }
  }
  
  
  colnames(pl_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "rain (mm)")
  pl_dt<- as.data.frame(pl_dt)
  
  date_string<- matrix(-31, nrow  = length(pl_dt[,1]))
  namesss<- matrix(-31, nrow = length(pl_dt[,1]))
  for (i in 1: length(pl_dt[,1])) {
    
    fecha1<- paste0( pl_dt$Dia[i],'/',
                     pl_dt$Mes[i],'/',
                     pl_dt$anno[i])
    
    fecha2<-paste0(pl_dt$Hora[i], ':',
                   pl_dt$Min[i],':',
                   pl_dt$Sec[i], ' ', pl_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  pl_dt<- cbind(date_string, namesss, pl_dt$rain)
  colnames(pl_dt)<- c("s/since_1/1/1970", "date_string_hour","rain (mm)")
  
  Dataset_XX1<- pl_dt
  return(Dataset_XX1)
  
}
term_hig<-function(id){
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  th_lk<-"deviceid=032B8F08E708&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  th_dt<- matrix(-31, ncol = 9, nrow = vpp*p)
  k<-1
  for (j in 1:p) {
    enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
    request2<- POST(enlace, encode = "form")
    request_list2<- htmlParse(request2, asText = TRUE)
    rr<-getNodeSet(request_list2, "//tbody")
    rr1<- xmlToDataFrame(rr)
    rr1<-t(rr1)
    rr2<-str_extract_all(rr1, boundary("word"))
    
    
    for (i in 1:length(rr2)) {
      month<- as.numeric(rr2[[i]][1])
      day<- as.numeric(rr2[[i]][2])
      year<- as.numeric(rr2[[i]][3])
      hora<- as.numeric(rr2[[i]][4])
      min<- as.numeric(rr2[[i]][5])
      sec<- as.numeric(rr2[[i]][6])
      merid<- as.character(rr2[[i]][7])
      temp<-as.numeric(rr2[[i]][8])
      humd<-as.numeric(rr2[[i]][10])
      
      
      xx<- cbind(month, day, year, hora, min, sec, merid, temp, humd)
      th_dt[k,]<- xx
      k<- k+1
    }
  }
  
  
  colnames(th_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "temp (ºC)", "humeda(%)")
  th_dt<- as.data.frame(th_dt)
  
  date_string<- matrix(-31, nrow  = length(th_dt[,1]))
  namesss<- matrix(-31, nrow = length(th_dt[,1]))
  for (i in 1: length(th_dt[,1])) {
    
    fecha1<- paste0( th_dt$Dia[i],'/',
                     th_dt$Mes[i],'/',
                     th_dt$anno[i])
    
    fecha2<-paste0(th_dt$Hora[i], ':',
                   th_dt$Min[i],':',
                   th_dt$Sec[i], ' ', th_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  th_dt<- cbind(date_string, namesss, th_dt$temp, th_dt$hum)
  colnames(th_dt)<- c("s/since_1/1/1970", "date_string_hour","temp (ºC)","Humedad (%)")
  
  
  
  Dataset_XX2<- th_dt
  return(Dataset_XX2)
}

get_anem <- possibly(anemos, otherwise = NA)
get_pluvs <- possibly(pluvs, otherwise = NA)
get_term_hig <- possibly(term_hig, otherwise = NA)

### función actualizadora


actualizar_anemos<-function(id,last_time){
  
  last_time<-last_time
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  U_dt<- matrix(-31, ncol = 10, nrow = vpp*p)
  k<-1
  enla1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  ll<-0
  for (j in 1:p) {
    if(ll==1){
      break
    }else{
      
      enlace<- paste0(enla1,"page=",j,"&deviceid=",id,link2)
      request2<- POST(enlace, encode = "form")
      request_list2<- htmlParse(request2, asText = TRUE)
      rr<-getNodeSet(request_list2, "//tbody")
      rr1<- xmlToDataFrame(rr)
      rr1<-t(rr1)
      rr2<-str_extract_all(rr1, boundary("word"))
      
      
      for (i in 1:length(rr2)) {
        month<- as.numeric(rr2[[i]][1])
        day<- as.numeric(rr2[[i]][2])
        year<- as.numeric(rr2[[i]][3])
        hora<- as.numeric(rr2[[i]][4])
        min<- as.numeric(rr2[[i]][5])
        sec<- as.numeric(rr2[[i]][6])
        merid<- as.character(rr2[[i]][7])
        wind<-as.numeric(rr2[[i]][8])
        gust<-as.numeric(rr2[[i]][11])
        dir<-paste(rr2[[i]][14],rr2[[i]][15], sep = "-")
        
        xx<- cbind(month, day, year, hora, min, sec, merid, wind, gust, dir)
        U_dt[k,]<- xx
        k<- k+1
        
        
        
        fecha1<- paste0( xx[2],'/',
                         xx[1],'/',
                         xx[3])
        
        fecha2<-paste0(xx[4], ':',
                       xx[5],':',
                       xx[6], ' ', xx[7] )
        
        fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                        format =  "%H:%M:%S")      
        
        fecha<- paste0(fecha1,' ',fecha2)
        l<-unclass(as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S"))[1]
        
        
        
        if(last_time==l){
          ll<-1
        }
        
        
        
      }
      
    }
    
  }
  
  
  colnames(U_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "Wd mean", "wd gust", "Dir")
  U_dt<- as.data.frame(U_dt)
  
  
  date_string<- matrix(-31, nrow  = length(U_dt[,1]))
  namesss<- matrix(-31, nrow = length(U_dt[,1]))
  for (i in 1: length(U_dt[,1])) {
    
    fecha1<- paste0( U_dt$Dia[i],'/',
                     U_dt$Mes[i],'/',
                     U_dt$anno[i])
    
    fecha2<-paste0(U_dt$Hora[i], ':',
                   U_dt$Min[i],':',
                   U_dt$Sec[i], ' ', U_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  U_dt<- cbind(date_string, namesss, U_dt$`Wd mean`, U_dt$`wd gust`, U_dt$Dir)
  colnames(U_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir_ch")
  
  U_dt$Dir_ch<-str_remove(U_dt$Dir_ch, pattern = '-NA')
  dir_degrees_U<-matrix(-31, nrow = length(U_dt$Dir))
  
  for (i in 1:length(U_dt$Dir)) {
    yy<-U_dt$Dir[i]
    dir_degrees_U[i]<- ifelse(yy=="North", 0, ifelse(yy=="North-northeast",20,
                                                     ifelse(yy=="Northeast",45, ifelse(yy=="East-northeast",65,
                                                                                       ifelse(yy=="East", 90, 
                                                                                              ifelse(yy=="East-southeast", 110,
                                                                                                     ifelse(yy=="Southeast", 135,
                                                                                                            ifelse(yy=="South-Southeast", 155, 
                                                                                                                   ifelse(yy=="South", 180, 
                                                                                                                          ifelse(yy=="South-southwest", 200, 
                                                                                                                                 ifelse(yy=="Southwest", 225,
                                                                                                                                        ifelse(yy=="West-southwest", 245, 
                                                                                                                                               ifelse(yy=="West", 270, 
                                                                                                                                                      ifelse(yy=="West-northwest",290,
                                                                                                                                                             ifelse(yy=="Northwest", 315,
                                                                                                                                                                    ifelse(yy=="North-northwest", 335, 
                                                                                                                                                                           ifelse(yy=='-31',NA)))))))))))))))))
  }
  dir_degrees_U<- as.data.frame( dir_degrees_U)
  U_dt<- cbind(U_dt, dir_degrees_U)
  U_dt<-U_dt[complete.cases(U_dt), ]
  
  colnames(U_dt)<- c("s/since_1/1/1970", "Date_hour","Mean","Max","Dir_ch","Dir_deg")
  Dataset_XX<- list(U_dt)
  return(Dataset_XX)
  
  
}
actualizar_pluvs<-function(id,last_time){
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  pl_dt<- matrix(-31, ncol = 8, nrow = vpp*p)
  k<-1
  ll<-0
  for (j in 1:p) {
    if(ll==1){
      break
    }else{
      enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
      request2<- POST(enlace, encode = "form")
      request_list2<- htmlParse(request2, asText = TRUE)
      rr<-getNodeSet(request_list2, "//tbody")
      rr1<- xmlToDataFrame(rr)
      rr1<-t(rr1)
      rr2<-str_extract_all(rr1, boundary("word"))
      
      
      for (i in 1:length(rr2)) {
        month<- as.numeric(rr2[[i]][1])
        day<- as.numeric(rr2[[i]][2])
        year<- as.numeric(rr2[[i]][3])
        hora<- as.numeric(rr2[[i]][4])
        min<- as.numeric(rr2[[i]][5])
        sec<- as.numeric(rr2[[i]][6])
        merid<- as.character(rr2[[i]][7])
        rain<-as.numeric(rr2[[i]][8])
        
        
        xx<- cbind(month, day, year, hora, min, sec, merid, rain)
        pl_dt[k,]<- xx
        k<- k+1
        
        
        fecha1<- paste0( xx[2],'/',
                         xx[1],'/',
                         xx[3])
        
        fecha2<-paste0(xx[4], ':',
                       xx[5],':',
                       xx[6], ' ', xx[7] )
        
        fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                        format =  "%H:%M:%S")      
        
        fecha<- paste0(fecha1,' ',fecha2)
        l<-unclass(as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S"))[1]
        
        
        
        if(last_time==l){
          ll<-1
        }
      }
    }
  }
  
  
  colnames(pl_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "rain (mm)")
  pl_dt<- as.data.frame(pl_dt)
  
  date_string<- matrix(-31, nrow  = length(pl_dt[,1]))
  namesss<- matrix(-31, nrow = length(pl_dt[,1]))
  for (i in 1: length(pl_dt[,1])) {
    
    fecha1<- paste0( pl_dt$Dia[i],'/',
                     pl_dt$Mes[i],'/',
                     pl_dt$anno[i])
    
    fecha2<-paste0(pl_dt$Hora[i], ':',
                   pl_dt$Min[i],':',
                   pl_dt$Sec[i], ' ', pl_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  
  
  pl_dt<- cbind(date_string, namesss, pl_dt$rain)
  pl_dt<-pl_dt[complete.cases(pl_dt), ]
  
  colnames(pl_dt)<- c("s/since_1/1/1970", "Date_hour","rain (mm)")
  
  Dataset_XX1<- pl_dt
  return(Dataset_XX1)
  
}
actualizar_term_hig<-function(id,last_time){
  link<- "https://measurements.mobile-alerts.eu/Home/MeasurementDetails?deviceid="
  link1<-"https://measurements.mobile-alerts.eu/Home/MeasurementDetails?"
  link2<- "&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  id<- id
  p1<- paste0(link,id,link2)
  p2<- POST(p1, encode = "form")
  p3<-htmlParse(p2, asText = TRUE)
  p4<-xpathApply(p3, "//a[@href]", xmlGetAttr, "href")
  pos<- length(p4)
  node_1<-as.character(p4[pos])
  node_2<-as.character(p4[pos-1])
  
  ##número de páginas
  if(is.na(str_extract(node_1,"(?<=page=)\\d\\d\\d"))){
    p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d"))
    if(is.na(p_1)){
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d"))
    }}else{
      p_1<-as.numeric(str_extract(node_1,"(?<=page=)\\d\\d\\d"))
    }
  
  if(is.na(str_extract(node_2,"(?<=page=)\\d\\d\\d"))){
    p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d"))
    if(is.na(p_2)){
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d"))
    }}else{
      p_2<-as.numeric(str_extract(node_2,"(?<=page=)\\d\\d\\d"))
    }
  
  
  if(is.na(p_2)){
    p<-p_1
  }else{
    if(p_2>p_1) {
      p<-p_2
    }else{
      p<-p_1
    }
  }
  
  if(is.na(p)){
    p<-1
  }
  
  
  vpp<-length(str_extract_all(t(xmlToDataFrame(getNodeSet(p3,
                                                          "//tbody"))),
                              boundary("word")))
  th_lk<-"deviceid=032B8F08E708&vendorid=f193c634-2611-475b-ba7a-27b0ead33c6f&appbundle=eu.mobile_alerts.mobilealerts"
  th_dt<- matrix(-31, ncol = 9, nrow = vpp*p)
  k<-1
  ll<-0
  for (j in 1:p) {
    if(ll==1){
      break
    }else{
      enlace<- paste0(link1,"page=",j,"&deviceid=",id,link2)
      request2<- POST(enlace, encode = "form")
      request_list2<- htmlParse(request2, asText = TRUE)
      rr<-getNodeSet(request_list2, "//tbody")
      rr1<- xmlToDataFrame(rr)
      rr1<-t(rr1)
      rr2<-str_extract_all(rr1, boundary("word"))
      
      
      for (i in 1:length(rr2)) {
        month<- as.numeric(rr2[[i]][1])
        day<- as.numeric(rr2[[i]][2])
        year<- as.numeric(rr2[[i]][3])
        hora<- as.numeric(rr2[[i]][4])
        min<- as.numeric(rr2[[i]][5])
        sec<- as.numeric(rr2[[i]][6])
        merid<- as.character(rr2[[i]][7])
        temp<-as.numeric(rr2[[i]][8])
        humd<-as.numeric(rr2[[i]][10])
        
        
        xx<- cbind(month, day, year, hora, min, sec, merid, temp, humd)
        th_dt[k,]<- xx
        k<- k+1
        
        
        fecha1<- paste0( xx[2],'/',
                         xx[1],'/',
                         xx[3])
        
        fecha2<-paste0(xx[4], ':',
                       xx[5],':',
                       xx[6], ' ', xx[7] )
        
        fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                        format =  "%H:%M:%S")      
        
        fecha<- paste0(fecha1,' ',fecha2)
        l<-unclass(as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S"))[1]
        
        
        
        if(last_time==l){
          ll<-1
        }
      }
    }
  }
  
  
  colnames(th_dt)<- c("Mes", "Dia", "anno", "Hora", "Min", "Sec","Merid", "temp (ºC)", "humeda(%)")
  th_dt<- as.data.frame(th_dt)
  
  date_string<- matrix(-31, nrow  = length(th_dt[,1]))
  namesss<- matrix(-31, nrow = length(th_dt[,1]))
  for (i in 1: length(th_dt[,1])) {
    
    fecha1<- paste0( th_dt$Dia[i],'/',
                     th_dt$Mes[i],'/',
                     th_dt$anno[i])
    
    fecha2<-paste0(th_dt$Hora[i], ':',
                   th_dt$Min[i],':',
                   th_dt$Sec[i], ' ', th_dt$Merid[i] )
    
    fecha2<- format(strptime(fecha2,"%I:%M:%S %p"),
                    format =  "%H:%M:%S")      
    
    fecha<- paste0(fecha1,' ',fecha2)
    date_string[i]<-as.POSIXct(fecha,format="%d/%m/%Y %H:%M:%S")
    namesss[i]<-as.character(fecha)
  }
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  
  date_string<- as.data.frame(date_string)
  namesss<- as.data.frame(namesss)
  
  th_dt<- cbind(date_string, namesss, th_dt$temp, th_dt$hum)
  th_dt<-th_dt[complete.cases(th_dt), ]
  
  colnames(th_dt)<- c("s/since_1/1/1970", "Date_hour","temp (ºC)","Humedad (%)")
  
  
  
  Dataset_XX2<- th_dt
  return(Dataset_XX2)
}
