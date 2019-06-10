library(RCurl)
library(request)
library(XML)
library(rvest)
library(stringr)

# CHANGE USER-AGENT -------------------------------------------
'
USER_AGENTS <- read_html("http://www.useragentstring.com/pages/useragentstring.php?typ=Browser") %>%
  html_nodes( "li") %>% html_text()
'
#DOWNLOAD DATA
bdown=function(url, file){
  
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}



# DESCARGA DE GRIBS -------------------------------------------------------
url_gfs_http<- "https://www.ncei.noaa.gov/thredds/catalog/gfs-004-files/catalog.html"
CARPETAS_DISPONIBLES<- url_gfs_http %>% GET() %>% htmlParse() %>% readHTMLTable() %>% .[[1]] %>% 
  .[3:nrow(.),1] %>% str_extract("[:digit:]{6}")

URLS_CARPETAS_MES<- CARPETAS_DISPONIBLES %>% 
  paste0(url_gfs_http %>%
           str_remove("catalog.html"),., "/catalog.html")

library(parallel)

FUNCTION_DOWNLOAD_GRIB<- function(URLS_CARPETAS_MES){
  for (i in 1:length(URLS_CARPETAS_MES)) {
    
    DIAS_DISPONIBLES<- URLS_CARPETAS_MES[i] %>% GET()%>% htmlParse() %>% readHTMLTable() %>% 
      .[[1]] %>% 
      .[2:nrow(.),1] %>% 
      str_extract("[:digit:]{8}")
    
    URLS_DIAS<- DIAS_DISPONIBLES %>% paste0(URLS_CARPETAS_MES[i] %>%
                                              str_remove("catalog.html"),., "/catalog.html")
    
    for (dias in 1:length(URLS_DIAS)) {
      
      ARCHIVOS_DISPONIBLES<- URLS_DIAS[dias] %>% GET()%>% htmlParse() %>% readHTMLTable() %>% 
        .[[1]] %>% .[2:nrow(.),1] %>% str_remove("\r\nGFS Grid 4  ") %>% str_trim()
      
      ARCHIVOS_HASTA_48<- ARCHIVOS_DISPONIBLES %>%
        .[str_detect(.,"00:00")] %>% 
        .[(length(.)-16):length(.)]%>% 
        sapply(function(x){
          x %>%  
            str_replace_all("-", "") %>% 
            str_replace_all(" ", "_") %>% 
            str_remove("UTC_fct:") %>% str_replace(":","")}) %>% paste0("gfs_4_",.)
      
      
      URLS_DESCARGA<- paste0("https://www.ncei.noaa.gov/thredds/fileServer/gfs-004-files/",
                             CARPETAS_DISPONIBLES[i],"/",
                             DIAS_DISPONIBLES[dias], "/",
                             ARCHIVOS_HASTA_48)
      
      
      path_gribs<- paste0(here::here('Gribs/'), DIAS_DISPONIBLES[dias])
      
      for (j in 1:length(URLS_DESCARGA)) {
        if(!dir.exists(path_gribs)){
          dir.create(path_gribs, recursive = T)}else{cat(paste0("Gribs para ",DIAS_DISPONIBLES[dias], " ya descargados\n"))}
        
        
        bdown(URLS_DESCARGA[j], 
              file = paste0(path_gribs,"/",
                            ARCHIVOS_HASTA_48[j]))
        Sys.sleep(0.5)
        
      }
      
    }
  }
  
}


mclapply(URLS_CARPETAS_MES[1:2],
         FUNCTION_DOWNLOAD_GRIB,
         mc.cores =  getOption("mc.cores", 10L))



# RENAME GRIBS TO FIT WRF -------------------------------------------------


######FORMATO NAME
###  QUIERO:  19043000.gfs.t00z.0p50.pgrb2f000
###  TENGO:    gfs_4_20190504_0000_000.grb2

#DIA+PARTE_FIJA+HORA


NOMBRE_PARTE_FIJA<- ".gfs.t00z.0p50.pgrb2f"



###PONGO DOS MANERAS DE HACER LO DEL NOMBRE PORQUE ME EQUIVOQUE Y TUVE QUE CAMBIAR EL NOBMRE
#DE LOS ARCHIVOS DE 2 PASOS... PARA LA SIGUIENTE VEZ... HAY QUE UNIFICAR ESTOS DOS PASOS EN 1
# ESO YA QUEDA PARA LA SIGUIENTE VEZ... ME DA PEREZA AHORA MISMO.

Horas<- list.files(path_gribs, full.name=T) %>% str_extract("[[:digit:]]{3}.grb2") %>% str_remove(".grb2")
'
Horas<- list.files(path_gribs, full.name=T) %>% str_extract("pgrb2f.{3}") %>% str_remove("pgrb2f")
'

DIA<- list.files(path_gribs, full.name=T) %>% str_extract("gfs_4_[[:digit:]]{8}") %>% str_remove("gfs_4_")

'
DIA<- list.files(path_gribs, full.name=T) %>% str_split("/") %>% sapply(function(x) x[length(x)]) %>% 
  str_remove(".{2}") %>% paste0(.,"00")
'


new_name<- paste0(DIA, NOMBRE_PARTE_FIJA, Horas)

file.rename(from =list.files(path_gribs, full.name=T), to= paste0(path_gribs, new_name) )


