library(RCurl)
library(request)
library(XML)
library(rvest)
library(stringr)

# CHANGE USER-AGENT -------------------------------------------

USER_AGENTS <- read_html("http://www.useragentstring.com/pages/useragentstring.php?typ=Browser") %>%
  html_nodes( "li") %>% html_text()

#DOWNLOAD DATA
bdown=function(url, file){
  
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}



# DESCARGA DE GRIBS -------------------------------------------------------
url_gfs_http<- "https://nomads.ncdc.noaa.gov/data/gfs4/"
Tabla_gfs<- url_gfs_http %>% GET() %>% htmlParse() %>% readHTMLTable() %>% .[[1]] %>% 
  .[3:nrow(.),]

CARPETAS_DISPONIBLES<- Tabla_gfs[Tabla_gfs$Name %>% as.character()%>%
            str_detect("[:digit:]{5}"),]$Name %>% 
  na.omit() %>% 
  as.character() 

URLS_CARPETAS<- CARPETAS_DISPONIBLES %>% paste0(url_gfs_http,.)

for (i in 1:length(CARPETAS_DISPONIBLES)) {
  
  CONTENIDO_CARPETAS<- URLS_CARPETAS[i] %>% 
    GET(add_headers("user-agent" = USER_AGENTS[i])) %>%
    htmlParse() %>% readHTMLTable() %>% .[[1]] %>% 
    .[3:nrow(.),]  %>% 
    .[.$Name %>% as.character()%>%
        str_detect("[:digit:]{6}"),"Name"] %>% 
    na.omit() %>% 
    as.character() 
  
  URLS_SUBCARPETAS<- CONTENIDO_CARPETAS %>% paste0(URLS_CARPETAS[i],.)
  
  
  if(length(CONTENIDO_CARPETAS)==0){cat(URLS_CARPETAS[i], " VACIO")}else{
    
    #SOLO KEREMOS LOS GFS DE 48 h
    ALL_ARCH<- URLS_SUBCARPETAS[i] %>% 
      GET(add_headers("user-agent" = USER_AGENTS[i+1])) %>%
      htmlParse() %>% readHTMLTable() %>% .[[1]] %>% 
      .[3:nrow(.),] 
    
    Gribs_hasta48<- ALL_ARCH %>% .$Name  %>%
      str_extract("[[:digit:]]{3}.grb2")  %>% 
      str_remove(".grb2") %>% 
      as.numeric()<50 
    
    ARCHIVOS_GRIB<- ALL_ARCH$Name[Gribs_hasta48] %>% na.omit() %>% 
      as.character() 
    
    
    SUBGRIB_FOLDER<- URLS_SUBCARPETAS[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)-1]
    
    path_gribs<- paste0(here::here('Gribs/'), SUBGRIB_FOLDER,"/")
    if(!dir.exists(path_gribs)){dir.create(path_gribs, recursive = T)}
    
    
    for (j in 1:length(ARCHIVOS_GRIB)) {
      bdown(paste0(URLS_SUBCARPETAS[j],
                   ARCHIVOS_GRIB), 
            file = paste0(path_gribs, 
                          ARCHIVOS_GRIB[i]))
      Sys.sleep(100)
      
    }
  }
}





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


