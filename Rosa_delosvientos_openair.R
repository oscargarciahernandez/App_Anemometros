library(openair)
prueba<-cbind(as.POSIXct(clean_data_list[[1]][,2]),clean_data_list[[1]][,c(3,6)])
names(prueba)<- c("date","ws","wd")

windRose(prueba,ws.int = 0.5,angle = 22.5,
         breaks = 8,paddle = F, annotate = F,
         key.position = "right", statistic = "prop.mean")
