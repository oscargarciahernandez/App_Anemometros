library(here)
source('NUEVO/Libraries.R')


txt_files<- here::here('GET_DATA/') %>% list.files(full.names = T)

lista_data<- list()
for(i in 1:15){
  
  x<- txt_files[i]
  
  info<- x %>% str_split("/") %>% .[[1]] %>% .[length(.)] %>% 
    str_split("_") %>% .[[1]]
  pot<- info[1]
  angle<- info[2]
  table<- x %>%  read.table(sep = "\n") %>% as.vector()
  table<- as.numeric(gsub('[a-zA-Z]', '', table$V1))
  table<- table %>% .[complete.cases(.)] 
  table_total<- cbind(table, 
        rep(pot, length(table)),
        rep(angle, length(table))) %>% as.data.frame()
  colnames(table_total)<- c('rpm', 'pot', 'angle')
  lista_data[[i]]<- table_total
}

tabla_merge<- lista_data %>% bind_rows()
tabla_merge<- tabla_merge %>% group_split(pot,angle) %>% lapply(function(y){
  y$n<- seq(1,nrow(y))
  return(y)
}) %>% bind_rows()


tabla_merge$rpm <- tabla_merge$rpm %>% as.numeric()
tabla_merge %>% group_split(pot) %>% lapply(function(x) x%>% ggplot()+
  geom_point(aes(x=n, y=rpm,colour= angle), size= 0.5, alpha= 0.3) + 
  geom_line(aes(x=n, y=SMA(rpm, 20),colour= angle)) + 
  
  theme_light())
