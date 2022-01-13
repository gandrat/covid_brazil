#Mapas temporais para BH e RN

packages<-c('dplyr','sf')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
rm(list=ls()) ## Removendo as variáveis

#Load COVID data--------
load('input_data/cv_data.Rda')
rm(cv_cases,cv_cases_state,cv_cases_state_week,cv_today,cv_today_state)

#Filtrando os estados

cvw<-cv_cases_week%>%filter(state %in% c('BA','RN'))
max(cvw$date)
unique(cvw$date)

#Filtrando datas selecionadas
cvw_BA<-cvw%>%filter(as.character(date) %in% c("2020-08-22","2020-11-14","2021-03-20","2021-05-01"))

cvw_RN<-cvw%>%filter(as.character(date) %in% c("2020-03-28","2020-04-25","2020-05-30","2020-06-27","2020-07-25","2020-08-29","2020-09-26",
                                               "2020-10-31","2020-11-28","2020-12-26","2021-01-30","2021-02-27","2021-03-27","2021-05-01"))

#Agregando informações de municipaios
city_sf<-read_sf('shapes/BR_Municipios_2020.shp')
names(city_sf)<-c('city_code','city','state','area','geometry')
city_sf<-city_sf%>%select(city_code,state)
city_sf$city_code<-as.integer(city_sf$city_code)

cvw_BA<-merge(cvw_BA,city_sf%>%filter(state=='BA'),by=c('city_code','state'),all = T)
cvw_RN<-merge(cvw_RN,city_sf%>%filter(state=='RN'),by=c('city_code','state'),all = T)

hist(cvw_RN$deaths100k)
cvw_RN<-cvw_RN%>%mutate(deaths100k=deaths*100000/pop)
cvw_RN<-cvw_RN%>%mutate(cases100k=cases*100000/pop)

cvw_BA<-cvw_BA%>%mutate(deaths100k=deaths*100000/pop)
cvw_BA<-cvw_BA%>%mutate(cases100k=cases*100000/pop)


write_sf(cvw_RN,dsn='output_data/cv_city_RN.shp',layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
write_sf(cvw_BA,dsn='output_data/cv_city_BA.shp',layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
