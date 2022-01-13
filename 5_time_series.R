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
load('input_data/cities.Rda')
rm(cv_cases,cv_cases_state,cv_cases_state_week,cv_today,cv_today_state)

#Filtrando os estados

cvw<-cv_cases_week%>%filter(state %in% c('BA','RN','PA','MA'))

cvw<-merge(cvw,cities%>%select(city_code, rgint_code, rgint, rgi_code, rgi, regsaude, nome_reg),by='city_code')

cvw<-cvw%>%arrange(date)

unique(cvw$date)
#Filtrando datas selecionadas
cvw_BA<-cvw%>%filter(as.character(date) %in% c("2020-08-22","2020-11-14","2021-03-20","2021-05-01"))

cvw_RN<-cvw%>%filter(as.character(date) %in% c("2020-03-28","2020-04-25","2020-05-30","2020-06-27","2020-07-25","2020-08-29","2020-09-26",
                                               "2020-10-31","2020-11-28","2020-12-26","2021-01-30","2021-02-27","2021-03-27","2021-05-01"))

cvw_PA<-cvw%>%filter(as.character(date) %in% c("2021-03-06","2021-04-03","2021-05-01",
                                               "2020-04-04","2020-05-02","2020-06-06"),state=='PA')

cvw_MA<-cvw%>%filter(as.character(date) %in% c("2020-04-04","2020-04-11","2020-04-18","2020-04-25",
                                               "2020-12-26","2021-01-09","2021-02-20","2021-03-20"),state=='MA')

unique(cvw_MA$date)
#Shape RGINT-----------
rgint<-read_sf('shapes/RG2017_rgint2.shp')
rgint<-rgint%>%transmute(rgint_code=as.numeric(rgint))

rgint2<-cities%>%group_by(rgint_code)%>%summarise(pop=sum(pop))

rgint<-merge(rgint,rgint2,by='rgint_code')

cv_rgint_PA<-cvw_PA%>%group_by(rgint_code, rgint,  state, date)%>%
  summarise(cases=sum(cases),
            deaths=sum(deaths))

cv_rgint_PA<-merge(cv_rgint_PA,rgint,by='rgint_code')

cv_rgint_PA<-cv_rgint_PA%>%mutate(cases100k=cases*100000/pop,
                                  cases100k=cases*100000/pop)

View(cv_rgint_PA%>%select(-geometry))

write_sf(cv_rgint_PA,dsn='output_data/cv_rgint_PA.shp',delete_layer=T,layer_options = "ENCODING=UTF-8")


#Shape Muni--------------
city_sf<-read_sf('shapes/BR_Municipios_2020.shp')
names(city_sf)<-c('city_code','city','state','area','geometry')
city_sf<-city_sf%>%select(city_code,state)
city_sf$city_code<-as.integer(city_sf$city_code)

cvw_BA<-merge(cvw_BA,city_sf%>%filter(state=='BA'),by=c('city_code','state'),all = T)
cvw_RN<-merge(cvw_RN,city_sf%>%filter(state=='RN'),by=c('city_code','state'),all = T)
cvw_MA<-merge(cvw_MA,city_sf%>%filter(state=='MA'),by=c('city_code','state'),all = T)



cvw_RN<-cvw_RN%>%mutate(deaths100k=deaths*100000/pop)
cvw_RN<-cvw_RN%>%mutate(cases100k=cases*100000/pop)

cvw_BA<-cvw_BA%>%mutate(deaths100k=deaths*100000/pop)
cvw_BA<-cvw_BA%>%mutate(cases100k=cases*100000/pop)

cvw_MA<-cvw_MA%>%mutate(deaths100k=deaths*100000/pop)
cvw_MA<-cvw_MA%>%mutate(cases100k=cases*100000/pop)

write_sf(cvw_RN,dsn='output_data/cv_city_RN.shp',layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
write_sf(cvw_BA,dsn='output_data/cv_city_BA.shp',layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
write_sf(cvw_MA,dsn='output_data/cv_city_MA.shp',layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
