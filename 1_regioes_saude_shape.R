#Merging cities with Health Regions

library(sf)
library(dplyr)

rm(list=ls()) ## Removendo as variáveis

cities_sf<-read_sf('shapes/BRMUE250GC_SIR.shp')
names(cities_sf)<-c('city','city_code','geometry')

#Relationship table
tbcity<-read.csv2('input_data/tb_municip.csv')
tbregsaude<-read.csv2('input_data/tb_regsaud.csv',sep=',')

rel<-read.csv('input_data/rl_municip_regsaud.csv')
names(rel)<-c('co_municip','regsaude')

tbcity<-merge(tbcity,rel,by='co_municip')

tbcity<-merge(tbcity,tbregsaude,by='regsaude')
cit<-cities_sf
cit$geometry<-NULL

cities_sf<-merge(cities_sf,tbcity,by='city_code')


#Merge polygons-------------
regsaude<-cities_sf%>%
  group_by(regsaude)%>%
  summarise(city_code=first(city_code),
            city=first(city.x))
write_sf(regsaude,'shapes/regsaude.shp')

plot(regsaude)

#Aggregating regsaude in cities table (input for COVID data)------------
# load('input_data/cities.Rda')
# cities<-merge(cities%>%select(-regsaude),tbcity%>%select(city_code,regsaude,nome_reg),by='city_code',all.x=T)
# save(cities,file='input_data/cities.Rda')
