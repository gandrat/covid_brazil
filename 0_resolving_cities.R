require(sf)
require(dplyr)

#Load Cities data
load('input_data/cities.Rda')

save(cit,file='input_data/cities2.Rda')

reg<-read_sf('shapes/regsaude.shp')

citiesp = st_as_sf(cities, coords = c("lon", "lat"), 
                 crs = 4326, agr='constant')

write_sf(citiesp,dsn='output_data/cities_point.shp')
citiespr<-st_join(citiesp,reg%>%select(regsaude))

reg<-st_set_crs(reg,st_crs(citiesp))
plot(citiesp)


############
cities<-read_sf('shapes/cities_pt.shp')
cities$geometry<-NULL

cit2<-read_ods('input_data/cities.ods')

names(cities)<-names(cit2)
cit<-rbind(cit2,cities)
cities<-cit
save(cities,file='input_data/cities2.Rda')
