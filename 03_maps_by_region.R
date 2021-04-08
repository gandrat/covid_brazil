#Maps by Regiões Imediatas do IBGE

packages<-c('ggplot2','dplyr','sf')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
rm(list=ls()) ## Removendo as variáveis

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=14))
)

#Load COVID data--------
load('input_data/cv_data.Rda')
sum(cv_today$deaths)

#Load Cities data
load('input_data/cities.Rda')



#Load Shapefiles--------
state<-read_sf('shapes/states.shp')
rgi<-read_sf('shapes/RG2017_rgi.shp')
rgi<-rgi%>%transmute(rgi_code=as.numeric(rgi))

cities_pt<-read_sf('shapes/municipios_pt.shp')
head(cities_pt)
cities_pt<-cities_pt%>%transmute(city_code=codmun2)

rgint<-read_sf('shapes/RG2017_rgint.shp')
rgint<-rgint%>%transmute(rgint_code=as.numeric(rgint))


#RGINT-------------
cv_today<-merge(cv_today,cities%>%select(city_code, rgint_code, rgint, rgi_code, rgi),by='city_code')
cv_city_today<-merge(cities_pt,cv_today,by='city_code')

cv_rgint_today<-cv_today%>%group_by(rgint_code, rgint,  state)%>%
  summarise(cases=sum(cases),
            deaths=sum(deaths),
            pop=sum(pop),
            deaths100k=sum(deaths)*100000/sum(pop))
rgint<-merge(rgint,cv_rgint_today,by='rgint_code')


#RGI
cv_rgi_today<-cv_today%>%group_by(rgi_code, rgi, state)%>%
  summarise(cases=sum(cases),
            deaths=sum(deaths),
            pop=sum(pop),
            deaths100k=sum(deaths)*100000/sum(pop))

rgi<-merge(rgi,cv_rgi_today,by='rgi_code')

#Brazil Map------------

ggplot()+
  geom_sf(data=rgint,aes(fill=deaths100k),size=.1)+
  geom_sf(data=state,fill=alpha("red",0.0))+
  scale_fill_distiller(palette='Oranges',direction = 1)+
  # coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
  theme(legend.position = 'bottom')
ggsave('figures/bra_rgint_deaths.jpg', width=20, height=20, units='cm',dpi=300)



#Maps by states------------

rgi$state<-replace(x=rgi$state, which(rgi$state=='DF'),'GO')
cv_city_today$state<-replace(x=cv_city_today$state, which(cv_city_today$state=='DF'),'GO')


for(s in unique(rgi$state)){
  d<-rgi%>%filter(state==sprintf(s,'%s'))
  c<-cv_city_today%>%filter(state==sprintf(s,'%s'))
  
  bbox<-st_bbox(d)
  
  ggplot(state)+
    geom_sf()+
    geom_sf(data=d,aes(fill=deaths100k),size=.1)+
    geom_sf(data=c,aes(size=deaths100k),alpha=.1)+
    scale_fill_distiller(palette='Oranges',direction = 1)+
    coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
    labs(size='Deaths/100k (Cities)',fill='Deaths/100k (RGI)')+
    ggtitle(sprintf(s,'%s'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_map_deaths.jpg'), width=20, height=20, units='cm',dpi=300)
}

#Maps by cities-------------
city_sf<-read_sf('input_data/BRMUE250GC_SIR.shp')
names(city_sf)<-c('city','city_code','geometry')
city_sf<-city_sf%>%select(city_code)

cv_city_sf<-cv_city_today
cv_city_sf$geometry<-NULL

cv_city_sf<-merge(city_sf,cv_city_sf,by='city_code')

s<-'RS'
for(s in unique(cv_city_sf$state)){
  d<-cv_city_sf%>%filter(state==sprintf(s,'%s'))
  
  bbox<-st_bbox(d)
  
  ggplot(state)+
    geom_sf()+
    geom_sf(data=d,aes(fill=deaths100k),size=.1)+
    scale_fill_distiller(palette='Oranges',direction = 1)+
    coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
    labs(size='Deaths/100k (Cities)',fill='Deaths/100k (City)')+
    ggtitle(sprintf(s,'%s'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_map_deaths_city.jpg'), width=20, height=20, units='cm',dpi=300)
}


#Maps for DF
rmb<-c('BRASÍLIA','ÁGUAS LINDAS DE GOIÁS','ALEXÂNIA', 'CIDADE OCIDENTAL','COCALZINHO DE GOIÁS','CRISTALINA','FORMOSA','LUZIÂNIA','NOVO GAMA', 'PADRE BERNARDO','PLANALTINA','SANTO ANTÔNIO DO DESCOBERTO','VALPARAÍSO DE GOIÁS')
cv_city_df<-cv_city_sf%>%filter(city %in% rmb)

bbox<-st_bbox(cv_city_df)

ggplot(state)+
  geom_sf()+
  geom_sf(data=cv_city_df,aes(fill=deaths100k),size=.1)+
  scale_fill_distiller(palette='Oranges',direction = 1)+
  coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
  labs(size='Deaths/100k (Cities)',fill='Deaths/100k (City)')+
  ggtitle('Região Metropolitana de Brasília')
ggsave('figures/DF_map_deaths_city.jpg', width=20, height=20, units='cm',dpi=300)


          