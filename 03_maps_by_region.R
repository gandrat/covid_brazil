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
    theme(text=element_text(family="Times"))
)

#Load COVID data--------
load('input_data/cv_data.Rda')

#Load Cities data
load('input_data/cities.Rda')

#Load Shapefiles--------
state<-read_sf('shapes/states.shp')
rgi<-read_sf('shapes/RG2017_rgi.shp')
rgi<-rgi%>%transmute(rgi_code=as.numeric(rgi))

rgint<-read_sf('shapes/RG2017_rgint.shp')
rgint<-rgint%>%transmute(rgint_code=as.numeric(rgint))

#RGINT
cv_today<-merge(cv_today,cities%>%select(city_code, rgint_code, rgint, rgi_code, rgi),by='city_code')
sum(cv_today$deaths)

cv_rgint_today<-cv_today%>%group_by(rgint_code, rgint,  state)%>%
  summarise(cases=sum(cases),
            deaths=sum(deaths),
            pop=sum(pop),
            deaths100k=sum(deaths)*100000/sum(pop))
rgint<-merge(rgint,cv_rgint_today,by='rgint_code')

ggplot(state)+
  geom_sf()+
  geom_sf(data=rgint,aes(fill=deaths100k),size=.1)+
  scale_fill_distiller(palette='Oranges',direction = 1)+
  # coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
  theme(legend.position = 'bottom')
ggsave('figures/bra_rgint_deaths.jpg', width=20, height=20, units='cm',dpi=300)



#RGI
cv_rgi_today<-cv_today%>%group_by(rgi_code, rgi, state)%>%
  summarise(cases=sum(cases),
            deaths=sum(deaths),
            pop=sum(pop),
            deaths100k=sum(deaths)*100000/sum(pop))

rgi<-merge(rgi,cv_rgi_today,by='rgi_code')

rgi$state<-replace(x=rgi$state, which(rgi$state=='DF'),'GO')
for(s in unique(rgi$state)){
d<-rgi%>%filter(state==sprintf(s,'%s'))
bbox<-st_bbox(d)
  ggplot(state)+
    geom_sf()+
    geom_sf(data=d,aes(fill=deaths100k),size=.1)+
    scale_fill_distiller(palette='Oranges',direction = 1)+
    coord_sf(xlim = c(bbox[1]-.2, bbox[3]+.2), ylim = c(bbox[2]-.2, bbox[4]+.2), expand = FALSE)+
    theme(legend.position = 'bottom')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_map_deaths.jpg'), width=15, height=20, units='cm',dpi=300)
}

          