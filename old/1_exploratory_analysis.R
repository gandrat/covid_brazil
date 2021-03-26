

gc()

packages<-c('ggplot2','dplyr')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

rm(list=ls()) ## Removendo as variáveis

#Load COVID data--------
load('input_data/cv_data.Rda')

#Load Cities data
load('input_data/cities.Rda')

#select and rename variables
cities<-cities%>%transmute(city_code=cod_mun,
                           micro_code=Cod_micro,
                           micro=Nome_da_micro,
                           rm_code=Cod_RM,
                           rm=Nome_da_RM,
                           meso_code=Cod_meso,
                           meso=Nome_da_meso,
                           regic=hierar,
                           regic_class=regic_class,
                           regiao=regiao,
                           renda=renda,
                           pib=pib,
                           pop2010=pop,
                           popurb=popurb,
                           idhm=idhm,
                           idhrenda=idhrenda,
                           idhlonge=idhlonge,
                           idhedu=idhedu,
                           lat=lat,
                           lon=lon)
unique(cv_cases$city_code,cv_cases$pop2019)
cities<-merge(cities,cv_cases%>%select(city_code,pop2019),by='city_code',all.x = T)
#Get some indicators
max(cv_cases$date)
sum(cv_today$deaths)
sum(cv_today$pop2019)
max(cv_cases$new_cases)
min(cv_cases$new_cases)
sum(cv_cases$new_deaths,na.rm = T)

cv_cases<-cv_cases%>%mutate(new_cases=replace(new_cases,new_cases<0,NA),
                             new_deaths=replace(new_deaths,new_deaths<0,NA))%>%
  as.data.frame()

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
  theme(text=element_text(family="Times"))
)


cvw<-cv_cases_week%>%select(city_code,state, date,week, cases, deaths, new_cases, new_deaths, pop2019)

cvw<-merge(cvw,cities%>%select(city_code,meso_code,meso), by='city_code')


cv_meso<-cvw%>%group_by(meso_code, meso, week)%>%
  summarise(meso_code=min(meso_code), meso=min(meso), state=min(state),
            week=min(week),
            date=min(date),
            cases=sum(cases, na.rm=T),
            cases100k=sum(cases,na.rm=T)/sum(pop2019,na.rm=T),
            deaths=sum(deaths, na.rm=T),
            deaths100k=sum(deaths,na.rm=T)/sum(pop2019,na.rm=T),
            new_cases=sum(cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T),
            pop=sum(pop2019, na.rm=T))

cv_bra<-cvw%>%group_by(week)%>%summarise(date=min(date),
                                         week=min(week,na.rm = T),
                                         new_cases=sum(new_cases, na.rm=T),
                                         new_deaths=sum(new_deaths,na.rm=T))

sum()


ggplot(cv_meso%>%filter(state=='RS'),aes(x=week,y=deaths100k))+
  geom_bar(stat='identity',fill='grey')+
  facet_wrap(~meso)+
  geom_path(data=cvbra,aes(x=week,y=deaths100k,group=1, inherit.aes=F),size=.2)+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab(NULL)


ggplot(cvbra,aes(x=weeky,y=deaths100k,group=1))+geom_path()

