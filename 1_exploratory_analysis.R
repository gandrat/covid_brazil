

gc()

packages<-c('ggplot2','dplyr',)
library(readODS)
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

rm(list=ls()) ## Removendo as variáveis

#Load COVID data--------
load('input_data/cv_data_2021-03-20.Rda')
cv_cases$week<-as.factor(cv_cases$week)
#Load Cities data
load('input_data/cities.Rda')


#Get some indicators
max(cv_cases$date)
sum(cv_today$deaths)
max(cv_cases$new_cases)
min(cv_cases$new_cases)
sum(cv_cases$new_deaths,na.rm = T)

#Some new_cases and new_deaths data happens when the SE move data from one city to another
# cv_cases<-cv_cases%>%mutate(new_cases=replace(new_cases,new_cases<0,NA),
#                             new_deaths=replace(new_deaths,new_deaths<0,NA))%>%
#   as.data.frame()

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"))
)


cvw<-cv_cases_week%>%select(city_code,state, date,week, cases, deaths, new_cases, new_deaths, pop)

cvw<-merge(cvw,cities%>%select(city_code,rgint_code,rgint,rgi_code,rgi), by='city_code')

#Plots by rgint-----------
cv_rgint<-cvw%>%group_by(rgint_code, rgint, week)%>%
  summarise(rgint_code=min(rgint_code), rgint=min(rgint), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T),
            pop=sum(pop, na.rm=T))
cv_rgint<-cv_rgint%>%mutate(deaths100k=new_deaths*100000/pop,
                            cases100k=new_cases*100000/pop)


cv_bra<-cvw%>%group_by(week)%>%summarise(date=min(date),
                                         week=as.factor(min(week,na.rm = T)),
                                         new_cases=sum(new_cases, na.rm=T),
                                         new_deaths=sum(new_deaths,na.rm=T))

cv_bra$pop=sum(cities$pop)
cv_bra<-cv_bra%>%mutate(cases100k=new_cases*100000/pop,
                           deaths100k=new_deaths*100000/pop)


for(s in unique(cv_rgint$state)){
  ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Deaths per 100k')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgint.jpg'), width=16, height=18, units='cm',dpi=300)
}

for(s in unique(cv_rgint$state)){
  ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Cases per 100k')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_rgint.jpg'), width=15, height=18, units='cm',dpi=300)
}




#Plots by rgi-----------
cv_rgi<-cvw%>%group_by(rgi_code, rgi, week)%>%
  summarise(rgi_code=min(rgi_code), rgi=min(rgi), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T),
            pop=sum(pop, na.rm=T))
cv_rgi<-cv_rgi%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)


for(s in unique(cv_rgi$state)){
  ggplot(cv_rgi%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Deaths per 100k')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgi.jpg'), width=15, height=18, units='cm',dpi=300)
}

for(s in unique(cv_rgi$state)){
  ggplot(cv_rgi%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Cases per 100k')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_rgi.jpg'), width=15, height=18, units='cm',dpi=300)
}



