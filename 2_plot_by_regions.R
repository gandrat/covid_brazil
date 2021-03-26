packages<-c('ggplot2','dplyr')

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


cvw<-cv_cases_week%>%select(city_code,state, date,week, cases, deaths, new_cases, new_deaths)
cvw<-merge(cvw,cities%>%select(city_code,rgint_code,rgint,rgi_code,rgi,pop), by='city_code')

#Plots by rgiint-----------
cv_rgi<-cvw%>%group_by(rgi_code, rgi, week)%>%
  summarise(rgi_code=min(rgi_code), rgi=min(rgi), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T))
rgipop<-cities%>%group_by(rgi)%>%summarise(pop=sum(pop,na.rm=T))
cv_rgi<-merge(cv_rgi,rgipop,by='rgi')
cv_rgi<-cv_rgi%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)


cv_bra<-cvw%>%group_by(week)%>%summarise(date=min(date),
                                         week=min(week,na.rm = T),
                                         new_cases=sum(new_cases, na.rm=T),
                                         new_deaths=sum(new_deaths,na.rm=T))
cv_bra$pop<-sum(rgipop$pop)
sum(cv_bra$new_deaths)
cv_bra<-cv_bra%>%mutate(deaths100k=new_deaths*100000/pop,
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


#Plots by rgint-----------
cv_rgint<-cvw%>%group_by(rgint_code, rgint, week)%>%
  summarise(rgint_code=min(rgint_code), rgint=min(rgint), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T))
rgintpop<-cities%>%group_by(rgint)%>%summarise(pop=sum(pop,na.rm=T))
cv_rgint<-merge(cv_rgint,rgintpop,by='rgint')
cv_rgint<-cv_rgint%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)


cv_bra<-cvw%>%group_by(week)%>%summarise(date=min(date),
                                         week=min(week,na.rm = T),
                                         new_cases=sum(new_cases, na.rm=T),
                                         new_deaths=sum(new_deaths,na.rm=T))
cv_bra$pop<-sum(rgintpop$pop)

cv_bra<-cv_bra%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)


for(s in unique(cv_rgint$state)){
  ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Deaths per 100k')
  ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgint.jpg'), width=15, height=18, units='cm',dpi=300)
}


