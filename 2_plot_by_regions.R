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
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=14))
)

Sys.setlocale(category = "LC_TIME", locale = "pt_BR.utf8")
#Load COVID data--------
load('input_data/cv_data.Rda')
sum(cv_today$deaths)
max(cv_cases$date)
sum(cv_today_state$deaths)
max(cv_today$week)
dates<-(cv_cases%>%filter(week==202115)%>%select(date))
unique(dates$date)
#Load Cities data
load('input_data/cities.Rda')

cit<-unique(cv_cases%>%select(city_code,state))
cities<-merge(cities,cit,by='city_code')



cvw<-cv_cases_week%>%select(city_code,city,state,date,week, cases, deaths, new_cases, new_deaths,deaths100k,cases100k)
cvw<-merge(cvw,cities%>%select(city_code,rgint_code,rgint,rgi_code,rgi, regsaude, nome_reg, pop), by='city_code')
max(cvw$week)
# cvw<-cvw%>%filter(week!=max(cvw$week))
max(cvw$date)

#Agregating for Brazil-------
cv_bra<-cvw%>%group_by(week)%>%summarise(date=max(date),
                                         new_cases=sum(new_cases, na.rm=T),

                                                                                  new_deaths=sum(new_deaths,na.rm=T))
cv_bra$pop<-sum(cv_today_state$pop)

cv_bra<-cv_bra%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)


#Plots by RegSaude-----------
cv_rgs<-cvw%>%group_by(regsaude, nome_reg,state, week)%>%
  summarise(date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T))
rgspop<-cities%>%group_by(regsaude,state)%>%summarise(pop=sum(pop,na.rm=T))
cv_rgs<-merge(cv_rgs,rgspop,by=c('regsaude','state'))
cv_rgs<-cv_rgs%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)

cv_rgs<-cv_rgs%>%filter(state!='DF')

s='RS'
for(s in unique(cv_rgs$state)){
  ggplot(cv_rgs%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~nome_reg)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Óbitos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões de Saúde'))
  ggsave(paste0('figures_V6/',sprintf(s,'%s'),'_deaths_regsaude.jpg'), width=15, height=20, units='cm',dpi=300)
}

for(s in unique(cv_rgs$state)){
  ggplot(cv_rgs%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~nome_reg)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Casos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões de Saúde'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_regsaude.jpg'), width=15, height=20, units='cm',dpi=300)
}

#Plots by rgi-----------
cv_rgi<-cvw%>%group_by(rgi_code, rgi, state, week)%>%
  summarise(rgi_code=min(rgi_code), rgi=min(rgi), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T))
rgipop<-cities%>%group_by(rgi,state)%>%summarise(pop=sum(pop,na.rm=T))
cv_rgi<-merge(cv_rgi,rgipop,by=c('rgi','state'))
cv_rgi<-cv_rgi%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)

cv_rgi<-cv_rgi%>%filter(state!='DF')


for(s in unique(cv_rgi$state)){
  ggplot(cv_rgi%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Óbitos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões Imediatas'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgi.jpg'), width=15, height=20, units='cm',dpi=300)
}

for(s in unique(cv_rgi$state)){
  ggplot(cv_rgi%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Casos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões Imediatas'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_rgi.jpg'), width=15, height=20, units='cm',dpi=300)
}


#Plots by rgint-----------
cv_rgint<-cvw%>%group_by(rgint_code, rgint, week, state)%>%
  summarise(rgint_code=min(rgint_code), rgint=min(rgint), state=min(state),
            week=as.factor(min(week)),
            date=min(date),
            cases=sum(cases, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_cases=sum(new_cases, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T))
rgintpop<-cities%>%group_by(rgint,state)%>%summarise(pop=sum(pop,na.rm=T))
cv_rgint<-merge(cv_rgint,rgintpop,by=c('rgint','state'))
cv_rgint<-cv_rgint%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)

cv_rgint<-cv_rgint%>%filter(state!='DF')


cv_rgint$state<-replace(x=cv_rgint$state, which(cv_rgint$state=='DF'),'GO')

for(s in unique(cv_rgint$state)){
  ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Óbitos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões Intermediárias'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgint.jpg'), width=15, height=20, units='cm',dpi=300)
}

for(s in unique(cv_rgint$state)){
  ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_bar(stat='identity',fill='grey')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Casos por 100 mil hab')+
    scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
    ggtitle(paste0(sprintf(s,'%s'),' - Regiões Intermediárias'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_rgint.jpg'), width=15, height=20, units='cm',dpi=300)
}


#Plots for DF--------------------

#Change state for RMB (DF X GO)
rmb<-c('BRASÍLIA','ÁGUAS LINDAS DE GOIÁS','ALEXÂNIA', 'CIDADE OCIDENTAL','COCALZINHO DE GOIÁS','CRISTALINA','FORMOSA','LUZIÂNIA','NOVO GAMA', 'PADRE BERNARDO','PLANALTINA','SANTO ANTÔNIO DO DESCOBERTO','VALPARAÍSO DE GOIÁS')
cvw_df<-cvw%>%filter(city %in% rmb)
unique(cvw_df%>%select(city_code,city))

cvw_df<-cvw_df%>%mutate(cases100k=new_cases*100000/pop,deaths100k=new_deaths*100000/pop)

ggplot(cvw_df,aes(x=date,y=cases100k))+
  geom_bar(stat='identity',fill='grey')+
  facet_wrap(~city)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Cases per 100k')+
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month",limits=c(as.Date('2020-03-01'),as.Date('2021-07-01')))+
  ggtitle(NULL)
ggsave('figures/DF_cases_city.jpg', width=15, height=20, units='cm',dpi=300)

ggplot(cvw_df,aes(x=date,y=deaths100k))+
  geom_bar(stat='identity',fill='grey')+
  facet_wrap(~city)+
  geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
  # theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month")+
  xlab(NULL)+ylab('Deaths per 100k')+
  ggtitle('Área Metropolitana de Brasília')
ggsave('figures/DF_deaths_city.jpg', width=15, height=20, units='cm',dpi=300)
max(cv_cases_state_week$date)
s='RS'
#Plots by States--------------
for(s in unique(cv_cases_state_week$state)){
  ggplot(cv_cases_state_week%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
    geom_area(stat='identity',fill='grey')+
    geom_vline(xintercept=as.Date("2021-05-02"),linetype=2)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
    # theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ggtitle(NULL)
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_state.jpg'), width=12.3, height=8, units='cm',dpi=300)
}

s<-'RS'
for(s in unique(cv_cases_state_week$state)){
  ggplot(cv_cases_state_week%>%filter(state==sprintf(s,'%s')),aes(x=date,y=deaths100k))+
    geom_area(fill='grey')+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,group=1, inherit.aes=F),size=.2)+
    geom_vline(xintercept=as.Date("2021-05-02"),linetype=2)+
    # theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ggtitle(NULL)
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_state.jpg'), width=12.3, height=8, units='cm',dpi=300)
}


# #SCRAPBOOK----------------
# #Boxplots by RGINT----
# s='RS'
# for(s in unique(cvw$state)){
#   ggplot(cvw%>%filter(state==sprintf(s,'%s')),aes(x=as.factor(week),y=deaths100k))+
#     geom_boxplot(outlier.shape = NA)+
#     facet_wrap(~rgint)+
#     geom_path(data=cv_bra,aes(x=as.factor(week),y=deaths100k,group=1, inherit.aes=F),size=.2)+
#     theme(axis.text.x = element_text(angle = 90))+
#     xlab(NULL)+ylab('Deaths per 100k')+
#     ylim(c(0,50))+
#     ggtitle(sprintf(s,'%s'))
#   ggsave(paste0('figures/',sprintf(s,'%s'),'_deaths_rgint_boxplot.jpg'), width=15, height=20, units='cm',dpi=300)
# }
# 
# for(s in unique(cv_rgint$state)){
#   ggplot(cv_rgint%>%filter(state==sprintf(s,'%s')),aes(x=date,y=cases100k))+
#     geom_bar(stat='identity',fill='grey')+
#     facet_wrap(~rgint)+
#     geom_path(data=cv_bra,aes(x=date,y=cases100k,group=1, inherit.aes=F),size=.2)+
#     theme(axis.text.x = element_text(angle = 90))+
#     xlab(NULL)+ylab('Cases per 100k')+
#     ggtitle(sprintf(s,'%s'))
#   ggsave(paste0('figures/',sprintf(s,'%s'),'_cases_rgint.jpg'), width=15, height=20, units='cm',dpi=300)
# }
