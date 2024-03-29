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
  theme_bw(base_size = 9)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=14),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

Sys.setlocale(category = "LC_TIME", locale = "pt_BR.utf8")
#Load COVID data--------
load('input_data/cv_data_V6_MT.Rda')
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
cvw$week<-as.integer(cvw$week)
unique(cvw$week)
#Agregating for Brazil-------
cv_bra<-cvw%>%group_by(week)%>%summarise(date=max(date),
                                         new_cases=sum(new_cases, na.rm=T),
                                         new_deaths=sum(new_deaths,na.rm=T))
cv_bra$pop<-sum(cv_today_state$pop)

cv_bra<-cv_bra%>%mutate(deaths100k=new_deaths*100000/pop,
                        cases100k=new_cases*100000/pop)

cv_bra<-arrange(cv_bra,date)
cv_bra$state2='Brasil'



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

cv_rgs<-cv_rgs%>%filter(state%in% c('AL','ES','MS','MT','PB','PE','SE','TO','RO','BA'))

s='MT'
for(s in unique(cv_rgs$state)){
  d<-cv_rgs%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
    facet_wrap(~nome_reg)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_regsaude.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}

for(s in unique(cv_rgs$state)){
  d<-cv_rgs%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
    facet_wrap(~nome_reg)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_regsaude.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}

#Plots by RegSaude - BA -------------

#Agregating for BA
cv_BA<-cvw%>%filter(state=='BA')%>%
  group_by(week)%>%summarise(date=max(date),
                             new_cases=sum(new_cases, na.rm=T),
                             new_deaths=sum(new_deaths,na.rm=T))
BA_pop<-unique(cv_today_state[which(cv_today_state$state=='BA'),'pop'])

cv_BA<-cv_BA%>%mutate(deaths100k=new_deaths*100000/BA_pop$pop,
                        cases100k=new_cases*100000/BA_pop$pop)
cv_BA<-as.data.frame(cv_BA)


cv_BA$state2='Bahia'

s<-'BA'
d<-cv_rgs%>%filter(state==sprintf(s,'%s'),
                   nome_reg %in% c('Jacobina','Ribeira do Pombal','Ibotirama',
                                   'Salvador','Itabuna','Ilhéus'))
d<-d%>%mutate(top3=ifelse(nome_reg %in% c('Salvador','Itabuna','Ilhéus'),'Piores Desempenhos','Melhores Desempenhos'))

#Casos
ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
xmin=as.Date(min(cv_bra$date))
ggplot(d)+
  geom_area(data=d,aes(x=date,y=cases100k),stat='identity',fill='grey')+
  facet_wrap(top3~nome_reg,drop=T)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
  geom_path(data=cv_BA,aes(x=date,y=cases100k,color=state2),size=.3)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_color_manual(values = c("#de5004"))+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_regsaude.jpg'), width=12.3, height=12.3, units='cm',dpi=300)

#Deaths
ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
xmin=as.Date(min(cv_bra$date))
ggplot()+
  geom_area(data=d,aes(x=date,y=deaths100k),stat='identity',fill='grey')+
  facet_wrap(top3~nome_reg)+
  geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
  geom_path(data=cv_BA,aes(x=date,y=deaths100k,color=state2),size=.3)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_color_manual(values = c("#de5004"))+
  xlab(NULL)+ylab('Óbitos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_regsaude.jpg'), width=12.3, height=12.3, units='cm',dpi=300)


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

cv_rgi<-cv_rgi%>%filter(state%in% c('AC','CE','PR','SC','AM','PI','SC'))

s<-'AC'
for(s in unique(cv_rgi$state)){
  d<-cv_rgi%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_rgi.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}

for(s in unique(cv_rgi$state)){
  d<-cv_rgi%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
    facet_wrap(~rgi)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_rgi.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
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

cv_rgint<-cv_rgint%>%filter(state%in%c('PA','SP','GO','RS','PR','SC'))


s<-'PA'
for(s in unique(cv_rgint$state)){
  d<-cv_rgint%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_rgint.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}

for(s in unique(cv_rgint$state)){
  d<-cv_rgint%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
    facet_wrap(~rgint)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_rgint.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}



#Plots for DF--------------------

#Change state for RMB (DF X GO)
rmb<-c('BRASÍLIA','ÁGUAS LINDAS DE GOIÁS','ALEXÂNIA', 'CIDADE OCIDENTAL','COCALZINHO DE GOIÁS','CRISTALINA','FORMOSA','LUZIÂNIA','NOVO GAMA', 'PADRE BERNARDO','PLANALTINA','SANTO ANTÔNIO DO DESCOBERTO','VALPARAÍSO DE GOIÁS')

#Similar ao state, mas abrangendo todos os municípios da RMB.
d<-cvw_df<-cvw%>%filter(city %in% rmb)
d$state<-'RMB'
rmb_pop<-sum(cities%>%filter(city_code %in% unique(d$city_code))%>%select(pop))

d<-d%>%group_by(date,state)%>%summarise(new_deaths=sum(new_deaths),new_cases=sum(new_cases))

d$deaths100k<-d$new_deaths*100000/rmb_pop
d$cases100k<-d$new_cases*100000/rmb_pop

ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k,fill=state,),stat='identity')+
  geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax),
            color='black', alpha=0, linetype=2)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2, inherit.aes=F),size=.2)+
  scale_fill_grey(start=.7,end=1)+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  theme(legend.position = c(.9,0.8), legend.title = element_blank())+
  ylim(c(0,max(c(max(d$cases100k),max(cv_bra$cases100k)))))
ggsave('figures_v6/DF_RMB_cases_state.jpg', width=12.3, height=8, units='cm',dpi=300)

#Deaths
ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
ggplot()+
  geom_area(data=d,aes(x=date,y=deaths100k,fill=state,),stat='identity')+
  geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax),
            color='black', alpha=0, linetype=2)+
  geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2, inherit.aes=F),size=.2)+
  scale_fill_grey(start=.7,end=1)+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Óbitos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  theme(legend.position = c(.9,0.8), legend.title = element_blank())+
  ylim(c(0,max(c(max(d$deaths100k),max(cv_bra$deaths100k)))))
ggsave('figures_v6/DF_RMB_deaths_state.jpg', width=12.3, height=8, units='cm',dpi=300)


#Plots by States--------------
#Cases
for(s in unique(cv_cases_state_week$state)){
  d<-cv_cases_state_week%>%filter(state==sprintf(s,'%s'))  
  ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
  ggplot()+
    geom_area(data=d,aes(x=date,y=cases100k,fill=state,),stat='identity')+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2, inherit.aes=F),size=.2)+
    geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax),
              color='black', alpha=0, linetype=2)+
    # annotate("text", x = as.Date("2021-05-02"), y = ymax,label = "Período fora do escopo dos capítulos")+
    scale_fill_grey(start=.7,end=1)+
    # theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    theme(legend.position = c(.9,0.8), legend.title = element_blank())+
    ylim(c(0,ymax))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_state.jpg'), width=12.3, height=8, units='cm',dpi=300)
}


#Deaths
for(s in unique(cv_cases_state_week$state)){
  d<-cv_cases_state_week%>%filter(state==sprintf(s,'%s'))  
  ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
  ggplot()+
    geom_area(data=d,aes(x=date,y=deaths100k,fill=state,),stat='identity')+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2, inherit.aes=F),size=.2)+
    geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax),
              color='black', alpha=0, linetype=2)+
    # annotate("text", x = as.Date("2021-05-02"), y = ymax,label = "Período fora do escopo dos capítulos")+
    scale_fill_grey(start=.7,end=1)+
    # theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    theme(legend.position = c(.9,0.8), legend.title = element_blank())+
    ylim(c(0,ymax))
  
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_state.jpg'), width=12.3, height=8, units='cm',dpi=300)
}

# Plots by States - BA -----------
s<-'BA'
d<-cv_cases_state_week%>%filter(state==sprintf(s,'%s'))
ds<-d%>%filter(as.character(date) %in% c("2020-08-22","2020-11-14","2021-03-20","2021-05-01"))
ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k,fill=state,),stat='identity')+
  geom_bar(data=ds,aes(x=date,y=cases100k),stat='identity',width = 7)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2, inherit.aes=F),size=.2)+
  geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax+30),
            color='black', alpha=0, linetype=2)+
  # annotate("text", x = as.Date("2021-05-02"), y = ymax,label = "Período fora do escopo dos capítulos")+
  scale_fill_grey(start=.7,end=1)+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  theme(legend.position = c(.9,0.8), legend.title = element_blank())+
  ylim(c(0,ymax+30))
ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_state.jpg'), width=12.3, height=8, units='cm',dpi=300)

# Deaths
s<-'BA'
d<-cv_cases_state_week%>%filter(state==sprintf(s,'%s'))
ds<-d%>%filter(as.character(date) %in% c("2020-08-22","2020-11-14","2021-03-20","2021-05-01"))
ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
ggplot()+
  geom_area(data=d,aes(x=date,y=deaths100k,fill=state,),stat='identity')+
  geom_bar(data=ds,aes(x=date,y=deaths100k),stat='identity',width = 7)+
  geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2, inherit.aes=F),size=.2)+
  geom_rect(aes(xmax=as.Date("2021-05-02"), xmin=min(d$date), ymin=0,ymax=ymax+2),
            color='black', alpha=0, linetype=2)+
  # annotate("text", x = as.Date("2021-05-02"), y = ymax,label = "Período fora do escopo dos capítulos")+
  scale_fill_grey(start=.7,end=1)+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Óbitos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  theme(legend.position = c(.9,0.8), legend.title = element_blank())+
  ylim(c(0,ymax+2))
ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_state.jpg'), width=12.3, height=8, units='cm',dpi=300)

#Plots by States - PA --------------
#Cases
d<-cv_cases_state_week%>%filter(state %in% c('PA','AM','RR','AC','AP','RO','TO'))
ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
xmin=as.Date(min(cv_bra$date))
ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k),stat='identity',fill='grey')+
  facet_wrap(~state)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ylim(c(0,ymax))+
  theme(legend.position = c(.9,0.2), legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('figures_v6/PA_cases_state_N.jpg', width=12.3, height=12.3, units='cm',dpi=300)

#Plots by States - RN --------------
#Cases
d<-cv_cases_state_week%>%filter(state %in% c('RN'))

#separando períodos
d$fase<-ifelse(d$date<'2020-10-01','Fase 1','Fase 2')
cv_bra$fase<-ifelse(cv_bra$date<'2020-10-01','Fase 1','Fase 2')

d<-d%>%filter(date<='2021-05-03')
ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
xmin=as.Date(min(cv_bra$date))
ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
  facet_wrap(~fase,scales='free_x',ncol=2)+
  geom_path(data=cv_bra%>%filter(date<='2021-05-03'),aes(x=date,y=cases100k,linetype=state2),size=.2)+
  # geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%m/%Y",date_breaks  ="2 month")+
  # ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank())
ggsave('figures_v6/RN_cases_state_sep.jpg', width=12.3, height=12.3, units='cm',dpi=300)

ggplot()+
  geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
  facet_wrap(~fase,scales='free_x',ncol=2)+
  geom_path(data=cv_bra%>%filter(date<='2021-05-03'),aes(x=date,y=deaths100k,linetype=state2),size=.2)+
  # geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab('Óbitos / 100 mil hab')+
  scale_x_date(date_labels="%m/%Y",date_breaks  ="2 month")+
  # ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank())
ggsave('figures_v6/RN_deaths_state_sep.jpg', width=12.3, height=12.3, units='cm',dpi=300)

#Plots by States - PA --------------

d<-cv_cases_state_week%>%filter(state %in% c('PA'))

d<-d%>%select(state,date,cases100k,deaths100k)

library(reshape2)
d.m<-melt(d,id=c('state','date'))
cv_bra.m<-melt(cv_bra%>%select(state2,date,cases100k,deaths100k),id=c('state2','date'))
ggplot()+
  geom_area(data=d.m,aes(x=date,y=value,fill=state),stat='identity')+
  facet_wrap(~variable,scales='free_y',ncol=1,
             strip.position = "left", 
             labeller = as_labeller(c(cases100k = "Casos / 100 mil", 
                                      deaths100k = "Óbitos / 100 mil") ) )+
  
  geom_path(data=cv_bra.m,aes(x=date,y=value,linetype=state2),size=.2)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab(NULL)+
  scale_x_date(date_labels="%m/%Y",date_breaks  ="3 month")+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
ggsave('figures_v6/PA_casesdeaths_state.jpg', width=12.3, height=12.3, units='cm',dpi=300)

#período específico
d<-cv_cases_state%>%filter(state %in% c('PA'),
                           date>='2020-03-19',
                           date<='2020-05-25')

ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k,fill=state,),stat='identity')+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2, inherit.aes=F),size=.2)+
  # annotate("text", x = as.Date("2021-05-02"), y = ymax,label = "Período fora do escopo dos capítulos")+
  scale_fill_grey(start=.7,end=1)+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Casos / 100 mil')+
  scale_x_date(date_labels="%d/%m/%y",date_breaks  ="15 days",limits = c(min(d$date),max(d$date)))+
  theme(legend.position = c(0.1,0.7), legend.title = element_blank())
ggsave('figures_v6/PA_cases_state_periodo.jpg', width=12.3, height=6.3, units='cm',dpi=300)
#Plot by cities-----------
cv_city<-cvw%>%filter(state%in% c('RO'))

s<-'RO'
for(s in unique(cv_city$state)){
  d<-cv_city%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
    facet_wrap(~city)+
    geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Casos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.text.x = element_text(size = 6))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_cases_city.jpg'), width=12.3, height=12.3, units='cm',dpi=300)
}


for(s in unique(cv_city$state)){
  d<-cv_city%>%filter(state==sprintf(s,'%s'))
  ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
  ymax=50
  xmin=as.Date(min(cv_bra$date))
  ggplot()+
    geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
    facet_wrap(~city, ncol=6)+
    geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
    geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
    scale_fill_grey(start=.7,end=1)+
    xlab(NULL)+ylab('Óbitos / 100 mil hab')+
    scale_x_date(date_labels="%b-%y",date_breaks  ="4 month")+
    ylim(c(0,ymax))+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.text.x = element_text(size = 6))
  ggsave(paste0('figures_v6/',sprintf(s,'%s'),'_deaths_city.jpg'), width=12.3, height=14.3, units='cm',dpi=300)
}

#DF-----------
#Obitos DF
d<-cvw_df<-cvw%>%filter(city %in% rmb)
d$state='RMB'
ymax=max(c(max(d$deaths100k),max(cv_bra$deaths100k)))
xmin=as.Date(min(cv_bra$date))
ggplot()+
  geom_area(data=d,aes(x=date,y=deaths100k,fill=state),stat='identity')+
  facet_wrap(~city,ncol=3)+
  geom_path(data=cv_bra,aes(x=date,y=deaths100k,linetype=state2),size=.2)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab('Óbitos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 6))
ggsave('figures_v6/DF_RMB_deaths_city.jpg', width=12.3, height=14.3, units='cm',dpi=300)

#Cases DF
d<-cvw_df<-cvw%>%filter(city %in% rmb)
d$state='RMB'
ymax=max(c(max(d$cases100k),max(cv_bra$cases100k)))
xmin=as.Date(min(cv_bra$date))
ggplot()+
  geom_area(data=d,aes(x=date,y=cases100k,fill=state),stat='identity')+
  facet_wrap(~city,ncol=3)+
  geom_path(data=cv_bra,aes(x=date,y=cases100k,linetype=state2),size=.2)+
  geom_vline(xintercept = as.Date('2021-05-02'),linetype=2,size=.2)+
  scale_fill_grey(start=.7,end=1)+
  xlab(NULL)+ylab('Casos / 100 mil hab')+
  scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ylim(c(0,ymax))+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 6))
ggsave('figures_v6/DF_RMB_cases_city.jpg', width=12.3, height=14.3, units='cm',dpi=300)
