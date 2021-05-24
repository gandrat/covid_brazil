## COVID-2019

#Preparing data for Mato Grosso State
#Data from https://sistemas.saude.mt.gov.br/

## Made for represent COVID cases evolution for each city in Brazil
## Tiago Gandra (tiago.gandra@riogrande.ifrs.edu.br)

rm(list=ls()) #removing previous objects

# Load or install required packages-------------
packages = c('dplyr','ggplot2','readxl','zoo','lubridate','tidyr','stringr')


package.check = lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)}})



#Load MT_data-----------
cv<-read_xlsx('input_data/MT_data_20210510.xlsx',skip = 2)
head(cv)

#Aggregating by days---------------
cv_casesMT<-cv%>%group_by(Municipio,CodigoIBGE,DataNotificacao)%>%
  summarise(new_cases=n(),new_deaths=sum(Situacao=='Óbito'))


cv_casesMT<-cv_casesMT%>%transmute(city=toupper(Municipio),
                                   date=as.Date(DataNotificacao, format="%d/%m/%Y"),
                                   new_cases=new_cases,
                                   new_deaths=new_deaths)
cv_casesMT$Municipio<-NULL
cv_casesMT$CodigoIBGE<-NULL

#Completing Dates-----------
cv_casesMT<-cv_casesMT%>%group_by(city)%>%
  complete(date = seq.Date(min(date), max(date), by="day"))

cv_casesMT[is.na(cv_casesMT)]<-0

min(cv_casesMT$date)
max(cv_casesMT$date)

#Calculating cummulative cases/deaths
cv_casesMT<-cv_casesMT%>%group_by(city)%>%
  arrange(date)%>%
  mutate(cases=cumsum(new_cases),
         deaths=cumsum(new_deaths))
ggplot(cv_casesMT,aes(x=date,y=cases))+geom_bar(stat='identity')
ggplot(cv_casesMT,aes(x=date,y=new_cases))+geom_bar(stat='identity')
ggplot(cv_casesMT,aes(x=date,y=deaths))+geom_bar(stat='identity')

str_replace(cv_casesMT,"D OESTE","D'OESTE")

#Merge with original data (brasil.io)------------
load('input_data/cv_data.Rda')
cv_data<-cv_cases%>%filter(state=='MT')%>%select(city, city_code, pop, state, state_code)
cv_data<-unique(cv_data)
cv_casesMT<-merge(cv_casesMT,cv_data,by='city',all.x=T)
head(cv_casesMT)

cv_casesMT<-cv_casesMT%>%transmute(city=city,
                                   city_code=city_code,
                                   date=date,
                                   week=strftime(date, format='%Y%V'),
                                   cases=cases,
                                   new_cases=new_cases,
                                   cases100k=cases*100000/pop,
                                   deaths=deaths,
                                   new_deaths=new_deaths,
                                   deaths100k=deaths*100000/pop,
                                   death_rate=deaths/cases,
                                   pop=pop,
                                   is_last='False',
                                   place_type='city',
                                   state=state,
                                   state_code=state_code)

#Rolling average
cv_casesMT<-cv_casesMT%>%
  arrange(city_code,date)%>%
  group_by(city_code)%>%
  mutate(cases7=rollapply(new_cases, 7, mean, align='right',fill=NA,na.rm=T),
         deaths7=rollapply(new_deaths, 7, mean, align='right',fill=NA,na.rm=T))

head(cv_casesMT)
head(cv_cases)

#Set maximum date
maxdate<-'2021-05-02'
cv_casesMT<-cv_casesMT%>%filter(date<=maxdate)


#Comparing data
sum(cv_casesMT$new_deaths,na.rm=T)
sum(cv_cases%>%filter(state=='MT')%>%select(new_deaths))

sum(cv_cases%>%filter(state=='MT')%>%select(new_cases))
sum(cv_casesMT$new_cases)


# Merge with Brazil IO
cv_cases_raw<-cv_cases
cv_cases_state_raw<-cv_cases_state



cv_cases<-cv_cases%>%filter(state!='MT')
# cv_cases_state<-cv_cases_state%>%filter(state!='MT')

cv_cases<-rbind(cv_cases,cv_casesMT)
# cv_cases_state<-rbind(cv_cases_state,cv_cases_stateMT)

sum(cv_cases$new_cases)
sum(cv_cases$new_deaths,na.rm=T)

cv_cases<-cv_cases%>%mutate(week=strftime(date, format='%Y%V'))

cv_today<-cv_cases%>%filter(date=='2021-05-02')

#Calculating weekly numbers-------------
cv_cases_week<-cv_cases%>%group_by(city_code,city,state,state_code,week)%>%
  summarise(date=min(date),
            cases=max(cases),deaths=max(deaths),
            new_cases=sum(new_cases),new_deaths=sum(new_deaths),
            pop=mean(pop))%>%
  as.data.frame()

cv_cases_week<-cv_cases_week%>%mutate(cases100k=new_cases*100000/pop,
                                      deaths100k=new_deaths*100000/pop)



# cv_cases_state_week<-cv_cases_state%>%group_by(state,state_code,week)%>%
#   summarise(date=max(date),
#             cases=max(cases),deaths=max(deaths),
#             new_cases=sum(new_cases),new_deaths=sum(new_deaths),
#             pop=mean(pop))%>%
#   as.data.frame()
# 
# cv_cases_state_week<-cv_cases_state_week%>%mutate(cases100k=new_cases*100000/pop,
#                                                   deaths100k=new_deaths*100000/pop)


save(cv_cases,cv_cases_week, cv_today, cv_cases_state, cv_today_state, cv_cases_state_week,
     file='input_data/cv_data_V5_MT.Rda')
