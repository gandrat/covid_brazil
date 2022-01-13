## COVID-2019

#Preparing data for Mato Grosso State
#Data from https://sistemas.saude.mt.gov.br/

## Made for represent COVID cases evolution for each city in Brazil
## Tiago Gandra (tiago.gandra@riogrande.ifrs.edu.br)

rm(list=ls()) #removing previous objects

# Load or install required packages-------------
packages = c('dplyr','readxl','lubridate','tidyr','stringr')


package.check = lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)}})



#Load MT_data-----------
# cv<-read_xlsx('input_data/MT_data_20220113.xlsx',skip = 2)
# save(cv,file='input_data/MT_data_20220113.Rda')
# head(cv)


#Converting to brasil.io format---------------
load('input_data/MT_data_20220113.Rda')
cv_casesMT<-cv%>%group_by(Municipio,CodigoIBGE,DataNotificacao)%>%
  summarise(new_cases=n(),new_deaths=sum(Situacao=='Óbito'))

rm(cv)

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

#Get cities info----------
load('input_data/cv_data.Rda')

cv_MT<-cv_cases%>%filter(state=='MT')%>%select(city, city_code, pop, state, state_code)
cv_MT<-unique(cv_MT)

cv_casesMT$city<-str_replace(cv_casesMT$city,"D OESTE","D'OESTE")

cv_casesMT<-merge(cv_casesMT,cv_MT,by='city',all.x=T)
head(cv_casesMT)

cv_casesMT<-cv_casesMT%>%transmute(city=city,
                                   city_code=city_code,
                                   date=date,
                                   week=as.integer(strftime(date, format='%Y%V')),
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
library(zoo)
cv_casesMT<-cv_casesMT%>%
  arrange(city_code,date)%>%
  group_by(city_code)%>%
  mutate(cases7=rollapply(new_cases, 7, mean, align='right',fill=NA,na.rm=T),
         deaths7=rollapply(new_deaths, 7, mean, align='right',fill=NA,na.rm=T))

head(cv_casesMT)
head(cv_cases)

#Set maximum date
cv_casesMT<-cv_casesMT%>%filter(date<=max(cv_cases$date))


# Merge with Brazil IO-----------
cv_cases_raw<-cv_cases
cv_cases_state_raw<-cv_cases_state

cv_cases$death_rate<-as.numeric(cv_cases$death_rate)

cv_cases<-cv_cases%>%filter(state!='MT')
# cv_cases_state<-cv_cases_state%>%filter(state!='MT')

cv_cases<-rbind(cv_cases,cv_casesMT)
# cv_cases_state<-rbind(cv_cases_state,cv_cases_stateMT)

sum(cv_cases$new_cases)
sum(cv_cases_raw$new_cases)

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
     file='input_data/cv_data_V6_MT.Rda')
