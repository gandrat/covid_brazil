## COVID-2019

#Preparing data

## Made for represent COVID cases evolution for each city in Brazil
## Tiago Gandra (tiago.gandra@riogrande.ifrs.edu.br)

rm(list=ls()) #removing previous objects

# Load or install required packages-------------
packages = c('dplyr','curl','jsonlite','ggplot2','R.utils','zoo')


package.check = lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)}})


#setting workspace for server

#Check its existence and remove it
fn<-'input_data/casos_full'
if (file.exists(fn)) {file.remove(fn)}

# Download data-------------
curl_download(url="https://data.brasil.io/dataset/covid19/caso_full.csv.gz","input_data/casos_full.gz")
gunzip('input_data/casos_full.gz',remove=F)
cv_all<-read.csv2('input_data/casos_full',header = T, sep=',')

#Change variables names and order----------
cv_all<-cv_all%>%transmute(city=city,
                           city_code=city_ibge_code,
                           date=date,
                           week=epidemiological_week,
                           cases=last_available_confirmed,
                           new_cases=new_confirmed,
                           cases100k=as.numeric(last_available_confirmed_per_100k_inhabitants),
                           deaths=last_available_deaths,
                           new_deaths=new_deaths,
                           deaths100k=last_available_deaths*100000/estimated_population_2019,
                           death_rate=last_available_death_rate,
                           pop=estimated_population,
                           is_last=is_last,
                           place_type=place_type,
                           state=state,
                           state_code=substring(city_ibge_code,1,2))

cv_all$city<-toupper(cv_all$city)
cv_all$date<-as.Date(cv_all$date)

#Removing data with undefined city----------
cv_cases_state<-cv_all%>%select(-city,-city_code)%>%filter(place_type=='state')
cv_cases_ind<-cv_all%>%filter(place_type!='state' & city=='IMPORTADOS/INDEFINIDOS')
cv_cases<-cv_all%>%filter(place_type=='city' & city!='IMPORTADOS/INDEFINIDOS')



#Get the last available data--------------
cv_today<-cv_cases%>%filter(is_last=='True')
cv_today_state<-cv_cases_state%>%filter(is_last=='True')

#Check data
sum(cv_today$deaths)
sum(cv_today_state$deaths)
sum(cv_today$new_deaths)
cv_today_ind<-cv_cases_ind%>%filter(is_last=='True')
sum(cv_today_ind$deaths)


#Calculating rolling average (7 days)-------------
cv_cases<-cv_cases%>%
  arrange(city_code,date)%>%
  group_by(city_code)%>%
  mutate(cases7=rollapply(new_cases, 7, mean, align='right',fill=NA,na.rm=T),
         deaths7=rollapply(new_deaths, 7, mean, align='right',fill=NA,na.rm=T))

cv_cases_state<-cv_cases_state%>%
  arrange(state_code,date)%>%
  group_by(state_code)%>%
  mutate(cases7=rollapply(new_cases, 7, mean, align='right',fill=NA,na.rm=T),
         deaths7=rollapply(new_deaths, 7, mean, align='right',fill=NA,na.rm=T))

#Calculating weekly numbers-------------
cv_cases_week<-cv_cases%>%group_by(city_code,city,state,state_code,week)%>%
  summarise(date=max(date),
            cases=max(cases),deaths=max(deaths),
            new_cases=sum(new_cases),new_deaths=sum(new_deaths),
            pop=mean(pop))%>%
  as.data.frame()

cv_cases_week<-cv_cases_week%>%mutate(cases100k=new_cases*100000/pop,
                                                  deaths100k=new_deaths*100000/pop)



cv_cases_state_week<-cv_cases_state%>%group_by(state,state_code,week)%>%
  summarise(date=max(date),
            cases=max(cases),deaths=max(deaths),
            new_cases=sum(new_cases),new_deaths=sum(new_deaths),
            pop=mean(pop))%>%
  as.data.frame()

cv_cases_state_week<-cv_cases_state_week%>%mutate(cases100k=new_cases*100000/pop,
                                                  deaths100k=new_deaths*100000/pop)
  


#save RDA----------
save(cv_cases,cv_cases_week, cv_today, cv_cases_state, cv_today_state, cv_cases_state_week,
     file='input_data/cv_data.Rda')


#Saving csv files
write.csv(cv_cases_week,'output_data/cv_cases_week.csv')
write.csv(cv_today,'output_data/cv_cases_today.csv')
write.csv(cv_cases,'output_data/cv_cases.csv')
write.csv(cv_cases_state,'output_data/cv_cases_states.csv')
write.csv(cv_cases_state_week,'output_data/cv_cases_states_week.csv')
