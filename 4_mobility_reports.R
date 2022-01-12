#Mobility reports
library(ggplot2)
library(curl)
library(dplyr)
library(readr)
library(reshape2)
library(zoo)

rm(list=ls()) ## Removendo as variáveis

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=14))
)

load('input_data/cities.Rda')
load('input_data/cv_data.Rda')

city_names<-unique(cv_today%>%select(city_code,city,state))

cities<-merge(city_names,cities,by='city_code')

urlfile='https://github.com/ActiveConclusion/COVID19_mobility/blob/master/google_reports/mobility_report_brazil.csv?raw=true'

mob<-read_csv(url(urlfile))

#filter total and states data

names(mob)<-c('country','state','city','date','recreation','grocery','parks','stations','workplaces','residential')



mob<-as.data.frame(mob)

mob$city<-toupper(mob$city)


#Solving errors at mob
unique(mob$state)
mob$state<-replace(x=mob$state, which(mob$state=='Federal District'),'DF')
mob$state<-replace(x=mob$state, which(mob$state=='State of Alagoas'),'AL')
mob$state<-replace(x=mob$state, which(mob$state=='State of Amazonas'),'AM')
mob$state<-replace(x=mob$state, which(mob$state=='State of Goiás'),'GO')
mob$state<-replace(x=mob$state, which(mob$state=='State of Minas Gerais'),'MG')
mob$state<-replace(x=mob$state, which(mob$state=='State of Pernambuco'),'PE')
mob$state<-replace(x=mob$state, which(mob$state=='State of Rio Grande do Sul'),'RS')
mob$state<-replace(x=mob$state, which(mob$state=='State of São Paulo'),'SP')
mob$state<-replace(x=mob$state, which(mob$state=='State of Acre'),'AC')
mob$state<-replace(x=mob$state, which(mob$state=='State of Bahia'),'BA')
mob$state<-replace(x=mob$state, which(mob$state=='State of Maranhão'),'MA')
mob$state<-replace(x=mob$state, which(mob$state=='State of Pará'),'PA')
mob$state<-replace(x=mob$state, which(mob$state=='State of Piauí'),'PI')
mob$state<-replace(x=mob$state, which(mob$state=='State of Rondônia'),'RO')
mob$state<-replace(x=mob$state, which(mob$state=='State of Sergipe'),'SE')
mob$state<-replace(x=mob$state, which(mob$state=='State of Ceará'),'CE')
mob$state<-replace(x=mob$state, which(mob$state=='State of Mato Grosso'),'MT')
mob$state<-replace(x=mob$state, which(mob$state=='State of Paraíba'),'PB')
mob$state<-replace(x=mob$state, which(mob$state=='State of Rio de Janeiro'),'RJ')
mob$state<-replace(x=mob$state, which(mob$state=='State of Roraima'),'RR')
mob$state<-replace(x=mob$state, which(mob$state=='State of Tocantins'),'TO')
mob$state<-replace(x=mob$state, which(mob$state=='State of Amapá'),'AP')
mob$state<-replace(x=mob$state, which(mob$state=='State of Espírito Santo'),'ES')
mob$state<-replace(x=mob$state, which(mob$state=='State of Mato Grosso do Sul'),'MS')
mob$state<-replace(x=mob$state, which(mob$state=='State of Paraná'),'PR')
mob$state<-replace(x=mob$state, which(mob$state=='State of Rio Grande do Norte'),'RN')
mob$state<-replace(x=mob$state, which(mob$state=='State of Santa Catarina'),'SC')

mob$city<-replace(x=mob$city, which(mob$city=='ACARAPÉ'),'ACARAPE')
mob$city<-replace(x=mob$city, which(mob$city=='ASSU'),'AÇU')
mob$city<-replace(x=mob$city, which(mob$city=='ATILIO VIVÁCQUA'),'ATILIO VIVACQUA')
mob$city<-replace(x=mob$city, which(mob$city=='BIRIGÜI'),'BIRIGUI')
mob$city<-replace(x=mob$city, which(mob$city=='BIRITIBA-MIRIM'),'BIRITIBA MIRIM')
mob$city<-replace(x=mob$city, which(mob$city=='BOM SUCESSO, MINAS GERAIS'),'BOM SUCESSO')
mob$city<-replace(x=mob$city, which(mob$city=='BRASÓPOLIS'),'BRAZÓPOLIS')
mob$city<-replace(x=mob$city, which(mob$city=='CANDEIAS, MINAS GERAIS'),'CANDEIAS')
mob$city<-replace(x=mob$city, which(mob$city=='CHUI'),'CHUÍ')
mob$city<-replace(x=mob$city, which(mob$city=='CARIRIAÇÚ'),'CARIRIAÇU')
mob$city<-replace(x=mob$city, which(mob$city=='DOM PEDRO, MARANHÃO'),'DOM PEDRO')
mob$city<-replace(x=mob$city, which(mob$city=='ELDORADO DOS CARAJÁS'),'ELDORADO DOS CARAJAS')
mob$city<-replace(x=mob$city, which(mob$city=='EMBÚ'),'EMBÚ DAS ARTES')
mob$city<-replace(x=mob$city, which(mob$city=='GUARANTA DO NORTE'),'CARIRIAÇU')
mob$city<-replace(x=mob$city, which(mob$city=='IPÚ'),'IPU')
mob$city<-replace(x=mob$city, which(mob$city=='IRACEMA, CEARÁ'),'IRACEMA')
mob$city<-replace(x=mob$city, which(mob$city=='MOJI MIRIM'),'MOGI MIRIM')
mob$city<-replace(x=mob$city, which(mob$city=='RIO MARIA, PARÁ'),'RIO MARIA')
mob$city<-replace(x=mob$city, which(mob$city=='SANTA ISABEL DO PARÁ'),'SANTA ISABEL')
mob$city<-replace(x=mob$city, which(mob$city=='SAO JOSE DOS CAMPOS'),'SÃO JOSÉ DOS CAMPOS')
mob$city<-replace(x=mob$city, which(mob$city=='SÃO THOMÉ DAS LETRAS'),'SÃO TOMÉ DAS LETRAS')
mob$city<-replace(x=mob$city, which(mob$city=='VALENTE, BAHIA'),'VALENTE')
mob$city<-replace(x=mob$city, which(mob$city=='POMPEIA'),'POMPÉIA')
mob$city<-replace(x=mob$city, which(mob$city=='PINHALZINHO, SÃO PAULO'),'PINHALZINHO')
mob$city<-replace(x=mob$city, which(mob$city=='ITAPAJE'),'ITAPAJÉ')
mob$city<-replace(x=mob$city, which(mob$city=='ITAMARACÁ'),'ILHA DE ITAMARACÁ')
mob$city<-replace(x=mob$city, which(mob$city=='LUÍS EDUARDO MAGALHAES'),'LUÍS EDUARDO MAGALHÃES')
mob$city<-replace(x=mob$city, which(mob$city=='ELDORADO DOS CARAJÁS'),'ELDORADO DO CARAJÁS')
mob$city<-replace(x=mob$city, which(mob$city=='PACAJÚS'),'PACAJUS')
mob$city<-replace(x=mob$city, which(mob$city=='EMBÚ DAS ARTES'),'EMBU DAS ARTES')
mob$city<-replace(x=mob$city, which(mob$city=='SANTA ISABEL'),'SANTA IZABEL DO PARÁ')



##
mob_city<-mob%>%filter(city!='Total', state!='Total')

unique(mob_city$rgint)


mob_city<-merge(mob_city,cities,by=c('city','state'))

mob_rgint<-mob_city%>%group_by(rgint,state,date)%>%
  summarise(workplaces=max(workplaces,na.rm = T),
            stations=max(stations,na.rm = T),
            grocery=max(grocery,na.rm = T),
            residential=max(residential, na.rm=T),
            recreation=max(recreation, na.rm=T),
            parks=max(parks, na.rm=T))


##Calculating rolling average (7 days)-------------
mob_rgint<-mob_rgint%>%
  arrange(state,rgint,date)%>%
  group_by(state,rgint)%>%
  mutate(grocery=rollapply(grocery, 14, mean, align='right',fill=NA,na.rm=T),
         parks=rollapply(parks, 14, mean, align='right',fill=NA,na.rm=T),
         recreation=rollapply(recreation, 14, mean, align='right',fill=NA,na.rm=T),
         stations=rollapply(stations, 14, mean, align='right',fill=NA,na.rm=T),
         workplaces=rollapply(workplaces, 14, mean, align='right',fill=NA,na.rm=T),
         residential=rollapply(residential, 14, mean, align='right',fill=NA,na.rm=T))
  
mob_rgint.m<-melt(mob_rgint%>%select(-parks,-residential,-recreation,-grocery, -stations),id=c('state','rgint','date'))


s='RS'
for(s in unique(mob_rgint.m$state)){
  ggplot(mob_rgint%>%filter(state==sprintf(s,'%s'),workplaces>=-50),aes(x=date,y=workplaces),alpha=0.5)+
    geom_line()+
    facet_wrap(~rgint)+
    theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Mobility in Workplaces')+
    ylim(c(-50,50))+
    ggtitle(sprintf(s,'%s'))
  ggsave(paste0('figures/',sprintf(s,'%s'),'_mobility_rgint.jpg'), width=15, height=20, units='cm',dpi=300)
}



unique(mob_city$state)
unique(cities$rgint)
unique(cv_cases$state)
unique(mob$state)
#################3




mob_state<-mob%>%filter(city=='Total',state!='Total')
mob_state<-mob_state%>%select(-city,-country,-grocery,-recreation,-parks,-residential)


mob.m<-melt(mob_state,id=c('state','date'))
ggplot(mob.m,aes(x=date,y=value,color=variable))+
  geom_path()+
  facet_wrap(~state,scales='free_y')
