### PLASTIC POLLUTION ###
library(readr)
library(roahd)

setwd("~/Documents/Politecnico/Magistrale/Non Parametric Statistics/PROJECT")
PP_2 <- read_csv("PP_2.csv")

data = PP_2[ , -c(1,2,4,5,6,7,9,11,23,24,25,28,29,30,35,37,38,43:83)]
View(data)

######
#SECTION PULIZIA E PROBLEMI (MICHAEL E CAMI)
##TODO: ricordarsi di cambiare i nomi delle colonne di sto dataset perchè fanno vomitare
##TODO: scatterplot cose raccolte vs num di volontari

# impongo unica classe USA anche per United States, va fatto per tutti i doppioni
data[which(data$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
data[which(data$CountryName_FromSource == 'IT'), ]$CountryName_FromSource = 'Italy'
levels(factor(data$CountryName_FromSource))
#CAMI: ci sono un sacco di etichette strane per gli stati, vediamo come fare

#CAMI: sposto questa parte qua così pulisco prima i NAN
sum(is.na(data$TotalVolunteers))
newdata <- data[!is.na(data$TotalVolunteers),] #-2418 dati
View(newdata)

#CAMI: per ora metto in commentato l'area, mi concentro su nome stato, data e q.tà raccolta
#per fortuna non ho NAN in TotalItems_EventRecord
#

#newdata <- newdata[!is.na(newdata$TotalWidth_m),] #-49894 dati!! contiene solo 2121 dati
#significa che 49894 righe non hanno il dato larghezza e quindi non possiamo calcolare 
#la superificie quindi rende le tre colonne larghezza, lunghezza e superficie inutili, 
#questo secondo me è un problema perchè avremmo potuto fare delle analisi tipo volontari vs area
#o quantità di plastica vs area 
levels(factor(newdata$CountryName_FromSource))

#CAMI: queste etichette andavano cambiate per omonimia o cose no sense(tipo oslo municipality)
newdata[which(newdata$CountryName_FromSource == 'BA'), ]$CountryName_FromSource = 'Bosnia Erzegovina'
newdata[which(newdata$CountryName_FromSource == 'BE'), ]$CountryName_FromSource = 'Belgium'
newdata[which(newdata$CountryName_FromSource == 'BG'), ]$CountryName_FromSource = 'Bulgaria'
newdata[which(newdata$CountryName_FromSource == 'CH'), ]$CountryName_FromSource = 'Switzerland'
newdata[which(newdata$CountryName_FromSource == "CÃ´te d'Ivoire"), ]$CountryName_FromSource = 'Ivory Cost'
newdata[which(newdata$CountryName_FromSource == 'CY'), ]$CountryName_FromSource = 'Cyprus'
newdata[which(newdata$CountryName_FromSource == 'DK'), ]$CountryName_FromSource = 'Denmark'
newdata[which(newdata$CountryName_FromSource == 'DZ'), ]$CountryName_FromSource = 'Algeria'
newdata[which(newdata$CountryName_FromSource == 'British Virgin Islands'), ]$CountryName_FromSource = 'Virgin Islands'
newdata[which(newdata$CountryName_FromSource == 'ZA'), ]$CountryName_FromSource = 'South Africa'
newdata[which(newdata$CountryName_FromSource == 'DE'), ]$CountryName_FromSource = 'Germany'
newdata[which(newdata$CountryName_FromSource == 'Caribbean Netherlands'), ]$CountryName_FromSource = 'Netherlands'
newdata[which(newdata$CountryName_FromSource == 'CNMI'), ]$CountryName_FromSource = 'Northern Mariana Island'
newdata[which(newdata$CountryName_FromSource == 'Czechia'), ]$CountryName_FromSource = 'Czech Republic'
newdata[which(newdata$CountryName_FromSource == 'ES'), ]$CountryName_FromSource = 'Spain'
newdata[which(newdata$CountryName_FromSource == 'FR'), ]$CountryName_FromSource = 'France'
newdata[which(newdata$CountryName_FromSource == 'GE'), ]$CountryName_FromSource = 'Georgia'
newdata[which(newdata$CountryName_FromSource == 'GR'), ]$CountryName_FromSource = 'Greece'
newdata[which(newdata$CountryName_FromSource == 'Dakar'), ]$CountryName_FromSource = 'Senegal'
newdata[which(newdata$CountryName_FromSource == 'NO'), ]$CountryName_FromSource = 'Norway'
newdata[which(newdata$CountryName_FromSource == 'Oslo Municipality'), ]$CountryName_FromSource = 'Norway'
newdata[which(newdata$CountryName_FromSource == 'RU'), ]$CountryName_FromSource = 'Russia'
newdata[which(newdata$CountryName_FromSource == 'HR'), ]$CountryName_FromSource = 'Croatia'
newdata[which(newdata$CountryName_FromSource == 'EE'), ]$CountryName_FromSource = 'Estonia'
newdata[which(newdata$CountryName_FromSource == 'UA'), ]$CountryName_FromSource = 'Ukraine'
newdata[which(newdata$CountryName_FromSource == 'NL'), ]$CountryName_FromSource = 'Netherlands'
newdata[which(newdata$CountryName_FromSource == 'SI'), ]$CountryName_FromSource = 'Slovenia'
newdata[which(newdata$CountryName_FromSource == 'SE'), ]$CountryName_FromSource = 'Sweden'
newdata[which(newdata$CountryName_FromSource == 'TR'), ]$CountryName_FromSource = 'Turkey'
newdata[which(newdata$CountryName_FromSource == 'RO'), ]$CountryName_FromSource = 'Romania'
newdata[which(newdata$CountryName_FromSource == 'PT'), ]$CountryName_FromSource = 'Portugal'
newdata[which(newdata$CountryName_FromSource == 'RMI'), ]$CountryName_FromSource = 'Marshall Island'
newdata[which(newdata$CountryName_FromSource == 'MT'), ]$CountryName_FromSource = 'USA'
newdata[which(newdata$CountryName_FromSource == 'MA'), ]$CountryName_FromSource = 'USA'
newdata[which(newdata$CountryName_FromSource == 'IL'), ]$CountryName_FromSource = 'USA'
newdata[which(newdata$CountryName_FromSource == 'IE'), ]$CountryName_FromSource = 'Ireland'
newdata[which(newdata$CountryName_FromSource == 'Micronesia, Federated States of'), ]$CountryName_FromSource = 'Micronesia'
newdata[which(newdata$CountryName_FromSource == 'Western Greece and the Ionian'), ]$CountryName_FromSource = 'Greece'
newdata[which(newdata$CountryName_FromSource == 'USVI'), ]$CountryName_FromSource = 'UK'
newdata[which(newdata$CountryName_FromSource == 'Saint Lucia'), ]$CountryName_FromSource = 'USA'


levels(factor(newdata$CountryName_FromSource))



#calcolo superficie totale, TotalArea_Sq_m sono tutti NA, pu? essere utile
for (i in 1:length(data$OBJECTID)) {
  data$TotalArea_Sq_m[i]=data$TotalWidth_m[i]*data$TotalLength_m[i]
}

#cosa stranissima, ad Hong Kong c'è il massimo del numero di volontari 188463
#ma con solo 1 oggetto raccolto, non è abbastanza strano?
#inoltre ci sono un sacco di NA anche in TotalWidth quindi non abbiamo tutte le aree
#e tanti NA anche in CountryName_FromSource quindi nell'aggregate sotto della Michi non ci sono
#tutti i raggrupamenti

#abbiamo due latitudini e due longitudini che sono praticamente uguali perchè una corrisponde
#alla zona e l'altra allo stato, possiamo decidere di tenere sono lo stato secondo me in modo 
#da poter fare un'analisi più "interna", tipo negli USA, analizzando i SubCountry

#"Day" non so quanto sia utile dato che rappresenta il giorno del mese ma non ci sono tutti
#per nessun mese e nessun stato, anzi alcuni giorni sono uguali e corrispondono a diverse zone
#secondo me possiamo eliminarlo 

#data$TotalClassifiedItems_EC2020 non ho capito cosa rappresenta
#CAMI(feat MICHI): rappresenta come alcuni rifiuti sono stati catalogati, per ora non è utile
#ma ce lo teniamo buono




###########
#SECTION GRAFICI (MICHI)
# prova dataset 2015 stati/mesi
dati_2015 = data[which(data$Year=="2015"), ]
stati = aggregate(dati_2015$Totalltems_EventRecord, by=list(Country=dati_2015$CountryName_FromSource, month=dati_2015$MonthNum), FUN=sum)
nomi_stati = levels(factor(dati_2015$CountryName_FromSource))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)
#length(nomi_stati) 2015=43, 2016=117, 2017=120, 2018=138
#da capire se tenere anche il 2015 o considerare solo gli ultimi tre anni dove abbiamo 
#pi? o meno lo stesso numero di stati

#dataframe stati/mesi
for (i in 1:12){
  stati_fda = cbind(stati_fda, zeros);
  temp = stati[which(stati$month==i), ]
  index = match(temp$Country, stati_fda$nomi_stati)
  stati_fda[index, i+1] = temp$x
}
names(stati_fda) =  as.character(c('Country', 1:12))

# plot functional data
quartz()
matplot(t(stati_fda[ ,-1]), type='l')
title('Country/Month 2015')
# con pacchetto roahd
data_fun = fData(1:12, stati_fda[ ,-1])
plot(data_fun)
title('Country/Month 2015')
# molto fikoh


#### provo su tutto dataset
stati = aggregate(data$Totalltems_EventRecord, by=list(Country=data$CountryName_FromSource, month=data$MonthNum, year=data$Year), FUN=sum)
nomi_stati = levels(factor(data$CountryName_FromSource)) #cosa sono gli stati "25000,42000,43000 e 92000"?
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

#dataframe stati/mesi
count = 1

for (y in 2015:2018){
  for (i in 1:12){
    stati_fda = cbind(stati_fda, zeros);
    temp = stati[which(stati$month==i & stati$year==y), ]
    index = match(temp$Country, stati_fda$nomi_stati)
    stati_fda[index, count+1] = temp$x
    count = count+1
  }
}
names(stati_fda) =  as.character(c('Country', 1:(12*4)))

# plot functional data
quartz()
matplot(t(stati_fda[ ,-1]), type='l')
title('Country/Month 2015-2018')
# con pacchetto roahd
data_fun = fData(1:(12*4), stati_fda[ ,-1])
quartz()
plot(data_fun)
title('Country/Month 2015-2018')

# palese outlier nel 2018, va trovato
stati[which(stati$x==max(stati$x)),1] #outlier Ghana

Ghana=stati_fda[which(stati_fda$Country=='Ghana'),] #61 riga del Ghana
sum(Ghana==0) #42 su 48, solo 6 valori in 4 anni e uno di questi ? il pi? alto tra tutti i paesi
matplot(t(stati_fda[-61 ,-1]), type='l')
title('Country/Month 2015-2018')

#nuovo dataset senza Ghana
stati_new=stati[-which(stati$Country=='Ghana'),]
stati_new[which(stati_new$x==max(stati_new$x)),1]

Philippines=stati_fda[which(stati_fda$Country=='Philippines'),] #119 riga delle Philippines
sum(Philippines==0) #22 su 48, 26 valori da met? del secondo anno in poi
matplot(t(stati_fda[-c(61,119) ,-1]), type='l')
title('Country/Month 2015-2018')

stati_new1=stati_new[-which(stati_new$Country=='Philippines'),]
stati_new1[which(stati_new1$x==max(stati_new1$x)),1]

USA=stati_fda[which(stati_fda$Country=='USA'),] #164 riga degli USA
sum(USA==0) #0 ci sono tutti i valori dei 48 mesi
matplot(t(stati_fda[-c(61,119,164) ,-1]), type='l')
title('Country/Month 2015-2018')

#plotto andamento USA
#USA magari caso particolare essendo davvero grande, magari utile analizzare i SubCountry
USA = stati_fda[which(stati_fda$Country=='USA'), ]
data_fun = fData(1:(12*4), USA[ ,-1])
plot(data_fun)
title('USA Monthly collected plastic')
# picchi raccolta a settembre/ottobre di ogni anno




