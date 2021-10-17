### PLASTIC POLLUTION ###
library(readr)
library(roahd)
library(dplyr)
library(stringr)

setwd("~/Documents/Politecnico/Magistrale/Non Parametric Statistics/PROJECT")
PP_2 <- read_csv("PP_2.csv")

#SECTION PULIZIA E PROBLEMI (MICHAEL E CAMI)

###SUBSECTION: pulizia kontatti!!11!!
##TODO: ricordarsi di cambiare i nomi delle colonne di sto dataset perch� fanno vomitare
##TODO: scatterplot cose raccolte vs num di volontari


data = PP_2[ , -c(1,2,4,5,6,7,9,11,22,23,24,25,28,29,30,35,37,38,43:83)]
<<<<<<< HEAD
# impongo unica classe USA anche per United States, va fatto per tutti i doppioni
data[which(data$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
data[which(data$CountryName_FromSource == 'IT'), ]$CountryName_FromSource = 'Italy'
#CAMI: ci sono un sacco di etichette strane per gli stati, vediamo come fare
=======

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

#"Organization" ci serve? ci sono un sacco di NA che non possiamo recuperare

>>>>>>> aba0c5c58dda8827965b30c80df386ad8df3eab0

#CAMI: sposto questa parte qua così pulisco prima i NAN
sum(is.na(data$TotalVolunteers))
newdata <- data[!is.na(data$TotalVolunteers),] #-2418 dati
View(newdata)


#CAMI: per ora metto in commentato l'area, mi concentro su nome stato, data e q.tà raccolta
#per fortuna non ho NAN in TotalItems_EventRecord


levels(factor(newdata$CountryName_FromSource))

#CAMI: queste etichette andavano cambiate per omonimia o cose no sense(tipo oslo municipality)
#impongo unica classe USA anche per United States, va fatto per tutti i doppioni
data[which(data$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
data[which(data$CountryName_FromSource == 'Italy'), ]$CountryName_FromSource = 'IT'
#CAMI: ci sono un sacco di etichette strane per gli stati, vediamo come fare
newdata[which(newdata$CountryName_FromSource == 'BA'), ]$CountryName_FromSource = 'Bosnia Erzegovina'
newdata[which(newdata$CountryName_FromSource == 'BE'), ]$CountryName_FromSource = 'Belgium'
newdata[which(newdata$CountryName_FromSource == 'BG'), ]$CountryName_FromSource = 'Bulgaria'
newdata[which(newdata$CountryName_FromSource == 'CH'), ]$CountryName_FromSource = 'Switzerland'
newdata[which(newdata$CountryName_FromSource == "Côte d'Ivoire"), ]$CountryName_FromSource = 'Ivory Cost'
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
newdata[which(newdata$CountryName_FromSource == 'Federal Territory of Kuala Lumpur'), ]$CountryName_FromSource = 'Malaysia'
newdata[which(newdata$CountryName_FromSource == 'Madrid'), ]$CountryName_FromSource = 'Spain'
newdata[which(newdata$CountryName_FromSource == 'Hong Kong'), ]$CountryName_FromSource = 'China'
newdata[which(newdata$CountryName_FromSource == 'MalÃ©'), ]$CountryName_FromSource = 'Maldives'
newdata[which(newdata$CountryName_FromSource == 'Macedonia (FYROM)'), ]$CountryName_FromSource = 'Macedonia'


levels(factor(newdata$CountryName_FromSource))

#eliminazione di cose no sense
newdata=newdata[!(newdata$CountryName_FromSource=='25000'),]
newdata=newdata[!(newdata$CountryName_FromSource=='42000'),]
newdata=newdata[!(newdata$CountryName_FromSource=='43000'),]
newdata=newdata[!(newdata$CountryName_FromSource=='92000'),]
newdata=newdata[!(newdata$CountryName_FromSource=='ASCN 1ZZ'),]
newdata=newdata[!(newdata$CountryName_FromSource=='STHL 1ZZ'),]


##SEMBRA OKAY ORA
#facciamo qualche analisi: prima cosa faccio uno scatterplot
#num volontari vs qtà plastica raccolta

plot(newdata$TotalVolunteers,newdata$Totalltems_EventRecord)


##PARENTESI COSI' SI SALVANO LE COSE
#save(newdata,file="cleandata.Rdata")
##COSI' SI RIAPRONO
#library(rio)
#dati=import("cleandata.Rdata")




#Michael

location=aggregate(newdata$Location, by=list(zona=newdata$Location), FUN=length)
length(location$zona) #3337, ci sono 3337 location, ma alcune Location sono NA ma magari hanno 
#la sigla dello stato o il nome del SubCountry_L1/L2
#esattamente length(newdata$Location)-sum(location$x)= 5520 NA

#alcuni hanno NA sia in location che in CountryName che in SubCountry_L1/L2 quindi li tolgo
#per ora lavoro sul newdata per poi capire che fare con quelli senza TotalWidth
newdata <- newdata[!(is.na(newdata$Location)&is.na(newdata$CountryName_FromSource)&is.na(newdata$SubCountry_L1_FromSource)&is.na(newdata$SubCountry_L2_FromSource)),]
#51952 prima - 48001 ora =  3951 con NA nelle quattro colonne 
length(na.omit(newdata$Location)) #1569 senza Location ma almeno uno tra CountryName, SubCountry_L1/L2
#o solo 1, o 2, o tutte 3 

#riempio almeno tutta la colonna Location e CountryName che secondo me saranno quelle che useremo  
#secondo me si possono eliminare i SubCountry_L1/L2 che hanno entrambi NA perch? non si possono recuperare, 
#a differenza del CountryName che si recupera da chi ha almeno uno tra Location, SubCountry_L1/L2,
#inoltre abbiamo Latitudine e Longitudine di almeno un SubCountry che sono pressoch? identici quindi
#non c'? bisogno di averli entrambi, quindi alla fine avremo un dataset che ha tutta la colonna Location,
#tutta la colonna CountryName e almeno una tra i due SubCountry_L1/L2

#sotto codice dove chi non ha CountryName_FromSource ma ha Location, mette CountryName_FromSource=l'ultima "parola"/"sigla" 
#della stringa "Location" che corrisponde al Country (ho controllato uno per uno)
#(considero stringhe Location diverse da 1, le faccio dopo, capire il perch? vedendo il codice sotto ma il succo 
#? che la singola stringa non corrisponde sempre al Country ma a volte a un SubCountry)

for (i in 1:length(newdata$Location)) {
  if((is.na(newdata$Location)[i]==FALSE)&(length(unlist(strsplit(newdata$Location[i], ", ")))!=1)&(is.na(newdata$CountryName_FromSource[i])==TRUE))
    newdata$CountryName_FromSource[i]=last(unlist(strsplit(newdata$Location[i], ", ")))
}  

#sotto il codice che riempie tutte le righe con Location NA
for (i in 1:length(newdata$OBJECTID)) {
  if(is.na(newdata$Location)[i]==TRUE){
    if((is.na(newdata$CountryName_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==FALSE))
      newdata$Location[i]=paste(newdata$SubCountry_L1_FromSource[i],newdata$SubCountry_L2_FromSource[i],newdata$CountryName_FromSource[i], sep = ", ")
    if((is.na(newdata$CountryName_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==TRUE))
      newdata$Location[i]=paste(newdata$SubCountry_L1_FromSource[i],newdata$CountryName_FromSource[i], sep = ", ")
    if((is.na(newdata$CountryName_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==FALSE))
      newdata$Location[i]=paste(newdata$SubCountry_L2_FromSource[i],newdata$CountryName_FromSource[i], sep = ", ")
    if((is.na(newdata$CountryName_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==FALSE))
      newdata$Location[i]=paste(newdata$SubCountry_L1_FromSource[i],newdata$SubCountry_L2_FromSource[i], sep = ", ")
    if((is.na(newdata$CountryName_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==FALSE))
      newdata$Location[i]=paste(newdata$SubCountry_L2_FromSource[i])
    if((is.na(newdata$CountryName_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==TRUE))
      newdata$Location[i]=paste(newdata$SubCountry_L1_FromSource[i])
    if((is.na(newdata$CountryName_FromSource)[i]==FALSE)&(is.na(newdata$SubCountry_L1_FromSource)[i]==TRUE)&(is.na(newdata$SubCountry_L2_FromSource)[i]==TRUE))
      newdata$Location[i]=paste(newdata$CountryName_FromSource[i])
  }
}
#ora tutte le righe hanno la Location
location=aggregate(newdata$Location, by=list(zona=newdata$Location), FUN=length)
length(location$zona) #ora 3604 location (prima 3337), i 5520 NA di prima hanno dato 267 zone in pi?
#ora length(newdata$Location)-sum(location$x)= 0 NA

#codice sotto: ho aggiunto con la pazienza di Dio a chi aveva solo una "parola" in Location
#e nulla in CountryName_FromSource il corrispettivo CountryName_FromSource
for (i in 1:length(newdata$Location)) {
  if((length(unlist(strsplit(newdata$Location[i], ", ")))==1)&(is.na(newdata$CountryName_FromSource[i])==TRUE))
    if(newdata$Location[i]=="Singapore")
      newdata$CountryName_FromSource[i]="Singapore"
    if(newdata$Location[i]=="Hong Kong")
      newdata$CountryName_FromSource[i]="Hong Kong"
    if(newdata$Location[i]=="Fujairah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Dubai - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Jeddah Saudi Arabia")
      newdata$CountryName_FromSource[i]="Saudi Arabia"
    if(newdata$Location[i]=="Dibba Al Fujairah - Fujairah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Dhahran Saudi Arabia")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Curaçao")
      newdata$CountryName_FromSource[i]="Curaçao"
    if(newdata$Location[i]=="Panama")
      newdata$CountryName_FromSource[i]="Panama"
    if(newdata$Location[i]=="Umm Al Quwain - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Famagusta")
      newdata$CountryName_FromSource[i]="Cyprus"
    if(newdata$Location[i]=="Tuzla")
      newdata$CountryName_FromSource[i]="Bosnia and Herzegovina"
    if(newdata$Location[i]=="Collectivity of Saint Martin")
      newdata$CountryName_FromSource[i]="Collectivity of Saint Martin"
    if(newdata$Location[i]=="Abu Dhabi - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Sharjah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Macau")
      newdata$CountryName_FromSource[i]="Macau"
    if(newdata$Location[i]=="United States")
      newdata$CountryName_FromSource[i]="	United States"
    if(newdata$Location[i]=="Dhadna - Fujairah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Ajman - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Khor Fakkan - Sharjah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
    if(newdata$Location[i]=="Eastern Province Saudi Arabia")
      newdata$CountryName_FromSource[i]="Saudi Arabia"
    if(newdata$Location[i]=="Vasileia")
      newdata$CountryName_FromSource[i]="Cyprus"
    if(newdata$Location[i]=="Al Aqah - Fujairah - United Arab Emirates")
      newdata$CountryName_FromSource[i]="United Arab Emirates"
}

nrow(newdata[na.omit(newdata$CountryName_FromSource),]) #48001
nrow(newdata[na.omit(newdata$Location),]) #48001
#finalmente abbiamo un dataset senza NA n� nella colonna Location n� nella colonna CountryName_FromSource

newdata=newdata[,-c(1,3,11,13)]
colnames(newdata)[c(2,3,4,9,17,19,20)] <- c("Country","SubCountry1","SubCountry2","Length","TotalItems","%Plastic&Foam","%GlassRubberLumberMetal")


#alcuni SubCountry_L1 sono a sigla altri nome intero tipo CA=California, bisogna con pazienza 
#metterli uguali per poter magari fare degli aggregate o confrontare stessi SubCountry
#a prima occhiata sembrano solo i primi 2121 da aggiustare che sono con la sigla mentre tutti gli
#altri hanno il nome completo


#####SUBSECTION: ANALISI AREA (Michael)

<<<<<<< HEAD
# newdata1 <- newdata[!is.na(newdata$TotalWidth_m),] #-49894 dati!! contiene solo 2121 dati
#significa che 49894 righe non hanno il dato larghezza e quindi non possiamo calcolare 
#la superificie quindi rende le tre colonne larghezza, lunghezza e superficie inutili, 
#questo secondo me ? un problema perch? avremmo potuto fare delle analisi tipo volontari vs area
#o quantit? di plastica vs area


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

#"Organization" ci serve? ci sono un sacco di NA che non possiamo recuperare
=======
#sum(newdata$Length==0) 3328 con lunghezza 0
#capire se possiamo fare qualcosa con la lunghezza e quindi eliminare queste 3328 righe o invece eliminare 
#la colonna Length
>>>>>>> aba0c5c58dda8827965b30c80df386ad8df3eab0


###########
#SECTION GRAFICI (MICHI, Michael(outlier))
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
matplot(t(stati_fda[ ,-1]), type='l')
title('Country/Month 2015-2018')
# con pacchetto roahd
data_fun = fData(1:(12*4), stati_fda[ ,-1])
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

