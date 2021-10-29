### PLASTIC POLLUTION ###
library(readr)
library(roahd)
library(dplyr)
library(stringr)

current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
PP_2 <- read_csv("PP_2.csv")

#SECTION PULIZIA E PROBLEMI (MICHAEL E CAMI)

###SUBSECTION: pulizia kontatti!!11!!
##TODO: ricordarsi di cambiare i nomi delle colonne di sto dataset perchè fanno vomitare
##TODO: scatterplot cose raccolte vs num di volontari


data = PP_2[ , -c(1,2,4,5,6,7,9,11,22,23,24,25,28,29,30,35,37,38,43:73,75,76,78:83)]
data=data[,-c(11,12,13)]
#cosa stranissima, ad Hong Kong c'Ã¨ il massimo del numero di volontari 188463
#ma con solo 1 oggetto raccolto, non Ã¨ abbastanza strano?

#abbiamo due latitudini e due longitudini che sono praticamente uguali perchÃ¨ una corrisponde
#alla zona e l'altra allo stato, possiamo decidere di tenere sono lo stato secondo me in modo 
#da poter fare un'analisi piÃ¹ "interna", tipo negli USA, analizzando i SubCountry

#"Day" non so quanto sia utile dato che rappresenta il giorno del mese ma non ci sono tutti
#per nessun mese e nessun stato, anzi alcuni giorni sono uguali e corrispondono a diverse zone
#secondo me possiamo eliminarlo 

#data$TotalClassifiedItems_EC2020 non ho capito cosa rappresenta
#CAMI(feat MICHI): rappresenta come alcuni rifiuti sono stati catalogati, per ora non Ã¨ utile
#ma ce lo teniamo buono

#"Organization" ci serve? ci sono un sacco di NA che non possiamo recuperare


#CAMI: sposto questa parte qua cosÃ¬ pulisco prima i NAN
sum(is.na(data$TotalVolunteers))
newdata <- data[!is.na(data$TotalVolunteers),] #-2418 dati
View(newdata)


#CAMI: per ora metto in commentato l'area, mi concentro su nome stato, data e q.tÃ  raccolta
#per fortuna non ho NAN in TotalItems_EventRecord


levels(factor(newdata$CountryName_FromSource))

#CAMI: queste etichette andavano cambiate per omonimia o cose no sense(tipo oslo municipality)
#impongo unica classe USA anche per United States, va fatto per tutti i doppioni
newdata[which(newdata$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
newdata[which(newdata$CountryName_FromSource == 'Italy'), ]$CountryName_FromSource = 'IT'
#CAMI: ci sono un sacco di etichette strane per gli stati, vediamo come fare
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
newdata[which(newdata$CountryName_FromSource == 'Saint Lucia'), ]$CountryName_FromSource = 'St Lucia'
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
#alla fine avremo un dataset che ha tutta la colonna Location,
#tutta la colonna CountryName e almeno una tra i due SubCountry_L1/L2

#sotto codice dove chi non ha CountryName_FromSource ma ha Location, mette CountryName_FromSource=l'ultima "parola"/"sigla" 
#della stringa "Location" che corrisponde al Country (ho controllato uno per uno)
#(considero stringhe Location diverse da 1, le faccio dopo, capire il perchè vedendo il codice sotto ma il succo 
#è che la singola stringa non corrisponde sempre al Country ma a volte a un SubCountry)

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
length(location$zona) #ora 3603 location (prima 3337), i 5520 NA di prima hanno dato 266 zone in più
#ora length(newdata$Location)-sum(location$x)= 0 NA

#codice sotto: ho aggiunto con la pazienza di Dio a chi aveva solo una "parola" in Location
#e nulla in CountryName_FromSource il corrispettivo CountryName_FromSource
for (i in 1:length(newdata$Location)) {
  if((length(unlist(strsplit(newdata$Location[i], ", ")))==1)&(is.na(newdata$CountryName_FromSource[i])==TRUE)) #questa riga è inutile, l'ho usata per identificare quali erano quelli con location con stringa singola e Country NA
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
    if(newdata$Location[i]=="CuraÃ§ao")
      newdata$CountryName_FromSource[i]="CuraÃ§ao"
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
#finalmente abbiamo un dataset senza NA nè nella colonna Location nè nella colonna CountryName_FromSource

newdata=newdata[,-c(1,3,9,10)]
colnames(newdata)[c(2,3,4,14,16,17,18,19)] <- c("Country","SubCountry1","SubCountry2","TotalItems","%Plastic&Foam","%GlassRubberLumberMetal","Continent","Area")


#alcuni SubCountry_L1 sono a sigla altri nome intero tipo CA=California, bisogna con pazienza 
#metterli uguali per poter magari fare degli aggregate o confrontare stessi SubCountry
#a prima occhiata sembrano solo i primi 2121 da aggiustare che sono con la sigla mentre tutti gli
#altri hanno il nome completo

sum(is.na(newdata$SubCountry1)==TRUE) #c'è solo 1 SubCountry1 che è NA
which(is.na(newdata$SubCountry1)==TRUE) #riga 1206
newdata$SubCountry1[1206]="NA"
sum(is.na(newdata$SubCountry2)==TRUE) #1569 praticamente abbiamo quasi tutto il dataset con tutte le 4 colonne
#Location, Country, SubCountry1 e SubCountry2. 

for (i in 1:length(newdata$Location)) {
  if(newdata$SubCountry1[i]=="CA")
    newdata$SubCountry1[i]="California"
  if(newdata$SubCountry1[i]=="OR")
    newdata$SubCountry1[i]="Oregon"
  if(newdata$SubCountry1[i]=="WA")
    newdata$SubCountry1[i]="Washington"
  if(newdata$SubCountry1[i]=="AK")
    newdata$SubCountry1[i]="Alaska"
  if(newdata$SubCountry1[i]=="HI")
    newdata$SubCountry1[i]="Hawaii"
  if(newdata$SubCountry1[i]=="VA")
    newdata$SubCountry1[i]="Virginia"
  if(newdata$SubCountry1[i]=="TX")
    newdata$SubCountry1[i]="Texas"
  if(newdata$SubCountry1[i]=="Or")
    newdata$SubCountry1[i]="Oregon"
  if(newdata$SubCountry1[i]=="NJ")
    newdata$SubCountry1[i]="New Jersey"
  if(newdata$SubCountry1[i]=="Hi")
    newdata$SubCountry1[i]="Hawaii"
  if(newdata$SubCountry1[i]=="FL")
    newdata$SubCountry1[i]="Florida"
  if(newdata$SubCountry1[i]=="BC")
    newdata$SubCountry1[i]="British Columbia"
  if(newdata$SubCountry1[i]=="MA")
    newdata$SubCountry1[i]="Manabí"
  if(newdata$SubCountry1[i]=="Ng")
    newdata$SubCountry1[i]="Ngerkeklau"
  if(newdata$SubCountry1[i]=="AL")
    newdata$SubCountry1[i]="Alabama"
  if(newdata$SubCountry1[i]=="MS")
    newdata$SubCountry1[i]="Mississippi"
  if(newdata$SubCountry1[i]=="LA")
    newdata$SubCountry1[i]="Louisiana"
  if(newdata$SubCountry1[i]=="Ha")
    newdata$SubCountry1[i]="Hawaii"
  if(newdata$SubCountry1[i]=="Ya")
    newdata$SubCountry1[i]="Yap"
  if(newdata$SubCountry1[i]=="NA")
    newdata$SubCountry1[i]="Neiafu"
  if(newdata$SubCountry1[i]=="ME")
    newdata$SubCountry1[i]="Maine"
  if(newdata$SubCountry1[i]=="Al")
    newdata$SubCountry1[i]="Maine"
  if(newdata$SubCountry1[i]=="JA")
    newdata$SubCountry1[i]="Jalisco"
  if(newdata$SubCountry1[i]=="NC")
    newdata$SubCountry1[i]="North Carolina"
  if(newdata$SubCountry1[i]=="WAL")
    newdata$SubCountry1[i]="Wallonia"
}

#anomalia: alcune spedizioni hanno zero item raccolti, ovviamente le elimino
#idem per zero persone e item diversi da zero
newdata=newdata[which(newdata$TotalVolunteers != 0),] #-60
newdata=newdata[which(newdata$TotalItems != 0),] #-2201

table(as.factor(newdata$Country))

#righe ripetute 
newdata=newdata[!duplicated(newdata), ]#-4319
newdata=newdata[!duplicated(newdata[,c(1,2,3,4,7,9,10,11,12,13,14,15,16,17)]),] 

##PARENTESI COSI' SI SALVANO LE COSE
#save(newdata,file="cleandata.Rdata")
##COSI' SI RIAPRONO
#library(rio)
#cleandata=import("cleandata.Rdata")




