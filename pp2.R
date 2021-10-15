### PLASTIC POLLUTION ###
library(readr)
library(roahd)

setwd("C:/Users/rozzu/OneDrive/Desktop/NPS-project")
PP_2 <- read_csv("PP_2.csv")

data = PP_2[ , -c(1,2,7,9,11,23,24,25,28,29,30,37,38,43:83)]
# impongo unica classe USA anche per United States, va fatto per tutti i doppioni
data[which(data$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
data[which(data$CountryName_FromSource == 'Italy'), ]$CountryName_FromSource = 'IT'



# prova dataset 2015 stati/mesi
dati_2015 = data[which(data$Year=="2015"), ]
stati = aggregate(dati_2015$Totalltems_EventRecord, by=list(Country=dati_2015$CountryName_FromSource, month=dati_2015$MonthNum), FUN=sum)
nomi_stati = levels(factor(dati_2015$CountryName_FromSource))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)
#length(nomi_stati) 2015=43, 2016=117, 2017=120, 2018=138
#da capire se tenere anche il 2015 o considerare solo gli ultimi tre anni dove abbiamo 
#più o meno lo stesso numero di stati

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
sum(Ghana==0) #42 su 48, solo 6 valori in 4 anni e uno di questi è il più alto tra tutti i paesi
matplot(t(stati_fda[-61 ,-1]), type='l')
title('Country/Month 2015-2018')

#nuovo dataset senza Ghana
stati_new=stati[-which(stati$Country=='Ghana'),]
stati_new[which(stati_new$x==max(stati_new$x)),1]

Philippines=stati_fda[which(stati_fda$Country=='Philippines'),] #119 riga delle Philippines
sum(Philippines==0) #22 su 48, 26 valori da metà del secondo anno in poi
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


