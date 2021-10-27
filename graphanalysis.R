####GRAPHIC ANALYSIS
setwd("~/Documents/GitHub/NPS-project")
library(rio)
library(roahd)

load("~/Documents/GitHub/NPS-project/cleandata.Rdata")
cleandata=import("cleandata.Rdata")
View(cleandata)


##1)prima cosa faccio uno scatterplot
#num volontari vs quantità plastica raccolta
quartz()
plot(cleandata$TotalVolunteers,cleandata$TotalItems)

sort(cleandata$TotalVolunteers, decreasing = TRUE)
sort(cleandata$TotalItems, decreasing = TRUE)

noout=cleandata[which(cleandata$TotalVolunteers < 5000),]
noout=noout[which(noout$TotalItems < 100000),]


plot(noout$TotalVolunteers,noout$TotalItems, main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#metto un logaritmo solo nella quantitÃ  di plastica raccolta
#non lo metto anche per i volontari perchÃ¨ mi sembra concettualmente sbagliato (num volontari Ã¨ intero)


##altrimenti
zoom=noout[which(noout$TotalVolunteers < 500),]
zoom=zoom[which(zoom$TotalItems < 10000),]
plot(zoom$TotalVolunteers,zoom$TotalItems, main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#meglio ancora!


###########
#SECTION GRAFICI (MICHI, Michael(outlier))
dati_2018 = cleandata[which(cleandata$Year=="2018"), ]
stati = aggregate(dati_2018$TotalItems, by=list(Country=dati_2018$Country, month=dati_2018$MonthNum), FUN=sum)
nomi_stati = levels(factor(dati_2018$Country))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

#length(nomi_stati) 2015=20, 2016=93, 2017=100, 2018=109
#consideriamo solo gli ultimi tre anni dove abbiamo più o meno lo stesso numero di stati
cleandata=cleandata[which(cleandata$Year!="2015"),] #dataset finale 39272 osservazioni, 2015 ha solo 1394 osservazioni
dim(cleandata[which(cleandata$Year=="2016"),])[1] #9288 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2017"),])[1] #12378 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2018"),])[1] #17606 osservazioni nel 2016

#new dataset
stati = aggregate(cleandata$TotalItems, by=list(Country=cleandata$Country, month=cleandata$MonthNum, year=cleandata$Year), FUN=sum)
nomi_stati = levels(factor(cleandata$Country)) 
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

#dataframe stati/mesi
count = 1

for (y in 2016:2018){
  for (i in 1:12){
    stati_fda = cbind(stati_fda, zeros);
    temp = stati[which(stati$month==i & stati$year==y), ]
    index = match(temp$Country, stati_fda$nomi_stati)
    stati_fda[index, count+1] = temp$x
    count = count+1
  }
}
names(stati_fda) =  as.character(c('Country', 1:(12*3)))

# plot functional data
matplot(t(stati_fda[ ,-1]), type='l')
title('Country/Month 2016-2018')
# con pacchetto roahd
data_fun = fData(1:(12*3), stati_fda[ ,-1])
plot(data_fun)
title('Country/Month 2016-2018')

# palese outlier nel 2018, va trovato
stati[which(stati$x==max(stati$x)),1] #outlier Ghana

Ghana=stati_fda[which(stati_fda$Country=='Ghana'),] #38 riga del Ghana
sum(Ghana==0) #30 su 36, solo 6 valori in 3 anni e uno di questi è il più alto tra tutti i paesi
matplot(t(stati_fda[-38 ,-1]), type='l')
title('Country/Month 2016-2018')

#nuovo dataset senza Ghana
stati_new=stati[-which(stati$Country=='Ghana'),]
stati_new[which(stati_new$x==max(stati_new$x)),1]

Philippines=stati_fda[which(stati_fda$Country=='Philippines'),] #86 riga delle Philippines
sum(Philippines==0) #10 su 36, 26 valori da metà del secondo anno in poi
matplot(t(stati_fda[-c(38,86) ,-1]), type='l')
title('Country/Month 2016-2018')

stati_new1=stati_new[-which(stati_new$Country=='Philippines'),]
stati_new1[which(stati_new1$x==max(stati_new1$x)),1]

USA=stati_fda[which(stati_fda$Country=='USA'),] #123 riga degli USA
sum(USA==0) #0 ci sono tutti i valori dei 36 mesi
matplot(t(stati_fda[-c(38,86,123) ,-1]), type='l') #stupendo 
title('Country/Month 2016-2018')

#plotto andamento USA
#USA magari caso particolare essendo davvero grande, il più grande, magari è utile analizzare i SubCountry
USA = stati_fda[which(stati_fda$Country=='USA'), ]
data_fun = fData(1:(12*3), USA[ ,-1])
plot(data_fun)
title('USA Monthly collected plastic')
# picchi raccolta a settembre/ottobre di ogni anno






