setwd("~/Documents/GitHub/NPS-project")
library(rio)
library(roahd)
library(rgl)
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)

current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cleandata=import("cleandata.Rdata")
View(cleandata)

cleandata=cleandata[which(cleandata$Year!="2015"),] #dataset finale 38953 osservazioni, 2015 ha solo  1411 osservazioni
dim(cleandata[which(cleandata$Year=="2016"),])[1] #9273 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2017"),])[1] #12296 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2018"),])[1] #17384 osservazioni nel 2016

###FUNCTIONAL DATA
dati_2018 = cleandata[which(cleandata$Year=="2018"), ]
stati = aggregate(dati_2018$TotalItems, by=list(Country=dati_2018$Country, month=dati_2018$MonthNum), FUN=sum)
nomi_stati = levels(factor(dati_2018$Country))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

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


for (i in 1:127) {
  x=0;
  for (t in 1:36) {
    if(stati_fda[i,1+t]!=0)
      x=x+1;
  }
  if(x>=24)
    zeros[i]=1
}
sum(zeros) #11 stati su 127 che hanno ALMENO 24 mesi su 36 (2 anni su 3) di dati
#troppi stati hanno troppi mesi = 0 

# plot functional data
matplot(t(stati_fda[ ,-1]), type='l')
title('Country/Month 2016-2018')
# con pacchetto roahd
data_fun_stati = fData(1:(12*3), stati_fda[ ,-1])
plot(data_fun_stati)
title('Country/Month 2016-2018')

outliers_stati=outliergram(data_fun_stati, display = FALSE)
outliers_stati$ID_outliers #nessun outlier??

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

data_fun = fData(1:(12*3), stati_fda[-c(38,86,123) ,-1])
band_depth <- BD(Data = data_fun)
modified_band_depth <- MBD(Data = data_fun)

median_curve_manual <- data_fun[which.max(modified_band_depth),] # still an fData object

plot(data_fun)
grid_ecg <- seq(median_curve_manual$t0,median_curve_manual$tP,by=median_curve_manual$h)
lines(grid_ecg,median_curve_manual$values)

#plotto andamento USA
#USA magari caso particolare essendo davvero grande, il più grande, magari è utile analizzare i SubCountry
USA = stati_fda[which(stati_fda$Country=='USA'), ]
data_fun = fData(1:(12*3), USA[ ,-1])
plot(data_fun)
title('USA Monthly collected plastic')
# picchi raccolta a settembre/ottobre di ogni anno


USA=cleandata[which(cleandata$Country=='USA'),] #25188 dati su 38953 totali

subcountry = aggregate(USA$TotalItems, by=list(SubCountry=USA$SubCountry1, day=USA$Day, month=USA$MonthNum, year=USA$Year), FUN=sum)
nomi_subcountry = levels(factor(USA$SubCountry1)) 
zeros = rep(0, length(nomi_subcountry))
subcountry_fda = data.frame(nomi_subcountry)

#dataframe SubCountry/giorni
count = 1;
subcountry_fda = data.frame(matrix(NA,nrow=51,ncol=1096)) 
subcountry_fda[,1]=data.frame(nomi_subcountry)
names(subcountry_fda) =  as.character(c('SubCountry', rep(c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31),3)))

for (y in 2016:2018){
  for (x in 1:12){
    if(x==1||x==3||x==5||x==3||x==7||x==8||x==10||x==12){
      for (i in 1:31){
        temp = subcountry[which(subcountry$day==i & subcountry$month==x & subcountry$year==y), ]
        index = match(temp$SubCountry, subcountry_fda$SubCountry)
        subcountry_fda[index, count+1] = temp$x
        count = count+1
      }}
    if(x==2){
      for (i in 1:28){
        temp = subcountry[which(subcountry$day==i & subcountry$month==x & subcountry$year==y), ]
        index = match(temp$SubCountry, subcountry_fda$SubCountry)
        subcountry_fda[index, count+1] = temp$x
        count = count+1
      }}
    if(x==4||x==6||x==9||x==11){
      for (i in 1:30){
        temp = subcountry[which(subcountry$day==i & subcountry$month==x & subcountry$year==y), ]
        index = match(temp$SubCountry, subcountry_fda$SubCountry)
        subcountry_fda[index, count+1] = temp$x
        count = count+1
      }}
    
  }
}


data_fun = fData(1:(365*3), subcountry_fda[ ,-1])
plot(data_fun)
title('SubCountry/Days 2016-2018 of USA')
#tanti tanti giorni nulli

data_fun_subcountry = fData(1:(365*3), subcountry_fda[ ,-1])
plot(data_fun_subcountry)
title('SubCountry/Days 2016-2018')

outliers_subcountry=outliergram(data_fun_subcountry, display = FALSE)
outliers_subcountry$ID_outliers #5,10,35,48
subcountry_fda[c(outliers_subcountry$ID_outliers),1] #California, Florida, North Carolina, Washington

data_fun_subcountry_nooutlier = fData(1:(365*3), subcountry_fda[-c(outliers_subcountry$ID_outliers) ,-1])
plot(data_fun_subcountry_nooutlier)
title('SubCountry/Days 2016-2018')


OutliersSubCountry=subcountry_fda[c(outliers_subcountry$ID_outliers) ,]
data_fun_outliers = fData(1:(365*3), OutliersSubCountry[ ,-1])
plot(data_fun_outliers)
title('SubCountry/Days 2016-2018 Outliers')



stati = aggregate(USA$TotalItems, by=list(SubCountry=USA$SubCountry1, month=USA$MonthNum, year=USA$Year), FUN=sum)
nomi_stati = levels(factor(USA$SubCountry1)) 
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

#dataframe stati/mesi
count = 1

for (y in 2016:2018){
  for (i in 1:12){
    stati_fda = cbind(stati_fda, zeros);
    temp = stati[which(stati$month==i & stati$year==y), ]
    index = match(temp$SubCountry, stati_fda$nomi_stati)
    stati_fda[index, count+1] = temp$x
    count = count+1
  }
}
names(stati_fda) =  as.character(c('SubCountry', 1:(12*3)))


data_fun_stati = fData(1:(12*3), stati_fda[ ,-1])
plot(data_fun_stati)
title('SubCountry/Months 2016-2018 of USA')