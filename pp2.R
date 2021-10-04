### PLASTIC POLLUTION ###
library(readr)
library(roahd)

setwd("/Users/michelafrigeri/Downloads/plastic_pollution_2")
PP_2 <- read_csv("PP_2.csv")
data = PP_2[ , -c(1,2,7,11,23,24,25,28,29,30,37,38,43:83)]
# impongo unica classe USA anche per United States, va fatto per tutti i doppioni
data[which(data$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
data[which(data$CountryName_FromSource == 'Italy'), ]$CountryName_FromSource = 'IT'



# prova dataset 2015 stati/mesi
dati_2015 = data[which(data$Year==2015), ]
stati = aggregate(dati_2015$Totalltems_EventRecord, by=list(Country=dati_2015$CountryName_FromSource, month=dati_2015$MonthNum), FUN=sum)
nomi_stati = levels(factor(dati_2015$CountryName_FromSource))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)

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
nomi_stati = levels(factor(data$CountryName_FromSource))
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


# plotto andamento USA
USA = stati_fda[which(stati_fda$Country=='USA'), ]
data_fun = fData(1:(12*4), USA[ ,-1])
plot(data_fun)
title('USA Monthly collected plastic')
# picchi raccolta a settembre/ottobre di ogni anno

