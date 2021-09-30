### PLASTIC POLLUTION ###
library(readr)

setwd("/Users/michelafrigeri/Downloads/plastic_pollution_2")
PP_2 <- read_csv("PP_2.csv")
data = PP_2[ , -c(1,2,7,11,23,24,25,28,29,30,37,38,43:83)]

# prova dataset 2015 per mese
dati_2015 = data[which(data$Year==2015), ]
i = 6
prova = dati_2015
# impongo unica classe USA anche per United States, va fatto per tutti i doppioni
prova[which(prova$CountryName_FromSource == 'United States'), ]$CountryName_FromSource = 'USA'
stati = aggregate(prova$Totalltems_EventRecord, by=list(Country=prova$CountryName_FromSource, month=prova$MonthNum), FUN=sum)
nomi_stati = levels(factor(prova$CountryName_FromSource))
zeros = rep(0, length(nomi_stati))
stati_fda = data.frame(nomi_stati)
# voglio fare un dataframe stati/mesi ma non riesco :(