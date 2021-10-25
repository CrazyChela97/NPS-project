####GRAPHIC ANALYSIS
setwd("~/Documents/GitHub/NPS-project")

load("~/Documents/GitHub/NPS-project/cleandata.Rdata")
View(cleandata)


##1)prima cosa faccio uno scatterplot
#num volontari vs qtà plastica raccolta
quartz()
plot(cleandata$TotalVolunteers,cleandata$Totalltems_EventRecord)

sort(cleandata$TotalVolunteers, decreasing = TRUE)
sort(cleandata$Totalltems_EventRecord, decreasing = TRUE)

noout=cleandata[which(cleandata$TotalVolunteers < 5000),]
noout=noout[which(noout$Totalltems_EventRecord < 100000),]

#anomalia: alcune spedizioni hanno zero item raccolti, ovviamente le elimino
#idem per zero persone e item diversi da zero
noout=noout[which(noout$Totalltems_EventRecord != 0),]
noout=noout[which(noout$TotalVolunteers != 0),]

plot(noout$TotalVolunteers,(noout$Totalltems_EventRecord), main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#metto un logaritmo solo nella quantità di plastica raccolta
#non lo metto anche per i volontari perchè mi sembra concettualmente sbagliato (num volontari è intero)


##altrimenti
zoom=noout[which(noout$TotalVolunteers < 500),]
zoom=zoom[which(zoom$Totalltems_EventRecord < 10000),]
plot(zoom$TotalVolunteers,(zoom$Totalltems_EventRecord), main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#meglio ancora!









