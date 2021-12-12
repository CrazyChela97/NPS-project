####GRAPHIC ANALYSIS

# NB : da sistemare perchè è cambiato dataset! 
#      poi va incorporato al file 'Outliers_Detection'

setwd("~/Documents/GitHub/NPS-project")

load("~/Documents/GitHub/NPS-project/cleandata.Rdata")
View(cleandata)


##1)prima cosa faccio uno scatterplot
#num volontari vs qtà plastica raccolta
quartz()
plot(cleandata$TotalVolunteers,cleandata$TotalItems)

sort(cleandata$TotalVolunteers, decreasing = TRUE)
sort(cleandata$TotalItems, decreasing = TRUE)

noout=cleandata[which(cleandata$TotalVolunteers < 5000),]
noout=noout[which(noout$TotalItems < 100000),]

#anomalia: alcune spedizioni hanno zero item raccolti, ovviamente le elimino
#idem per zero persone e item diversi da zero
noout=noout[which(noout$TotalItems != 0),]
noout=noout[which(noout$TotalVolunteers != 0),]

plot(noout$TotalVolunteers,(noout$TotalItems), main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#metto un logaritmo solo nella quantità di plastica raccolta
#non lo metto anche per i volontari perchè mi sembra concettualmente sbagliato (num volontari è intero)


##altrimenti
zoom=noout[which(noout$TotalVolunteers < 500),]
zoom=zoom[which(zoom$Totalltems_EventRecord < 10000),]
plot(zoom$TotalVolunteers,(zoom$Totalltems_EventRecord), main="Human resouches VS Items collected",
     xlab="Num of total volunteers", ylab="Items recorded")
#meglio ancora!

##depth mearures!!
#prendo un dato bivariato

bivariate = noout[,c(15,21)]
biv.zoom=zoom[,c(15,21)]
bivariate = as.matrix(bivariate) #levels = 99
biv.zoom=as.matrix(biv.zoom) #levels = 49
bivariate.sd = scale(bivariate)

#riga 116 fino a 171

#l'ideale sarebbe rifare tutto con noout
#forse userò zoom
depthContour(
  bivariate,
  depth_params = list(method = 'Tukey'),
  points = FALSE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 99,
  pdmedian = F,
  graph_params = list(cex=0.01, pch=1),
  pmean = F
)

bagplot(bivariate)

#linguaggio ispirato a SQL per selezionare gli outliers
aplpack::bagplot(bivariate,show.whiskers = F,main="Bagplot")
aplpack::bagplot(bivariate,show.loophull = F,main="Sunburst plot")

bagplot_biv<- bagplot(bivariate)
outlying_obs <- bagplot_biv$pxy.outlier
outlying_obs
#mi dice lui come raffinare ancora noout

outlying_obs=as.matrix(outlying_obs)

ind_outlying_obs <- which(apply(bivariate,1,function(x) all(x %in% outlying_obs)))
raffinato <- bivariate[-ind_outlying_obs,]

raffinato=as.matrix(raffinato)
depthContour(
  raffinato,
  depth_params = list(method = 'Tukey'),
  points = FALSE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 99,
  pdmedian = F,
  graph_params = list(cex=0.01, pch=1),
  pmean = F
)

plot(raffinato)
#molto meglio di zoom!!




