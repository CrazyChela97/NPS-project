####GRAPHIC ANALYSIS
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

USA=cleandata[which(cleandata$Country=='USA'),]

#ratio plot
ratio=rep(0,length(USA$TotalItems))
for (i in 1:length(USA$TotalItems)) {
  ratio[i]=USA$TotalItems[i]/USA$TotalVolunteers[i]
}
ratio=as.data.frame(ratio)
USA=cbind(USA,ratio)


plot(USA$Area,USA$ratio)

u=USA[,c(19,20)]

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
cleanUSA <- u[-ind_outlying_obs,] #-4639 outliers TOP

plot(cleanUSA)

#plot Items vs Volunteers

u=USA[,c(8,14)]
plot(u)

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
cleanUSA <- u[-ind_outlying_obs,] #-9235 outliers, abbiamo comunque 15953 dati ma possiamo togliere 9k dati? 
#da chiedere e capire come risolvere

par(mfrow=c(1,2))
plot(u)
plot(cleanUSA)

CleanUsa=USA[-ind_outlying_obs,]
save(CleanUsa,file="cleanUSA.Rdata")

