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

##1)prima cosa faccio uno scatterplot
#num volontari vs quantità plastica raccolta
quartz()
plot(cleandata$TotalVolunteers,cleandata$TotalItems)

bin=hexbin(cleandata$TotalVolunteers,cleandata$TotalItems, xbins=10, xlab="TotalVolunteers", ylab="TotalItems")
plot(bin, main="Hexagonal Binning") 

u=cleandata[,c(8,14)]
tukey_depth=depth(u,method='Tukey')
depthMedian(u,depth_params = list(method='Tukey'))
u[which.max(tukey_depth),]
depthContour(u,depth_params = list(method='Tukey'))

depthPersp(u,depth_params = list(method='Tukey'))

depthPersp(u,depth_params = list(method='Tukey'),plot_method = 'rgl')

maha_depth <- depth(u,method='Mahalanobis') 

depthMedian(u,depth_params = list(method='Mahalanobis'))

depthContour(u,depth_params = list(method='Mahalanobis'))

depthPersp(u,depth_params = list(method='Mahalanobis'))

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
clean <- u[-ind_outlying_obs,]

tukey_depth=depth(clean,method='Tukey')
depthMedian(clean,depth_params = list(method='Tukey'))
clean[which.max(tukey_depth),]
depthContour(clean,depth_params = list(method='Tukey'))

depthPersp(clean,depth_params = list(method='Tukey'))

depthPersp(clean,depth_params = list(method='Tukey'),plot_method = 'rgl')

maha_depth <- depth(clean,method='Mahalanobis') 

depthMedian(clean,depth_params = list(method='Mahalanobis'))

depthContour(clean,depth_params = list(method='Mahalanobis'))

depthPersp(clean,depth_params = list(method='Mahalanobis'))

bagplot(clean)
plot(clean$TotalVolunteers,clean$TotalItems)

bin=hexbin(clean$TotalVolunteers,clean$TotalItems, xbins=10, xlab="TotalVolunteers", ylab="TotalItems")
plot(bin, main="Hexagonal Binning") 


North_America=cleandata[which(cleandata$Continent=='North America'),] #30108 dati su 38953 totali

u=North_America[,c(8,14)]

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
clean <- u[-ind_outlying_obs,]

plot(cleandata$TotalVolunteers,cleandata$Area)
u=cleandata[,c(8,19)]

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
noutlier <- u[-ind_outlying_obs,]

par(mfrow=c(1,2))
plot(cleandata$TotalVolunteers,cleandata$Area)
plot(noutlier$TotalVolunteers,noutlier$Area)





