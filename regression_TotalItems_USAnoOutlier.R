####Regression DATASET USA senza i 9235 outliers Y=TOTALITEMS
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
CleanUsa=import("CleanUsa.Rdata")
View(CleanUsa)


#grafici sui 4 tipi di evento
levels(as.factor(CleanUsa$EventType))
LandCleanup=CleanUsa[which(CleanUsa$EventType=='Land (beach, shoreline and inland) Cleanup'),] #14820
UnderwaterCleanup=CleanUsa[which(CleanUsa$EventType=='Underwater Cleanup'),] #54
WatercraftCleanup=CleanUsa[which(CleanUsa$EventType=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup'),] #231
MarineDebris=CleanUsa[which(CleanUsa$EventType=='Marine Debris Accumulation Survey'),] #848

par(mfrow=c(2,2))
plot(LandCleanup$TotalVolunteers,LandCleanup$TotalItems,main='Land Cleanup')
plot(UnderwaterCleanup$TotalVolunteers,UnderwaterCleanup$TotalItems,main='Underwater Cleanup')
plot(WatercraftCleanup$TotalVolunteers,WatercraftCleanup$TotalItems,main='Watercraft Cleanup')
plot(MarineDebris$TotalVolunteers,MarineDebris$TotalItems,main='Marine Debris')

for (i in 1:length(CleanUsa$EventType)) {
  if(CleanUsa$EventType[i]=='Land (beach, shoreline and inland) Cleanup')
    CleanUsa$EventType[i]='Land Cleanup'
  if(CleanUsa$EventType[i]=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup')
    CleanUsa$EventType[i]='Watercraft Cleanup'
  if(CleanUsa$EventType[i]=='Marine Debris Accumulation Survey')
    CleanUsa$EventType[i]='Marine Debris'
}

EventType=as.factor(CleanUsa$EventType)


weekend=rep(0,length(CleanUsa$Country))
for (i in 1:length(CleanUsa$Country)) {
  if(CleanUsa$DOW[i]=="Saturday"||CleanUsa$DOW[i]=="Sunday")
    weekend[i]=1
}
library(tibble)
CleanUsa=add_column(CleanUsa, weekend, .after = "DOW")
weekend=as.factor(CleanUsa$weekend)

seasonality=rep(0,length(CleanUsa$Country))
for (i in 1:length(CleanUsa$Country)) {
  if(CleanUsa$Month[i]=="Jun"||CleanUsa$Month[i]=="Jul"||CleanUsa$Month[i]=="Aug")
    seasonality[i]=1
}

CleanUsa=add_column(CleanUsa, seasonality, .after = "Month")
seasonality=as.factor(seasonality)

EventType_seasonality=as.factor(seasonality):EventType
Year=as.factor(CleanUsa$Year)
Month=as.factor(CleanUsa$Month)




###REGRESSION###
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

model_gam_interaction=gam(CleanUsa$TotalItems ~ s(CleanUsa$TotalVolunteers,bs='cr') + EventType + weekend
                          + seasonality + EventType_seasonality + Month + Year,data = CleanUsa)

summary(model_gam_interaction)

plot(model_gam_interaction)


#via EventType_seasonality e seasonality


model_gam=gam(CleanUsa$TotalItems ~ s(CleanUsa$TotalVolunteers,by=factor(EventType),bs='cr')  + weekend
                        + Month + Year,data = CleanUsa)

summary(model_gam)


plot(model_gam,col='red')



















LandCleanup=CleanUsa[which(CleanUsa$EventType=='Land Cleanup'),] #14820
UnderwaterCleanup=CleanUsa[which(CleanUsa$EventType=='Underwater Cleanup'),] #54
WatercraftCleanup=CleanUsa[which(CleanUsa$EventType=='Watercraft Cleanup'),] #231
MarineDebris=CleanUsa[which(CleanUsa$EventType=='Marine Debris'),] #848


modelgamLandCleanup=gam(LandCleanup$TotalItems ~ s(LandCleanup$TotalVolunteers,bs='cr') + as.factor(LandCleanup$weekend) +
                           as.factor(LandCleanup$seasonality) + as.factor(LandCleanup$Month) + as.factor(LandCleanup$Year),data = LandCleanup)

summary(modelgamLandCleanup)

plot(LandCleanup$TotalVolunteers,LandCleanup$TotalItems)
par(new=TRUE)
plot(modelgamLandCleanup,col='red')




modelgamUnderwaterCleanup=gam(UnderwaterCleanup$TotalItems ~ s(UnderwaterCleanup$TotalVolunteers,bs='cr') + as.factor(UnderwaterCleanup$weekend) +
                          as.factor(UnderwaterCleanup$seasonality) + as.factor(UnderwaterCleanup$Month) + as.factor(UnderwaterCleanup$Year),data = UnderwaterCleanup)

summary(modelgamUnderwaterCleanup)

plot(UnderwaterCleanup$TotalVolunteers,UnderwaterCleanup$TotalItems)
par(new=TRUE)
plot(modelgamUnderwaterCleanup,col='red')



modelgamWatercraftCleanup=gam(WatercraftCleanup$TotalItems ~ s(WatercraftCleanup$TotalVolunteers,bs='cr') + as.factor(WatercraftCleanup$weekend) +
                                as.factor(WatercraftCleanup$seasonality) + as.factor(WatercraftCleanup$Month) + as.factor(WatercraftCleanup$Year),data = WatercraftCleanup)

summary(modelgamWatercraftCleanup)

plot(WatercraftCleanup$TotalVolunteers,WatercraftCleanup$TotalItems)
par(new=TRUE)
plot(modelgamWatercraftCleanup,col='red')