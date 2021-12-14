####ANOVA DATASET COMPLETO ----> ANOVA DATASET USA Y=TOTALITEMS
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

#grafici sui 4 tipi di evento
levels(as.factor(cleandata$EventType))
LandCleanup=cleandata[which(cleandata$EventType=='Land (beach, shoreline and inland) Cleanup'),] #36371
UnderwaterCleanup=cleandata[which(cleandata$EventType=='Underwater Cleanup'),] #649
WatercraftCleanup=cleandata[which(cleandata$EventType=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup'),] #437
MarineDebris=cleandata[which(cleandata$EventType=='Marine Debris Accumulation Survey'),] #1496

par(mfrow=c(2,2))
plot(LandCleanup$TotalVolunteers,LandCleanup$TotalItems,main='Land Cleanup')
plot(UnderwaterCleanup$TotalVolunteers,UnderwaterCleanup$TotalItems,main='Underwater Cleanup')
plot(WatercraftCleanup$TotalVolunteers,WatercraftCleanup$TotalItems,main='Watercraft Cleanup')
plot(MarineDebris$TotalVolunteers,MarineDebris$TotalItems,main='Marine Debris')

#chiaramente non gaussiani, violazione ipotesi per ANOVA
EventType=as.factor(cleandata$EventType)
fit <- aov(cleandata$TotalItems ~ EventType)
summary(fit) #TotalItems indipendente da EventType usando ANOVA che però non possiamo usare perche ipotesi violate

#proviamo con permutational Anova
T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(cleandata$TotalItems)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- cleandata$TotalItems[permutation]
  fit_perm <- aov(TotalItems ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val  #TotalItems indipendente da EventType 


#proviamo solo negli USA
USA=cleandata[which(cleandata$Country=='USA'),] #25188 dati su 38953 totali

LandCleanup=USA[which(USA$EventType=='Land (beach, shoreline and inland) Cleanup'),] #23181
UnderwaterCleanup=USA[which(USA$EventType=='Underwater Cleanup'),] #148
WatercraftCleanup=USA[which(USA$EventType=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup'),] #397
MarineDebris=USA[which(USA$EventType=='Marine Debris Accumulation Survey'),] #1462

par(mfrow=c(2,2))
plot(LandCleanup$TotalVolunteers,LandCleanup$TotalItems,main='Land Cleanup')
plot(UnderwaterCleanup$TotalVolunteers,UnderwaterCleanup$TotalItems,main='Underwater Cleanup')
plot(WatercraftCleanup$TotalVolunteers,WatercraftCleanup$TotalItems,main='Watercraft Cleanup')
plot(MarineDebris$TotalVolunteers,MarineDebris$TotalItems,main='Marine Debris')


EventType=as.factor(USA$EventType)
fit <- aov(USA$TotalItems ~ EventType)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(USA$TotalItems)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- USA$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0.028 NEGLI USA EventType E' SIGNIFICATIVO con alpha=0.05 o 0.1



#provo a vedere giorni settimana vs weekend 
weekend=rep(0,length(USA$Country))
for (i in 1:length(USA$Country)) {
  if(USA$DOW[i]=="Saturday"||USA$DOW[i]=="Sunday")
    weekend[i]=1
}
library(tibble)
USA=add_column(USA, weekend, .after = "DOW")
USA$weekend=as.factor(USA$weekend)

fit <- aov(USA$TotalItems ~ USA$weekend)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(USA$weekend)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- USA$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ USA$weekend)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 giorno settimana vs weekend significativo per alpha=0.05 o 0.1 

#si puo provare interazione tra EventType e stagionalita, dato che ci sono watercraft e underwater clean up
#magari sono piu frequenti in estate o comunque in mesi non freddi
seasonality=rep(0,length(USA$Country))
for (i in 1:length(USA$Country)) {
  if(USA$Month[i]=="Jun"||USA$Month[i]=="Jul"||USA$Month[i]=="Aug")
    seasonality[i]=1
}

USA=add_column(USA, seasonality, .after = "Month")

seasonality=as.factor(seasonality)
fit <- aov(USA$TotalItems ~ seasonality)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(USA$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- USA$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ seasonality)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 seasonality significativa per alpha=0.05 o 0.1


#TWO-WAYS ANOVA
EventType_seasonality=as.factor(seasonality):EventType
summary.aov(aov(USA$TotalItems ~ seasonality + EventType + EventType_seasonality))

# Without interaction
summary.aov(aov(USA$TotalItems ~ seasonality + EventType))

T0_EventType_seasonality <- summary.aov(aov(USA$TotalItems ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
T0_EventType_seasonality

aov.H0EventType_seasonality <- aov(USA$TotalItems ~ seasonality + EventType)
aov.H0EventType_seasonality

residuals.H0EventType_seasonality <- aov.H0EventType_seasonality$residuals
n <- length(USA$TotalItems)

T_EventType_seasonality  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0EventType_seasonality <- residuals.H0EventType_seasonality[permutation]
  TotalItems.perm.H0EventType_seasonality <- aov.H0EventType_seasonality$fitted + residuals.H0EventType_seasonality
  T_EventType_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0EventType_seasonality ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
}

sum(T_EventType_seasonality >= T0_EventType_seasonality)/B #0.079
#EventType_seasonality significant con alpha=0.1


# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(USA$TotalItems ~ seasonality + EventType))[[1]][1,4]
# residuals under H0:
# ratio = mu + beta*EventType
aov.H0seasonality <- aov(USA$TotalItems  ~  EventType)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR EventType   (H0: beta=0)
T0_EventType <- summary.aov(aov(USA$TotalItems ~ seasonality + EventType))[[1]][2,4]
# residuals under H0:
# ratio = mu + alpha*seasonality
aov.H0EventType <- aov(USA$TotalItems ~ seasonality)
residuals.H0EventType <- aov.H0EventType$residuals


# TEST OF FACTOR Seasonality ANF TEST OF FACTOR EventType
# p-values
B <- 1000
T_seasonality  <- T_EventType <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  TotalItems.perm.H0seasonality <- aov.H0seasonality$fitted + residuals.H0seasonality[permutation]
  T_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality  ~ seasonality + EventType))[[1]][1,4]
  
  TotalItems.perm.H0EventType <- aov.H0EventType$fitted + residuals.H0EventType[permutation]
  T_EventType[perm] <- summary.aov(aov(TotalItems.perm.H0EventType ~ seasonality + EventType))[[1]][2,4]
}

sum(T_seasonality >= T0_seasonality)/B #0 seasonality significant per alpha=0.05 o 0.1 

sum(T_EventType >= T0_EventType)/B #0.041 EventType significant con alpha=0.1 o 0.05 

#abbiamo una riconferma, quindi con alpha=0.1 abbiamo TotalItems ~ EventType + EventType_seasonality
#con alpha=0.05 abbiamo TotalItems ~EventType


#mese, anno e loro interazione? 

#TWO-WAYS ANOVA
Year=as.factor(USA$Year)
Month=as.factor(USA$Month)
month_year=as.factor(USA$Year):as.factor(USA$Month)
summary.aov(aov(USA$TotalItems ~ Year + Month + month_year))

# Without interaction
summary.aov(aov(USA$TotalItems ~ Year + Month))

T0_month_year <- summary.aov(aov(USA$TotalItems ~ Year + Month + month_year))[[1]][3,4]
T0_month_year

aov.H0month_year <- aov(USA$TotalItems ~ Year + Month)
aov.H0month_year

residuals.H0month_year <- aov.H0month_year$residuals
n <- length(USA$TotalItems)

T_month_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0month_year <- residuals.H0month_year[permutation]
  TotalItems.perm.H0month_year <- aov.H0month_year$fitted + residuals.H0month_year
  T_month_year[perm] <- summary.aov(aov(TotalItems.perm.H0month_year ~ Year + Month + month_year))[[1]][3,4]
}

sum(T_month_year >= T0_month_year)/B #0.012 month_year significant se fissiamo alpha=0.05 o 0.1

#YEAR
fit <- aov(USA$TotalItems ~ Year)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(USA$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- USA$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ Year)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0.007 Year significativa per alpha=0.05 o 0.1

#MONTH
fit <- aov(USA$TotalItems ~ Month)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(USA$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- USA$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ Month)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 Month significativa per alpha=0.05 o 0.1


#YEAR\SEASONALITY 
#TWO-WAYS ANOVA
seasonality=as.factor(seasonality)
seasonality_year=as.factor(seasonality):as.factor(USA$Year)
summary.aov(aov(USA$TotalItems ~ Year + seasonality + seasonality_year))

# Without interaction
summary.aov(aov(USA$TotalItems ~ Year + seasonality))

T0_seasonality_year <- summary.aov(aov(USA$TotalItems ~ Year + seasonality + seasonality_year))[[1]][3,4]
T0_seasonality_year

aov.H0seasonality_year <- aov(USA$TotalItems ~ Year + seasonality)
aov.H0seasonality_year

residuals.H0seasonality_year <- aov.H0seasonality_year$residuals
n <- length(USA$TotalItems)

T_seasonality_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0seasonality_year <- residuals.H0seasonality_year[permutation]
  TotalItems.perm.H0seasonality_year <- aov.H0seasonality_year$fitted + residuals.H0seasonality_year
  T_seasonality_year[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality_year ~ Year + seasonality + seasonality_year))[[1]][3,4]
}

sum(T_seasonality_year >= T0_seasonality_year)/B #0.095 seasonality_year significant per alpha= 0.1


# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(USA$TotalItems ~ seasonality + Year))[[1]][1,4]
# residuals under H0:
# ratio = mu + beta*Year
aov.H0seasonality <- aov(USA$TotalItems  ~  Year)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR Year   (H0: beta=0)
T0_year <- summary.aov(aov(USA$TotalItems ~ seasonality + Year))[[1]][2,4]
# residuals under H0:
# ratio = mu + alpha*seasonality
aov.H0year <- aov(USA$TotalItems ~ seasonality)
residuals.H0year <- aov.H0year$residuals


# TEST OF FACTOR Seasonality ANF TEST OF FACTOR Year
# p-values
B <- 1000
T_seasonality  <- T_year <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  TotalItems.perm.H0seasonality <- aov.H0seasonality$fitted + residuals.H0seasonality[permutation]
  T_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality  ~ seasonality + Year))[[1]][1,4]
  
  TotalItems.perm.H0year <- aov.H0year$fitted + residuals.H0year[permutation]
  T_year[perm] <- summary.aov(aov(TotalItems.perm.H0year ~ seasonality + Year))[[1]][2,4]
}

sum(T_seasonality >= T0_seasonality)/B #0 seasonality significant per alpha=0.05 o 0.1 
sum(T_year >= T0_year)/B #0.022 year significant con alpha=0.1 o 0.05

#alpha=0.1
#TotalItems ~ EventType + weekend + seasonality + EventType_seasonality + month + Year + month_year + seasonality_year

#alpha=0.05 (di solito si prende questo)
#TotalItems ~ EventType + weekend + seasonality + month + Year + month_year 

