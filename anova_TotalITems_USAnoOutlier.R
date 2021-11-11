####ANOVA DATASET USA senza i 9235 outliers Y=TOTALITEMS
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

#stupendi, potremmo studiare 4 modelli diversi, cioè nel modello generale mettere un indice k che sta ad indicare 
#il k-esimo EventType da 1 a 4

#su Land Cleanup si puo pensare a una clusterizzazione(?)


#chiaramente non gaussiani, violazione ipotesi per ANOVA
EventType=as.factor(CleanUsa$EventType)
fit <- aov(CleanUsa$TotalItems ~ EventType)
summary(fit) #TotalItems indipendente da EventType usando ANOVA che però non possiamo usare perche ipotesi violate

#proviamo con permutational Anova
T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$TotalItems)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val  #0 EventType significativo  



#provo a vedere giorni settimana vs weekend 
weekend=rep(0,length(CleanUsa$Country))
for (i in 1:length(CleanUsa$Country)) {
  if(CleanUsa$DOW[i]=="Saturday"||CleanUsa$DOW[i]=="Sunday")
    weekend[i]=1
}
library(tibble)
CleanUsa=add_column(CleanUsa, weekend, .after = "DOW")
CleanUsa$weekend=as.factor(CleanUsa$weekend)

fit <- aov(CleanUsa$TotalItems ~ CleanUsa$weekend)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$weekend)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ CleanUsa$weekend)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 giorno settimana vs weekend significativo per alpha=0.05 o 0.1 

#si puo provare interazione tra EventType e stagionalita, dato che ci sono watercraft e underwater clean up
#magari sono piu frequenti in estate o comunque in mesi non freddi
seasonality=rep(0,length(CleanUsa$Country))
for (i in 1:length(CleanUsa$Country)) {
  if(CleanUsa$Month[i]=="Jun"||CleanUsa$Month[i]=="Jul"||CleanUsa$Month[i]=="Aug")
    seasonality[i]=1
}

CleanUsa=add_column(CleanUsa, seasonality, .after = "Month")

seasonality=as.factor(seasonality)
fit <- aov(CleanUsa$TotalItems ~ seasonality)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ seasonality)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 seasonality significativa per alpha=0.05 o 0.1


#TWO-WAYS ANOVA
EventType_seasonality=as.factor(seasonality):EventType
summary.aov(aov(CleanUsa$TotalItems ~ seasonality + EventType + EventType_seasonality))

# Without interaction
summary.aov(aov(CleanUsa$TotalItems ~ seasonality + EventType))

T0_EventType_seasonality <- summary.aov(aov(CleanUsa$TotalItems ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
T0_EventType_seasonality

aov.H0EventType_seasonality <- aov(CleanUsa$TotalItems ~ seasonality + EventType)
aov.H0EventType_seasonality

residuals.H0EventType_seasonality <- aov.H0EventType_seasonality$residuals
n <- length(CleanUsa$TotalItems)

T_EventType_seasonality  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0EventType_seasonality <- residuals.H0EventType_seasonality[permutation]
  TotalItems.perm.H0EventType_seasonality <- aov.H0EventType_seasonality$fitted + residuals.H0EventType_seasonality
  T_EventType_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0EventType_seasonality ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
}

sum(T_EventType_seasonality >= T0_EventType_seasonality)/B #0.009
#EventType_seasonality significant con alpha=0.1 o 0.05


# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(CleanUsa$TotalItems ~ seasonality + EventType))[[1]][1,4]
# residuals under H0:
# ratio = mu + beta*EventType
aov.H0seasonality <- aov(CleanUsa$TotalItems  ~  EventType)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR EventType   (H0: beta=0)
T0_EventType <- summary.aov(aov(CleanUsa$TotalItems ~ seasonality + EventType))[[1]][2,4]
# residuals under H0:
# ratio = mu + alpha*seasonality
aov.H0EventType <- aov(CleanUsa$TotalItems ~ seasonality)
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

#abbiamo una riconferma, quindi sia con alpha=0.1 che 0.05 abbiamo TotalItems ~ EventType + seasonality + EventType_seasonality


#mese, anno e loro interazione? 

#TWO-WAYS ANOVA
Year=as.factor(CleanUsa$Year)
Month=as.factor(CleanUsa$Month)
month_year=as.factor(CleanUsa$Year):as.factor(CleanUsa$Month)
summary.aov(aov(CleanUsa$TotalItems ~ Year + Month + month_year))

# Without interaction
summary.aov(aov(CleanUsa$TotalItems ~ Year + Month))

T0_month_year <- summary.aov(aov(CleanUsa$TotalItems ~ Year + Month + month_year))[[1]][3,4]
T0_month_year

aov.H0month_year <- aov(CleanUsa$TotalItems ~ Year + Month)
aov.H0month_year

residuals.H0month_year <- aov.H0month_year$residuals
n <- length(CleanUsa$TotalItems)

T_month_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0month_year <- residuals.H0month_year[permutation]
  TotalItems.perm.H0month_year <- aov.H0month_year$fitted + residuals.H0month_year
  T_month_year[perm] <- summary.aov(aov(TotalItems.perm.H0month_year ~ Year + Month + month_year))[[1]][3,4]
}

sum(T_month_year >= T0_month_year)/B #0 month_year significant se fissiamo alpha=0.05 o 0.1

#YEAR
fit <- aov(CleanUsa$TotalItems ~ Year)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ Year)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 Year significativa per alpha=0.05 o 0.1

#MONTH
fit <- aov(CleanUsa$TotalItems ~ Month)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$TotalItems[permutation]
  fit_perm <- aov(TotalItemss ~ Month)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 Month significativa per alpha=0.05 o 0.1


#YEAR\SEASONALITY 
#TWO-WAYS ANOVA
seasonality=as.factor(seasonality)
seasonality_year=as.factor(seasonality):as.factor(CleanUsa$Year)
summary.aov(aov(CleanUsa$TotalItems ~ Year + seasonality + seasonality_year))

# Without interaction
summary.aov(aov(CleanUsa$TotalItems ~ Year + seasonality))

T0_seasonality_year <- summary.aov(aov(CleanUsa$TotalItems ~ Year + seasonality + seasonality_year))[[1]][3,4]
T0_seasonality_year

aov.H0seasonality_year <- aov(CleanUsa$TotalItems ~ Year + seasonality)
aov.H0seasonality_year

residuals.H0seasonality_year <- aov.H0seasonality_year$residuals
n <- length(CleanUsa$TotalItems)

T_seasonality_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0seasonality_year <- residuals.H0seasonality_year[permutation]
  TotalItems.perm.H0seasonality_year <- aov.H0seasonality_year$fitted + residuals.H0seasonality_year
  T_seasonality_year[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality_year ~ Year + seasonality + seasonality_year))[[1]][3,4]
}

sum(T_seasonality_year >= T0_seasonality_year)/B #0 seasonality_year significant 


# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(CleanUsa$TotalItems ~ seasonality + Year))[[1]][1,4]
# residuals under H0:
# ratio = mu + beta*Year
aov.H0seasonality <- aov(CleanUsa$TotalItems  ~  Year)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR Year   (H0: beta=0)
T0_year <- summary.aov(aov(CleanUsa$TotalItems ~ seasonality + Year))[[1]][2,4]
# residuals under H0:
# ratio = mu + alpha*seasonality
aov.H0year <- aov(CleanUsa$TotalItems ~ seasonality)
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

sum(T_seasonality >= T0_seasonality)/B #0 seasonality significant  
sum(T_year >= T0_year)/B #0 year significant 

#alpha=0.1 o 0.05
#TotalItems ~ EventType + weekend + seasonality + EventType_seasonality + month + Year + month_year + seasonality_year


