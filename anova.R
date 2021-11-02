####ANOVA MANOVA DATASET COMPLETO ----> ANOVA DATASET NORTH AMERICA ----> RISULTATI SODDISFACENTI
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

#MANOVA
EventType <- factor(cleandata$EventType, labels=c('Land Cleanup','Underwater Cleanup','Watercraft Cleanup','Marine Debris'))
VolunteersItems <- cleandata[,c(8,14)]
plot(VolunteersItems,col=EventType)

fit1  <- manova(as.matrix(VolunteersItems) ~ EventType)
summary.manova(fit1,test="Wilks") 

T0 <- -summary.manova(fit1,test="Wilks")$stats[1,2]
T0

set.seed(seed)
T_stat <- numeric(B)

for(perm in 1:B){
  # choose random permutation
  permutation <- sample(1:n)
  EventType.perm <- EventType[permutation]
  fit.perm <- manova(as.matrix(VolunteersItems) ~ EventType.perm)
  T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-2,1))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val
#(TotalItems,TotalVolunteers) indipendente da EventType 

#Cappozzo ci ha suggerito di provare ad usare come y=TotalItems/TotalVolunteers
ratio=rep(0,length(cleandata$TotalItems))
for (i in 1:length(cleandata$TotalItems)) {
  ratio[i]=cleandata$TotalItems[i]/cleandata$TotalVolunteers[i]
}
ratio=as.data.frame(ratio)
cleandata=cbind(cleandata,ratio)

#ipotesi normalita
ks.test(ratio,y='pnorm',alternative='two.sided')
#ovviamente non gaussiano

EventType=as.factor(cleandata$EventType)
fit <- aov(cleandata$ratio ~ EventType)
summary(fit) 
T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(cleandata$ratio)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  ratio <- cleandata$ratio[permutation]
  fit_perm <- aov(cleandata$ratio ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val  #ratio indipendente da EventType 

#TWO-WAYS ANOVA
ratio   
continent=as.factor(cleandata$Continent) #7 levels
EventType=as.factor(cleandata$EventType) #4 levels
EventType_continent=continent:EventType  #28 levels

summary.aov(aov(ratio ~ continent + EventType + EventType_continent))

# Without interaction
summary.aov(aov(ratio ~ continent + EventType))

T0_EventType_continent <- summary.aov(aov(ratio ~ continent + EventType + EventType_continent))[[1]][3,4]
T0_EventType_continent

aov.H0EventType_continent <- aov(ratio ~ continent + EventType)
aov.H0EventType_continent

residuals.H0EventType_continent <- aov.H0EventType_continent$residuals
n <- length(cleandata$ratio)

T_EventType_continent  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0EventType_continent <- residuals.H0EventType_continent[permutation]
  ratio.perm.H0EventType_continent <- aov.H0EventType_continent$fitted + residuals.H0EventType_continent
  T_EventType_continent[perm] <- summary.aov(aov(ratio.perm.H0EventType_continent ~ continent + EventType + EventType_continent))[[1]][3,4]
}

sum(T_EventType_continent >= T0_EventType_continent)/B #EventType_continent not significant

# TEST OF FACTOR Continent   (H0: alpha=0)
T0_continent <- summary.aov(aov(ratio ~ continent + EventType))[[1]][1,4]
# residuals under H0:
# ratio = mu + beta*EventType
aov.H0continent <- aov(ratio ~  EventType)
residuals.H0continent <- aov.H0continent$residuals

# TEST OF FACTOR EventType   (H0: beta=0)
T0_EventType <- summary.aov(aov(ratio ~ continent + EventType))[[1]][2,4]
# residuals under H0:
# ratio = mu + alpha*continent
aov.H0EventType <- aov(ratio ~ continent)
residuals.H0EventType <- aov.H0EventType$residuals


# TEST OF FACTOR Continent ANF TEST OF FACTOR EventType
# p-values
B <- 1000
T_continent <- T_EventType <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  ratio.perm.H0continent <- aov.H0continent$fitted + residuals.H0continent[permutation]
  T_continent[perm] <- summary.aov(aov(ratio.perm.H0continent  ~ continent + EventType))[[1]][1,4]
  
  ratio.perm.H0EventType <- aov.H0EventType$fitted + residuals.H0EventType[permutation]
  T_EventType[perm] <- summary.aov(aov(ratio.perm.H0EventType ~ continent + EventType))[[1]][2,4]
}

sum(T_continent >= T0_continent)/B #continent not significant

sum(T_EventType >= T0_EventType)/B #EventType not significant

#proviamo solo sul North America
North_America=cleandata[which(cleandata$Continent=='North America'),] #30108 dati su 38953 totali

EventType=as.factor(North_America$EventType)
fit <- aov(North_America$TotalItems ~ EventType)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$TotalItems)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(TotalItems ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #SUL NORTH AMERICA EventType E' SIGNIFICATIVO

ratio=rep(0,length(North_America$TotalItems))
for (i in 1:length(North_America$TotalItems)) {
  ratio[i]=North_America$TotalItems[i]/North_America$TotalVolunteers[i]
}
ratio=as.data.frame(ratio)
North_America=cbind(North_America,ratio)

fit <- aov(North_America$ratio ~ EventType)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$ratio)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(North_America$ratio ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #se consideriamo il ratio risulta non significativo 

#provo a vedere giorni settimana vs weekend 
weekend=rep(0,length(North_America$Country))
for (i in 1:length(North_America$Country)) {
  if(North_America$DOW[i]=="Saturday"||North_America$DOW[i]=="Sunday")
    weekend[i]=1
}
library(tibble)
North_America=add_column(North_America, weekend, .after = "DOW")


fit <- aov(North_America$TotalItems ~ North_America$weekend)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$weekend)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(North_America$TotalItems ~ North_America$weekend)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #giorno settimana vs weekend NON significativo.

#si puo provare interazione tra EventType e stagionalita, dato che ci sono watercraft e underwater clean up
#magari sono piu frequenti in estate o comunque in mesi non freddi
seasonality=rep(0,length(North_America$Country))
for (i in 1:length(North_America$Country)) {
  if(North_America$Month[i]=="Jun"||North_America$Month[i]=="Jul"||North_America$Month[i]=="Aug")
    seasonality[i]=1
}

North_America=add_column(North_America, seasonality, .after = "Month")

seasonality=as.factor(seasonality)
fit <- aov(North_America$TotalItems ~ seasonality)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(North_America$TotalItems ~ seasonality)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #seasonality NON significativa.


#TWO-WAYS ANOVA
EventType_seasonality=as.factor(seasonality):EventType
summary.aov(aov(North_America$TotalItems ~ seasonality + EventType + EventType_seasonality))

# Without interaction
summary.aov(aov(North_America$TotalItems ~ seasonality + EventType))

T0_EventType_seasonality <- summary.aov(aov(North_America$TotalItems ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
T0_EventType_seasonality

aov.H0EventType_seasonality <- aov(North_America$TotalItems ~ seasonality + EventType)
aov.H0EventType_seasonality

residuals.H0EventType_seasonality <- aov.H0EventType_seasonality$residuals
n <- length(North_America$TotalItems)

T_EventType_seasonality  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0EventType_seasonality <- residuals.H0EventType_seasonality[permutation]
  TotalItems.perm.H0EventType_seasonality <- aov.H0EventType_seasonality$fitted + residuals.H0EventType_seasonality
  T_EventType_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0EventType_seasonality ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
}

sum(T_EventType_seasonality >= T0_EventType_seasonality)/B #0.09 (molto vicino a 0.1) EventType_seasonality significant se fissiamo alpha=0.1
#se consideriamo alpha=0.05 diventa non significativo e quindi(dipende che scelta globale facciamo di alpha, quello piu 
#usato e' 0.05):

# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(North_America$TotalItems ~ seasonality + EventType))[[1]][1,4]
# residuals under H0:
# TotalItems = mu + beta*EventType
aov.H0seasonality <- aov(North_America$TotalItems  ~  EventType)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR EventType   (H0: beta=0)
T0_EventType <- summary.aov(aov(North_America$TotalItems ~ seasonality + EventType))[[1]][2,4]
# residuals under H0:
# TotalItems = mu + alpha*seasonality
aov.H0EventType <- aov(North_America$TotalItems ~ seasonality)
residuals.H0EventType <- aov.H0EventType$residuals


# TEST OF FACTOR Seasonality ANF TEST OF FACTOR EventType
# p-values
B <- 1000
T_seasonality  <- T_EventType <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  Totalitems.perm.H0seasonality <- aov.H0seasonality$fitted + residuals.H0seasonality[permutation]
  T_seasonality[perm] <- summary.aov(aov(Totalitems.perm.H0seasonality  ~ seasonality + EventType))[[1]][1,4]
  
  Totalitems.perm.H0EventType <- aov.H0EventType$fitted + residuals.H0EventType[permutation]
  T_EventType[perm] <- summary.aov(aov(Totalitems.perm.H0EventType ~ seasonality + EventType))[[1]][2,4]
}

sum(T_seasonality >= T0_seasonality)/B #seasonality significant

sum(T_EventType >= T0_EventType)/B #EventType significant (questo lo sapevamo gia ma se mettiamo insieme le covariate
#EventType e seasonality anche quest'ultima diventa significativa, quindi perfetto perche' le usiamo entrambe come covariate)
#quindi se fissiamo alpha=0.05 seasonality e EventType sono significativi

#quindi se scegliamo alpha=0.05 le nostre covariate saranno EventType e seasonality
#quindi se scegliamo alpha=0.1 le nostre covariate di queste tre saranno EventType, seasonality e l'interazione EventType_seasonality

#io sceglierei 0.05 perche quello piu usato frequentemente


#mese, anno e loro interazione? 

#TWO-WAYS ANOVA
Year=as.factor(North_America$Year)
Month=as.factor(North_America$Month)
month_year=as.factor(North_America$Year):as.factor(North_America$Month)
summary.aov(aov(North_America$TotalItems ~ Year + Month + month_year))

# Without interaction
summary.aov(aov(North_America$TotalItems ~ Year + Month))

T0_month_year <- summary.aov(aov(North_America$TotalItems ~ Year + Month + month_year))[[1]][3,4]
T0_month_year

aov.H0month_year <- aov(North_America$TotalItems ~ Year+ Month)
aov.H0month_year

residuals.H0month_year <- aov.H0month_year$residuals
n <- length(North_America$TotalItems)

T_month_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0month_year <- residuals.H0month_year[permutation]
  TotalItems.perm.H0month_year <- aov.H0month_year$fitted + residuals.H0month_year
  T_month_year[perm] <- summary.aov(aov(TotalItems.perm.H0month_year ~ Year + Month + month_year))[[1]][3,4]
}

sum(T_month_year >= T0_month_year)/B #month_year significant se fissiamo alpha=0.05 o 0.1

#YEAR
fit <- aov(North_America$TotalItems ~ Year)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(North_America$TotalItems ~ Year)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #Year NON significativa.

#MONTH
fit <- aov(North_America$TotalItems ~ Month)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(North_America$seasonality)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItems <- North_America$TotalItems[permutation]
  fit_perm <- aov(North_America$TotalItems ~ Month)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #Month NON significativa.


#YEAR\SEASONALITY 
#TWO-WAYS ANOVA
seasonality=as.factor(seasonality)
seasonality_year=as.factor(seasonality):as.factor(North_America$Year)
summary.aov(aov(North_America$TotalItems ~ Year + seasonality + seasonality_year))

# Without interaction
summary.aov(aov(North_America$TotalItems ~ Year + seasonality))

T0_seasonality_year <- summary.aov(aov(North_America$TotalItems ~ Year + seasonality + seasonality_year))[[1]][3,4]
T0_seasonality_year

aov.H0seasonality_year <- aov(North_America$TotalItems ~ Year + seasonality)
aov.H0seasonality_year

residuals.H0seasonality_year <- aov.H0seasonality_year$residuals
n <- length(North_America$TotalItems)

T_seasonality_year  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0seasonality_year <- residuals.H0seasonality_year[permutation]
  TotalItems.perm.H0seasonality_year <- aov.H0seasonality_year$fitted + residuals.H0seasonality_year
  T_seasonality_year[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality_year ~ Year + seasonality + seasonality_year))[[1]][3,4]
}

sum(T_seasonality_year >= T0_seasonality_year)/B #seasonality_year significant se fissiamo alpha=0.05 o 0.1




