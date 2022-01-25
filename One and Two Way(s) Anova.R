#################################################
###             One/Two-Way(s) Anova          ###
#################################################


current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list=ls())

# Packages ----------------------------------------------------------------

library(rio)
library(roahd)
library(rgl)
library(MASS)
library(rgl)
library(tibble)

# Import Dataset ----------------------------------------------------------


CleanUsa=import("CleanUSA.Rdata")
View(CleanUsa)


# Plot of the four EventType ----------------------------------------------

levels(as.factor(CleanUsa$EventType))
LandCleanup=CleanUsa[which(CleanUsa$EventType=='Land (beach, shoreline and inland) Cleanup'),] #12231
UnderwaterCleanup=CleanUsa[which(CleanUsa$EventType=='Underwater Cleanup'),] #36
WatercraftCleanup=CleanUsa[which(CleanUsa$EventType=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup'),] #175
MarineDebris=CleanUsa[which(CleanUsa$EventType=='Marine Debris Accumulation Survey'),] #912

par(mfrow=c(2,2))
plot(LandCleanup$TotalVolunteers,LandCleanup$log_Items,main='Land Cleanup',col='blue',xlab = 'TotalVolunteers',ylab = 'TotalItems')
plot(UnderwaterCleanup$TotalVolunteers,UnderwaterCleanup$log_Items,main='Underwater Cleanup',col='blue',xlab = 'TotalVolunteers',ylab = 'TotalItems')
plot(WatercraftCleanup$TotalVolunteers,WatercraftCleanup$log_Items,main='Watercraft Cleanup',col='blue',xlab = 'TotalVolunteers',ylab = 'TotalItems')
plot(MarineDebris$TotalVolunteers,MarineDebris$log_Items,main='Marine Debris',col='blue',xlab = 'TotalVolunteers',ylab = 'TotalItems')

# Clearly not Gaussian, hypothesis violation for ANOVA --------------------


# PERMUTATIONAL ANOVA -----------------------------------------------------

# One-Way Anova for EventType ---------------------------------------------

EventType=as.factor(CleanUsa$EventType)
fit <- aov(CleanUsa$log_Items ~ EventType)
summary(fit) 

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$log_Items)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$log_Items[permutation]
  fit_perm <- aov(TotalItemss ~ EventType)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val  #0 EventType significant  


# Weekdays vs Weekend: Dummy Variable -------------------------------------

weekend=rep(0,length(CleanUsa$Country))
for (i in 1:length(CleanUsa$Country)) {
  if(CleanUsa$DOW[i]=="Saturday"||CleanUsa$DOW[i]=="Sunday")
    weekend[i]=1
}

CleanUsa=add_column(CleanUsa, weekend, .after = "DOW")
CleanUsa$weekend=as.factor(CleanUsa$weekend)

# One-Way Anova for Type of the Day ---------------------------------------

fit <- aov(CleanUsa$log_Items ~ CleanUsa$weekend)
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
  TotalItemss <- CleanUsa$log_Items[permutation]
  fit_perm <- aov(TotalItemss ~ CleanUsa$weekend)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 Type of the Day significant


# Seasonality: 4 Seasons --------------------------------------------------

Season = rep('Winter', dim(CleanUsa)[1])
CleanUsa=add_column(CleanUsa, Season, .after = "Month") 

CleanUsa[which(CleanUsa$Month %in% c('Mar', 'Apr', 'May')), 12] = 'Spring'
CleanUsa[which(CleanUsa$Month %in% c('Jun', 'Jul', 'Aug')), 12] = 'Summer'
CleanUsa[which(CleanUsa$Month %in% c('Sep', 'Oct', 'Nov')), 12] = 'Autumn'
seasonality=as.factor(CleanUsa$Season)

# One-Way Anova for Seasonality -------------------------------------------

fit <- aov(CleanUsa$log_Items ~ seasonality)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0
B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- length(CleanUsa$Season)

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  TotalItemss <- CleanUsa$log_Items[permutation]
  fit_perm <- aov(TotalItemss ~ seasonality)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


p_val <- sum(T_stat>=T0)/B
p_val #0 Seasonality significant


# Two-Ways Anova ----------------------------------------------------------

EventType_seasonality=as.factor(seasonality):EventType
summary.aov(aov(CleanUsa$log_Items ~ seasonality + EventType + EventType_seasonality))

# Without interaction
summary.aov(aov(CleanUsa$log_Items ~ seasonality + EventType))

T0_EventType_seasonality <- summary.aov(aov(CleanUsa$log_Items ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
T0_EventType_seasonality

aov.H0EventType_seasonality <- aov(CleanUsa$log_Items ~ seasonality + EventType)
aov.H0EventType_seasonality

residuals.H0EventType_seasonality <- aov.H0EventType_seasonality$residuals
n <- length(CleanUsa$log_Items)

T_EventType_seasonality  <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0EventType_seasonality <- residuals.H0EventType_seasonality[permutation]
  TotalItems.perm.H0EventType_seasonality <- aov.H0EventType_seasonality$fitted + residuals.H0EventType_seasonality
  T_EventType_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0EventType_seasonality ~ seasonality + EventType + EventType_seasonality))[[1]][3,4]
}


p_val <- sum(T_EventType_seasonality >= T0_EventType_seasonality)/B
p_val #0 EventType_seasonality significant 


# TEST OF FACTOR Seasonality   (H0: alpha=0)
T0_seasonality <- summary.aov(aov(CleanUsa$log_Items ~ seasonality + EventType))[[1]][1,4]

# residuals under H0:
# logItems = mu + beta*EventType
aov.H0seasonality <- aov(CleanUsa$log_Items  ~  EventType)
residuals.H0seasonality <- aov.H0seasonality$residuals

# TEST OF FACTOR EventType     (H0: beta=0)
T0_EventType <- summary.aov(aov(CleanUsa$log_Items ~ seasonality + EventType))[[1]][2,4]

# residuals under H0:
# logItems = mu + alpha*seasonality
aov.H0EventType <- aov(CleanUsa$log_Items ~ seasonality)
residuals.H0EventType <- aov.H0EventType$residuals


# TEST OF FACTOR Seasonality AND TEST OF FACTOR EventType
B <- 1000
T_seasonality  <- T_EventType <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  TotalItems.perm.H0seasonality <- aov.H0seasonality$fitted + residuals.H0seasonality[permutation]
  T_seasonality[perm] <- summary.aov(aov(TotalItems.perm.H0seasonality  ~ seasonality + EventType))[[1]][1,4]
  
  TotalItems.perm.H0EventType <- aov.H0EventType$fitted + residuals.H0EventType[permutation]
  T_EventType[perm] <- summary.aov(aov(TotalItems.perm.H0EventType ~ seasonality + EventType))[[1]][2,4]
}

sum(T_seasonality >= T0_seasonality)/B #0 Seasonality significant 

sum(T_EventType >= T0_EventType)/B #0 EventType significant  

# We have a reconfirmation, so, for now, we have logItems ~ EventType + seasonality + EventType_seasonality


# Final Model ------------------------------------------------------------- 

# If we choose alpha = 0.05 we have LogItems ~ EventType + Weekend + Seasonality + EventType_Seasonality

