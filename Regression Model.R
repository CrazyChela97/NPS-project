#################################################
###             REGRESSION MODEL              ###
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
library(DepthProc)
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)
library(ISLR2)
library(car)
library(mgcv)
library(splines)
library(pbapply)


# Data Transformation -----------------------------------------------------
CleanUsa = import("CleanUsa.Rdata")
# View(CleanUsa)

for (i in 1:length(CleanUsa$EventType)) {
  if(CleanUsa$EventType[i]=='Land (beach, shoreline and inland) Cleanup')
    CleanUsa$EventType[i]='Land Cleanup'
  if(CleanUsa$EventType[i]=='Watercraft (powerboat, sailboat, kayak or canoe) Cleanup')
    CleanUsa$EventType[i]='Watercraft Cleanup'
  if(CleanUsa$EventType[i]=='Marine Debris Accumulation Survey')
    CleanUsa$EventType[i]='Marine Debris'
}

levels(factor(CleanUsa$EventType))

data = CleanUsa[ , c(5,6,7,8,9,11,13,14,19)]
data$log_item = log(data$TotalItems)

plot(data$TotalVolunteers, data$log_item)



# Regressors Implementation -----------------------------------------------

# WEEKEND : Dummy Variable
weekend = rep(0, dim(data)[1])
for (i in 1:dim(data)[1]) {
  if(CleanUsa$DOW[i]=="Saturday"||CleanUsa$DOW[i]=="Sunday")
    weekend[i]=1
}
# DOW info removed and substituted with weekend
data = data[ , -7]   
data$weekend = weekend



# SEASONALITY : 4 Seasons 
data$Season = rep('Winter', dim(data)[1])

data[which(data$Month %in% c('Mar', 'Apr', 'May')), 11] = 'Spring'
data[which(data$Month %in% c('Jun', 'Jul', 'Aug')), 11] = 'Summer'
data[which(data$Month %in% c('Sep', 'Oct', 'Nov')), 11] = 'Autumn'

# saving new dataset
save(data, file="RegData.Rdata")


# Analysis for Categorical Regressors -------------------------------------

# YEAR
cat = levels(factor(data$Year))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Year == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_item, main=cat[k])
}


# MONTH
cat = levels(factor(data$Month))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Month == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_item, main=cat[k])
}


# EVENT TYPE
cat = levels(factor(data$EventType))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$EventType == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_item, main=cat[k])
}


# SEASON
cat = levels(factor(data$Season))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Season == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_item, main=cat[k])
}


# WEEKEND
cat = levels(factor(data$weekend))
c = length(cat)

par(mfrow=c(c/2,c))
for (k in 1:c){
  dati = data[which(data$weekend == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_item, main=cat[k])
}

dev.off()

# EventType con colori by cami
Land=data[which(data$EventType=='Land Cleanup'),]
Marine=data[which(data$EventType=='Marine Debris'),]
Under=data[which(data$EventType=='Underwater Cleanup'),]
Water=data[which(data$EventType=='Watercraft Cleanup'),]

plot(Land$TotalVolunteers, Land$log_item, pch=16, cex=0.5, col = 'forestgreen')
points(Marine$TotalVolunteers, Marine$log_item, pch=16, cex=0.5,col = 'gold')
points(Water$TotalVolunteers, Water$log_item, pch=16, cex=0.5, col = 'lightskyblue')
points(Under$TotalVolunteers, Under$log_item, pch=16, cex=0.5,col = 'firebrick3')




# Training vs Test Data ---------------------------------------------------

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data

test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]


# GAM Model -----------------------------------------------------------

gam_model = gam(log_Items ~ s(TotalVolunteers, by=factor(Season), bs='cr') + Area + 
                  weekend + EventType + as.factor(Year), data=CleanUsa)
summary(gam_model) 
# dal summary sembra tutto bellino

par(mfrow=c(2,2))
plot(gam_model)
dev.off()
# il plot non ha alcun senso

plot(data$TotalVolunteers, data$log_item)
par(new=TRUE)
par(mfrow=c(2,2))
plot(gam_model, col='red')




# NONPARAMETRIC REGRESSION : Basic ------------------------------------------------

# We try to find the best model defining the general outgoing of our data 
# that is : trying to estimate the behavior of y wrt x

# Poly --------------------------------------------------------------------
x <- train_data$TotalVolunteers
y <- train_data$log_item

# Choose the degree of the polynomial
m_list <- lapply(1:10, function(degree){lm(y ~ poly(x, degree=degree), data=data)})
do.call(anova, m_list)
# taking degree 8 since it's the last significant one

fit <- lm(y ~ poly(x , degree=8), data=data) 

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit, list(x=x.grid), se=T)
plot(x, y ,xlim=range(x.grid) ,cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit ,lwd =2, col =" blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic : not so good
summary(fit)
# R2 = 0.5681
par(mfrow=c(2,2))
plot(fit)
dev.off()



# Rifaccio il modello con poly togliendo il dato #2989 che ha elevata Leverage
data2 <- train_data[-2989,]
x <- data2$TotalVolunteers
y <- data2$log_item
m_list <- lapply(1:10, function(degree){lm(y ~ poly(x, degree=degree), data=data2)})
do.call(anova, m_list)

fit2 <- lm(y ~ poly(x , degree=8), data=data) # offset = Area

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit2, list(x=x.grid), se=T)
plot(x, y ,xlim=range(x.grid) ,cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit ,lwd =2, col =" blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic : slightly better
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)
dev.off()



# Step functions ----------------------------------------------------------
x <- train_data$TotalVolunteers
y <- train_data$log_item
# Even bins - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

table(cut(x,10))
# is(cut(x,5))

fit3 <- lm(y ~ cut(x,10))

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit3, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), ylim = c(2.8,10), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic : very bad 
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)
dev.off()



# uneven bins - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
br <- c(0, 2, 5, seq(10,50,by=10), seq(75,100,by=25), max(data$TotalVolunteers))
table(cut(x, breaks = br))

fit4 <- lm(y ~ cut(x, breaks = br))

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit4, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic : not very good
summary(fit4)
ad.test(fit4$residuals)

par(mfrow=c(2,2))
plot(fit4)
dev.off()


# Kernel regression ----------------------------

# Gaussian kernel : bandwidth=5
fit5 <- npreg(x, y, ckertype='gaussian', bws=5) # anche bws = 3 non sembra male

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit5, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

summary(fit5)

# diagnostic 
summary(fit5)
# R2 = 0.532


# adaptive kernel
#   setting bwscaling=T, the supplied bandwidths are interpreted as 'scale factors'
#   the bandwidth for each regression variable is computed by multiplying
#   the scale factor with an adaptive measure of the spread of such variable
#
# NOTA PER I RAGA: Non ho ancora capito bene cosa fa sta roba ma potrebbe essere la svolta

fit6 <- npreg(x, as.numeric(y), ckertype='gaussian', bws=1.8, bwscaling=T) 

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit6, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic
summary(fit6)



# Natural Splines ---------------------------------------------------------
knots = quantile(x, probs=c(seq(0.25, 0.95, by=0.1)))
boundary_knots <- quantile(x, probs=c(0.001, 0.99))
knots
boundary_knots

fit7 = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots))

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds = predict(fit7, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col="darkgrey")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col ="blue", lty =3)
# visualize knots
knots_pred = predict(fit7, list(x=knots))
points(knots, knots_pred, col='blue', pch=19)
boundary_pred = predict(fit7, list(x=boundary_knots))
points(boundary_knots, boundary_pred, col='red', pch=19)

# diagnostic : not bad
summary(fit7)
ad.test(fit7$residuals)
# R2 = 0.571
par(mfrow=c(2,2))
plot(fit7)
dev.off()



# NONPARAMETRIC REGRESSION : Complete ------------------------------------------------

# Adding regressors to the best performing models, that is:
# - NATURAL SPLINES
# - GAUSSIAN KERNEL REGRESSION : non so aggiungere ??
# - POLY


# Natural Splines ---------------------------------------------------------
y = train_data$log_item
x = train_data$TotalVolunteers
knots = quantile(x, probs=c(seq(0.35, 0.95, by=0.1), 0.98, 0.99, 0.996))
boundary_knots <- quantile(x, probs=c(0.01, 0.999))
knots
boundary_knots

model_ns = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots) 
              + EventType + weekend + Season, data=train_data) 

# plot considering TEST data (2018)
prova = test_data
preds = predict(model_ns, list(x=prova$TotalVolunteers, EventType=prova$EventType, 
                               weekend=prova$weekend, Season=prova$Season), se=T)
plot(prova$TotalVolunteers, prova$log_item, cex =.5, col="darkgrey", ylim=c(2,10))
points(prova$TotalVolunteers, preds$fit, pch=16,cex=.5, col ="blue")

# diagnostic : with regressor improved R2 + better fit
summary(model_ns)
# R2 = 0.5809 , Radj = 5799
par(mfrow=c(2,2))
plot(model_ns)
dev.off()

# errore su tutti i dati test
RMSE = sqrt(sum((prova$log_item - preds$fit)^2)/length(prova$log_item))
RMSE # 0.8995 





# Adding AREA variable as offset variable

# NOTA : Ho tolto l'Area dal dataset perchè era inconcludente
#        Quindi questo pezzo di codice NON FUNZIONA!!

area_std = train_data$Area/50
train_data$Area_std = area_std
area_std = test_data$Area/50
test_data$Area_std = area_std

# new model
model_ns = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots) 
              + EventType + weekend + Season, offset = Area_std, data=train_data)

# plot considering TEST data (2018)
prova = test_data
preds = predict(model_ns, list(x=prova$TotalVolunteers, EventType=prova$EventType, 
                               weekend=prova$weekend, Season=prova$Season, Area_std=prova$Area_std), se=T)

plot(prova$TotalVolunteers, prova$log_item, cex =.8, col="darkgrey", ylim=c(2,10))
points(prova$TotalVolunteers, preds$fit, cex=.8, col ="blue")

# diagnostic 
summary(model_ns)

par(mfrow=c(2,2))
plot(model_ns)
dev.off()

# errore medio su tutti i dati test
RMSE = sqrt(sum((prova$log_item - preds$fit)^2)/length(prova$log_item))
RMSE 


# PER I RAGA : alla fine il fitting è un po' peggiore quindi eviterei l'area
#              cerchiamo di usare conformal per avere CI più ampio piuttosto





# Poly --------------------------------------------------------------------

x <- train_data$TotalVolunteers
y <- train_data$log_item

model_poly <- lm(y ~ poly(x , degree=8)+ EventType + weekend + Season, data=train_data)

# plot considering TEST data (2018)
prova = test_data
preds = predict(model_poly, list(x=prova$TotalVolunteers, EventType=prova$EventType, 
                               weekend=prova$weekend, Season=prova$Season), se=T)
plot(prova$TotalVolunteers, prova$log_item, cex =.8, col="darkgrey", ylim=c(2,10))
points(prova$TotalVolunteers, preds$fit, cex=.8, col ="blue")

# diagnostic : with regressor improved R2 + better fit
summary(model_poly)
# R2 = 0.5785 , Radj = 0.5776
par(mfrow=c(2,2))
plot(model_poly)
dev.off()

# errore su tutti i dati test
RMSE = sqrt(sum((prova$log_item - preds$fit)^2)/length(prova$log_item))
RMSE # 0.898


