
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
CleanUsa=import("CleanUsa.Rdata")
View(CleanUsa)

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


# GAM Model -----------------------------------------------------------

gam_model = gam(log_item ~ s(TotalVolunteers, by=factor(Season), bs='cr') + Area + 
                  weekend + EventType + as.factor(Year), data=data)
summary(gam_model) 
# dal summary sembra tutto bellino

par(mfrow=c(2,2))
plot(gam_model)
dev.off()
# il plot non ha alcun senso

plot(data$TotalVolunteers, data$log_item)
par(new=TRUE)
plot(gam_model, col='red')




# NONPARAMETRIC REGRESSION ------------------------------------------------

# Poly --------------------------------------------------------------------
x <- data$TotalVolunteers
y <- data$log_item

# Choose the degree of the polynomial
m_list <- lapply(1:10, function(degree){lm(y ~ poly(x, degree=degree), data=data)})
do.call(anova, m_list)

fit <- lm(y ~ poly(x , degree=8), data=data) # offset = Area
summary(fit)

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=0.5)
preds <- predict(fit, list(x=x.grid), se=T)
plot(x, y ,xlim=range(x.grid) ,cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit ,lwd =2, col =" blue")

# PROBLEMA : nel fitting ad ogni x sono associate più y in base ai valori delle dummies!
#             bisogna capire come gestirlo!

# plot se.bands
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

summary(fit)
plot(fit)

# Rifaccio il modello con poly togliendo il dato #3588 dato ha elevata Leverage

data2 <- data[-3588,]
x <- data2$TotalVolunteers
y <- data2$log_item
m_list <- lapply(1:10, function(degree){lm(y ~ poly(x, degree=degree), data=data2)})
do.call(anova, m_list)

fit2 <- lm(y ~ poly(x , degree=8), data=data) # offset = Area
summary(fit2)

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=0.5)
preds <- predict(fit2, list(x=x.grid), se=T)
plot(x, y ,xlim=range(x.grid) ,cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit ,lwd =2, col =" blue")

# PROBLEMA : nel fitting ad ogni x sono associate più y in base ai valori delle dummies!
#             bisogna capire come gestirlo!

# plot se.bands
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

summary(fit2)
plot(fit2)

# Step functions ----------------------------------------------------------

# Even bins - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

table(cut(x,4))
is(cut(x,4))

fit3 <- lm(y ~ cut(x,4))

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=0.5)
preds <- predict(fit3, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

# diagnostic
summary(fit3)
plot(fit3)

# uneven bins - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
br <- c(seq(0,50,by=10),seq(75,100,by=25), seq(150,250,by=100))
br
table(cut(x, breaks = br))

# model
fit4 <- lm(y ~ cut(x, breaks = br))

# plot
x.grid <- seq(range(x)[1], range(x)[2], by=1)
preds <- predict(fit4, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)


# diagnostic
summary(fit4)
plot(fit4)


# Weighted local averaging / Kernel regression ----------------------------

# bandwidth=5, gaussian kernel
fit5 <- npreg(x, y, ckertype='gaussian', bws=5) # anche bws = 3 non sembra male
x.grid <- seq(range(x)[1], range(x)[2], by=0.5)
preds <- predict(fit5, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

summary(fit5)

# adaptive kernel
#   setting bwscaling=T, the supplied bandwidths are interpreted as 'scale factors'
#   the bandwidth for each regression variable is computed by multiplying
#   the scale factor with an adaptive measure of the spread of such variable
# NOTA PER I RAGA: Non ho ancora capito bene cosa fa sta roba ma potrebbe essere la svolta
fit6 <- npreg(x, as.numeric(y), ckertype='gaussian', bws=1.8, bwscaling=T) 
x.grid <- seq(range(x)[1], range(x)[2], by=0.5)
preds <- predict(fit6, list(x=x.grid), se=T)
plot(x, y, xlim=range(x.grid), cex =.5, col =" darkgrey ", main="")
lines(x.grid, preds$fit, lwd =2, col ="blue")
se.bands <- cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)

summary(fit6)

# Natural Splines ---------------------------------------------------------
knots = quantile(x, probs=seq(0.25, 0.95, by=0.1))
boundary_knots <- quantile(x, probs=c(0.01, 0.99))

model_ns = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots))
summary(model_ns)
preds = predict(model_ns, list(x=x.grid), se=T)
se.bands = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)

plot(x, y, xlim=range(x.grid), cex =.5, col="darkgrey")
lines(x.grid, preds$fit, lwd =2, col ="blue")
matlines(x.grid, se.bands, lwd =1, col ="blue", lty =3)
# visualize knots
knots_pred = predict(model_ns, list(x=knots))
points(knots, knots_pred, col='blue', pch=19)
boundary_pred = predict(model_ns, list(x=boundary_knots))
points(boundary_knots, boundary_pred, col='red', pch=19)



# aggiungo regressori
area_std = data$Area-mean(data$Area)

model_ns = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots) 
              + as.factor(EventType) + weekend + Season, data=data)
summary(model_ns)

preds = predict(model_ns, list(x=x, EventType=data$EventType, 
                               weekend=data$weekend, Season=data$Season), se=T)
se.bands = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)

plot(x, y, xlim=range(x.grid), cex =.5, col="darkgrey")
points(x, preds$fit, cex=.6, col ="blue")
points(x, se.bands[,1], cex=.3, col ="red", pch=16)
points(x, se.bands[,2], cex=.3, col ="red", pch=16)
# visualize knots
knots_pred = predict(model_ns, list(x=knots))
points(knots, knots_pred, col='blue', pch=19)
boundary_pred = predict(model_ns, list(x=boundary_knots))
points(boundary_knots, boundary_pred, col='red', pch=19)