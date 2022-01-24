#################################################
###       PLASTIC  PREDICTION  MODEL          ###
#################################################

current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list=ls())

# Packages ----------------------------------------------------------------

library(rio)
library(robustbase)
library(psych)
library(MASS)
library(ellipse)
library(here)
library(DescTools)
library(knitr)
library(RobStatTM)


# Data Analysis -----------------------------------------------------------

CleanUsa = import("CleanUsa.Rdata")

plastic.perc = CleanUsa$`%Plastic&Foam`/100
plastic.items = round(CleanUsa$TotalClassifiedItems_EC2020 * plastic.perc)
other.items = round(CleanUsa$TotalClassifiedItems_EC2020 - plastic.items)

# Considering the percentage
plot(CleanUsa$TotalVolunteers, plastic.perc) # no sense
plot(log(CleanUsa$TotalItems), plastic.perc) # no sense


# Considering the number of Plastic Items
plot(CleanUsa$TotalVolunteers, log(plastic.items))
# plastic vs volunteers : same shape as total.items vs volunteers
plot(log(CleanUsa$TotalItems), log(plastic.items))
# plastic vs total.items : nearly linear relation as expected

# manca fitted

# Considering the number of Other Items
plot(CleanUsa$TotalVolunteers, log(other.items))
# others vs volunteers : similar shape
plot(log(CleanUsa$TotalItems), log(other.items))
# others vs total.items : nearly linear but more variable


data = import("RegData.Rdata")
plastic.items[plastic.items==0] = 1
other.items[other.items==0] = 1
data$log_plastic = log(plastic.items)
data$log_others = log(other.items)

# Analysis for Categorical Regressors -------------------------------------

# YEAR
cat = levels(factor(data$Year))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Year == cat[k]), ]
  plot(dati$log_item, log(dati$plastic_items), main=cat[k])
}


# MONTH
cat = levels(factor(data$Month))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Month == cat[k]), ]
  plot(dati$log_item, log(dati$plastic_items), main=cat[k])
}


# EVENT TYPE
cat = levels(factor(data$EventType))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$EventType == cat[k]), ]
  plot(dati$log_item, log(dati$plastic_items), main=cat[k])
}


# SEASON
cat = levels(factor(data$Season))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Season == cat[k]), ]
  plot(dati$log_item, log(dati$plastic_items), main=cat[k])
}


# WEEKEND
cat = levels(factor(data$weekend))
c = length(cat)

par(mfrow=c(c/2,c))
for (k in 1:c){
  dati = data[which(data$weekend == cat[k]), ]
  plot(dati$log_item, log(dati$plastic_items), main=cat[k])
}

dev.off()

# EventType con colori by cami
Land=data[which(data$EventType=='Land Cleanup'),]
Marine=data[which(data$EventType=='Marine Debris'),]
Under=data[which(data$EventType=='Underwater Cleanup'),]
Water=data[which(data$EventType=='Watercraft Cleanup'),]

plot(Land$log_item, log(Land$plastic_items), col = 'green')
points(Marine$log_item, log(Marine$plastic_items), col = 'orange1')
points(Water$log_item, log(Water$plastic_items), col = 'blue')
points(Under$log_item, log(Under$plastic_items), col = 'red3')




# Outliers Robust ---------------------------------------------------------

# PLASTIC #
# Minimum Covariance Determinant (MCD)
y.train = train_data$log_plastic
x.train = train_data$log_item
dati = data.frame(cbind(x.train, y.train))
fit_MCD = covMcd(x = dati, alpha = .75, nsamp = "best")
fit_MCD

# Graphic analysis
plot(fit_MCD, classic=TRUE, labels.id=F)
# MOLTO BAD : non lo userei
# non toglie numeri : va fatta a mano
plot(dati, xlim=c(1.5,10),ylim=c(0,10), col='darkgrey')
# robust
lines(ellipse(x =fit_MCD$cov, centre=fit_MCD$center), col="red", lwd=2)
points(x=fit_MCD$center[1], y=fit_MCD$center[2], col="red", pch=16, cex=1.5)
# classical
n = nrow(dati)
sample_mean = apply(dati, 2, mean)
sample_cov = cov(dati)
lines(ellipse(x = sample_cov, centre=sample_mean), col="blue",lwd=2)
points(x=sample_mean[1],y=sample_mean[2], col="blue", pch=16, cex=1.5)
legend("topleft", c('Robust', 'Classical'), lwd=rep(2,2), col=c("red", "blue"))


# OTHERS #
# Minimum Covariance Determinant (MCD)
y.train = train_data$log_others
x.train = train_data$log_item
dati = data.frame(cbind(x.train, y.train))
fit_MCD = covMcd(x = dati, alpha = .75, nsamp = "best")
fit_MCD

# Graphic analysis
plot(fit_MCD, classic=TRUE, labels.id=F)
# MOLTO BAD : non lo userei
# non toglie numeri : va fatta a mano
plot(dati, xlim=c(1,10),ylim=c(-0.5,8), col='darkgrey')
# robust
lines(ellipse(x =fit_MCD$cov, centre=fit_MCD$center), col="red", lwd=2)
points(x=fit_MCD$center[1], y=fit_MCD$center[2], col="red", pch=16, cex=1.5)
# classical
n = nrow(dati)
sample_mean = apply(dati, 2, mean)
sample_cov = cov(dati)
lines(ellipse(x = sample_cov, centre=sample_mean), col="blue",lwd=2)
points(x=sample_mean[1],y=sample_mean[2], col="blue", pch=16, cex=1.5)
legend("topleft", c('Robust', 'Classical'), lwd=rep(2,2), col=c("red", "blue"))

# Robust Regression : plastic -------------------------------------------------------

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]

y.train = train_data$log_plastic
x.train = train_data$log_item
y.test = test_data$log_plastic
x.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

# basic OLS
fit_lm = lm(log_plastic ~ log_item, data=train_data)
# LMS : least median of squares
fit_lms = lmsreg(log_plastic ~ log_item, data=train_data)
# LTS : least trimmed squares
fit_lts = ltsReg(log_plastic ~ log_item, alpha=.75, mcd=TRUE, data=train_data)
# MM-type robust estimators
fit_rob = lmrob(log_plastic ~ log_item, data=train_data, method = 'MM', y=T)


# plot comparison
plot(x.train, y.train, col='darkgrey')
abline(fit_lm, col="red", lwd=2)
abline(fit_lms, col="darkblue", lwd=2)
abline(fit_lts, col="darkgreen", lwd=2)
legend("bottomright", c('OLS', 'LMS', 'LTS'), lwd=rep(2,4), col=c("red", "darkblue", "darkgreen"))

plot(x.train, y.train, col='darkgrey')
abline(fit_rob, col='red')

# graphical analysis
plot(fit_lts) # bad

pred = predict(fit_rob, list(log_item = x.grid), se=TRUE)
plot(x.test, y.test, col='darkgrey')
lines(x.grid, pred$fit, col='red', lwd=2)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =3)
# troppo troppo strette si sovrappongono a fit
range(pred$se.fit) # infatti sono strettissimi

lines(x.grid, pred$fit+pred$residual.scale, col='darkorange')
lines(x.grid, pred$fit-pred$residual.scale, col='darkorange')
# questi più bellini ma non so quanto abbia senso
help(lmrob)

# BOOTSTRAP per CI
# anche questi strettissimi

# Naive Bootstrap
pred = predict(fit_rob)
fitted.obs = pred
res.obs = y.train - fitted.obs
L.obs = summary(fit_rob)$coefficients[1,1]
B = 2000
alpha = 0.05
T.boot = numeric(B)
set.seed(2022)
for(b in 1:B) {
  response.b = fitted.obs + sample(res.obs, replace = T)
  fm.b = lmrob(response.b ~ x.train, method = 'MM', y=T)
  T.boot[b] = summary(fm.b)$coefficients[1,1]
}

# Reverse Percentile Intervals
right.quantile = quantile(T.boot, 1 - alpha/2)
left.quantile = quantile(T.boot, alpha/2)

CI.RP = c(L.obs-(right.quantile - L.obs), L.obs-(left.quantile - L.obs))
CI.RP

C2 = summary(fit_rob)$coefficients[2,1]
lines(x.grid, CI.RP[1]+ x.grid*C2, col='blue')
lines(x.grid, CI.RP[2]+ x.grid*C2, col='blue')

# Robust Regression : others -------------------------------------------------------

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]

y.train = train_data$log_others
x.train = train_data$log_item
y.test = test_data$log_others
x.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

# basic OLS
fit_lm = lm(log_others ~ log_item, data=train_data)
# LMS : least median of squares
fit_lms = lmsreg(log_others ~ log_item, data=train_data)
# LTS : least trimmed squares
fit_lts = ltsReg(log_others ~ log_item, alpha=.75, mcd=TRUE, data=train_data)

# plot comparison
plot(x.train, y.train, col='darkgrey')
abline(fit_lm, col="red", lwd=2)
abline(fit_lms, col="darkblue", lwd=2)
abline(fit_lts, col="darkgreen", lwd=2)
legend("topleft", c('OLS', 'LMS', 'LTS'), lwd=rep(2,4), col=c("red", "darkblue", "darkgreen"))

# graphical analysis
plot(fit_lts) # bad

pred = predict(fit_lms, list(log_item = x.test))
plot(x.test, y.test, col='darkgrey')
lines(x.test, pred, col='darkblue')



# Esempio -----------------------------------------------------------------








