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
# View(CleanUsa)

plastic.perc = CleanUsa$`%Plastic&Foam`/100
plastic.items = round(CleanUsa$TotalClassifiedItems_EC2020 * plastic.perc)
other.items = round(CleanUsa$TotalClassifiedItems_EC2020 - plastic.items)

# Considering the percentage :
plot(CleanUsa$TotalVolunteers, plastic.perc) # no sense
plot(log(CleanUsa$TotalItems), plastic.perc) # no sense


# Considering the number of Plastic Items :
plot(CleanUsa$TotalVolunteers, log(plastic.items))
# plastic vs volunteers : same shape as total.items vs volunteers
plot(CleanUsa$TotalItems, plastic.items)
# plastic vs total.items : linear trend with dispersion
plot(log(CleanUsa$TotalItems), log(plastic.items))
# log(plastic) vs log(total.items) : nearly linear relation as expected


# Considering the number of Other Items :
plot(CleanUsa$TotalVolunteers, log(other.items))
# others vs volunteers : similar shape
plot(CleanUsa$TotalItems, other.items)
# others vs total.items : data cloud with increasing trend
plot(log(CleanUsa$TotalItems), log(other.items))
# log(others) vs log(total.items) : nearly linear but more variable


data = import("RegData.Rdata")
plastic.items[plastic.items==0] = 1
other.items[other.items==0] = 1
data$log_plastic = log(plastic.items)
data$log_others = log(other.items)

# Robust Regression : plastic -------------------------------------------------------

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]

y.train = train_data$log_plastic
x.train = train_data$log_item
y.test = test_data$log_plastic
x.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=0.1)

# basic OLS
fit_lm = lm(log_plastic ~ log_item, data=train_data)
# LMS : least median of squares
fit_lms = lmsreg(log_plastic ~ log_item, data=train_data)
# LTS : least trimmed squares
fit_lts = ltsReg(log_plastic ~ log_item, alpha=.75, mcd=TRUE, data=train_data)

# MM-type robust estimators
# fit_rob = lmrob(log_plastic ~ log_item, data=train_data, method = 'MM')


# plot comparison
plot(x.train, y.train, col='darkgrey', main='Model Comparison',
     xlab='Collected Items', ylab='Plastic Items', cex=0.7)
abline(fit_lm, col="red3", lwd=3)
abline(fit_lms, col="darkorange", lwd=3)
abline(fit_lts, col="blue", lwd=3)
# abline(fit_rob, col='deeppink', lwd=2)
legend("topleft", c('OLS', 'LMS', 'LTS'), lwd=rep(3,3), 
       col=c("red3", "darkorange", "blue"))


# graphical analysis
plot(fit_lts) 
# performances
summary(fit_lm)
summary(fit_lms)
summary(fit_lts)


# CONFIDENCE INTERVALS 

# LM_ROB SE
pred = predict(fit_rob, list(log_item = x.grid), se=TRUE)
plot(x.test, y.test, col='darkgrey')
lines(x.grid, pred$fit, col='red', lwd=2)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
matlines(x.grid, se.bands, lwd =1, col =" blue", lty =2)
legend('topleft', c('fitted values', 'lm_rob CI (using SE)', 'using residual.scale'), lwd=c(2,2,2),
       lty=c(1,2,1), col=c('red', 'blue', 'darkorange'))

# troppo troppo strette si sovrappongono a fit
range(pred$se.fit) # infatti sono strettissimi

lines(x.grid, pred$fit+pred$residual.scale, col='darkorange', lwd=2)
lines(x.grid, pred$fit-pred$residual.scale, col='darkorange', lwd=2)
# questi più bellini ma non so quanto abbia senso
help(lmrob)


# BOOTSTRAP per CI

# Naive Bootstrap for the intercept
pred = fit_lts$fitted.values
fitted.obs = pred
res.obs = y.train - fitted.obs
L.obs = summary(fit_lts)$coefficients[1,1]
B = 2000
alpha = 0.01 # provare ?
T.boot = numeric(B)
set.seed(2022)
for(b in 1:B) {
  response.b = fitted.obs + sample(res.obs, replace = T)
  fm.b = ltsReg(response.b ~ x.train, alpha=.75, mcd=TRUE)
  T.boot[b] = summary(fm.b)$coefficients[1,1]
}

# Reverse Percentile Intervals
right.quantile = quantile(T.boot, 1 - alpha/2)
left.quantile = quantile(T.boot, alpha/2)

CI.RP = c(L.obs-(right.quantile - L.obs), L.obs-(left.quantile - L.obs))
CI.RP

# plot
plot(x.train, y.train, col='darkgrey')
C2 = summary(fit_lts)$coefficients[2,1]
lines(x.train, fit_lts$fitted.values, col='red')
lines(x.train, CI.RP[1]+ x.train*C2, col='blue')
lines(x.train, CI.RP[2]+ x.train*C2, col='blue')
legend('topleft', c('fitted values', 'bootstrap CI'), lwd=c(2,2), col=c('red', 'blue'))

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
plot(fit_lts) 

# prediction
pred = predict(fit_lms, list(log_item = x.test))
plot(x.test, y.test, col='darkgrey')
lines(x.test, pred, col='darkblue')



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

# Hand coding for the tolerance ellipse plot
# since having many data from the default output it isn't clear
plot(dati, xlim=c(1.5,10), ylim=c(0,10), col='grey60', cex=0.7,
     xlab='Collected Items', ylab='Plastic Items', main='Tolerance Ellipse')
# robust
lines(ellipse(x =fit_MCD$cov, centre=fit_MCD$center), col="darkorange", lwd=3)
points(x=fit_MCD$center[1], y=fit_MCD$center[2], bg="darkorange", pch=21, cex=1.5)
# classical
n = nrow(dati)
sample_mean = apply(dati, 2, mean)
sample_cov = cov(dati)
lines(ellipse(x = sample_cov, centre=sample_mean), col="deepskyblue",lwd=3)
points(x=sample_mean[1],y=sample_mean[2], bg="deepskyblue", pch=21, cex=1.5)
legend("topleft", c('Robust', 'Classical'), lwd=rep(2,2), col=c("darkorange","deepskyblue"))




# OTHERS #
# Minimum Covariance Determinant (MCD)
y.train = train_data$log_others
x.train = train_data$log_item
dati = data.frame(cbind(x.train, y.train))
fit_MCD = covMcd(x = dati, alpha = .75, nsamp = "best")
fit_MCD

# Graphic analysis
plot(fit_MCD, classic=TRUE, labels.id=F)

# Hand Coding
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





