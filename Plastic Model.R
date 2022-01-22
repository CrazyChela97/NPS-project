#################################################
###       PLASTIC  PREDICTION  MODEL          ###
#################################################



# Packages ----------------------------------------------------------------

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

# considerando la percentuale
plot(CleanUsa$TotalVolunteers, plastic.perc) # no sense
plot(log(CleanUsa$TotalItems), plastic.perc) # no sense


# considerando numero items plastica
plot(CleanUsa$TotalVolunteers, log(plastic.items))
# plastic vs volunteers : same shape as total.items vs volunteers
plot(log(CleanUsa$TotalItems[CleanUsa$Year==2018]), log(plastic.items[CleanUsa$Year==2018]))
# plastic vs total.items : nearly linear relation as expected
plot(y_fit_test, log(plastic.items[CleanUsa$Year==2018]))
# più un casino ma forse più corretto ? 

data = import("RegData.Rdata")
plastic.items[plastic.items==0] = 1
data$log_plastic = log(plastic.items)

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

# Minimum Covariance Determinant (MCD)
dati = data.frame(cbind(x.test, y.test))
fit_MCD = covMcd(x = dati, alpha = .75, nsamp = "best")
fit_MCD
# plot dati
ind_best_subset <- fit_MCD$best
N <- nrow(dati)
p <- ncol(dati)
plot(dati, col = ifelse(1:N %in% ind_best_subset, "black", "red"), pch=19, cex=0.5)

# MOLTO BAD : non lo userei

# Robust Regression -------------------------------------------------------

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
fit_lm = lm(log_plastic ~ log_item, data=test_data)
# LMS : least median of squares
fit_lms = lmsreg(log_plastic ~ log_item, data=test_data)
# LTS : least trimmed squares
fit_lts = ltsReg(log_plastic ~ log_item, alpha=.75, mcd=TRUE, data=test_data)

# plot comparison
plot(x.test, y.test, col='darkgrey')
abline(fit_lm, col="red", lwd=2)
abline(fit_lms, col="darkblue", lwd=2)
abline(fit_lts, col="darkgreen", lwd=2)
legend("bottomright", c('OLS', 'LMS', 'LTS'), lwd=rep(2,4), col=c("red", "darkblue", "darkgreen"))

# graphical analysis
plot(fit_lts)
