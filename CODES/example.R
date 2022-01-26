#################################################
###             REAL LIFE EXAMPLE             ###
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
library(roahd)
library(rgl)
library(DepthProc)
library(aplpack)
library(ISLR2)
library(car)
library(mgcv)
library(splines)
library(pbapply)
library(conformalInference)
library(plotrix)
library(fastDummies)




# Data Import -------------------------------------------------------------

CleanUsa = import("CleanUsa.Rdata")
plastic.perc = CleanUsa$`%Plastic&Foam`/100
plastic.items = round(CleanUsa$TotalClassifiedItems_EC2020 * plastic.perc)
other.items = round(CleanUsa$TotalClassifiedItems_EC2020 - plastic.items)

data = import("RegData.Rdata")

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]

# Total Items Prediction --------------------------------------------------

n = dim(test_data)[1]
set.seed(12101997)
obs = sample(1:n, 1)
View(test_data[obs, ])

x.train = train_data$TotalVolunteers
y.train = train_data$log_item
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
#x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)


# CPI using NS model
knots = quantile(x.train, probs=c(seq(0.35, 0.95, by=0.1), 0.98, 0.99, 0.996))
boundary_knots = quantile(x.train, probs=c(0.01, 0.999))

lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

# Design Matrix with Regressors
dummies = dummy_cols(train_data, select_columns = c('EventType', 'Season'))
interaction = dummies$`EventType_Marine Debris`*dummies$Season_Summer
design_matrix = ns(x.train, knots=knots, Boundary.knots=boundary_knots)
design_matrix = cbind(design_matrix, dummies[ ,c(10, 12:14, 16:18)], interaction)
design_matrix = as.matrix(design_matrix)
# Design Matrix of New Obs
dummies = dummy_cols(test_data, select_columns = c('EventType', 'Season'))
interaction = dummies$`EventType_Marine Debris`*dummies$Season_Summer
pred_grid = matrix(ns(x.test, knots=knots, Boundary.knots=boundary_knots), nrow=length(x.test))
pred_grid = cbind(pred_grid, dummies[ ,c(10, 12:14, 16:18)], interaction)
pred_grid = as.matrix(pred_grid)
x_new = pred_grid[obs, ]
c_preds_ns = conformal.pred(design_matrix, y.train, x_new, alpha=0.05, verbose=T, 
                            train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)


CI = c(c_preds_ns$lo, c_preds_ns$up)
real = log(test_data$TotalItems)[obs]



# Plastic Prediction ------------------------------------------------------
plastic.items[plastic.items==0] = 1
other.items[other.items==0] = 1
data$log_plastic = log(plastic.items)
data$log_others = log(other.items)

test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]
# LMS : least median of squares
fit_lms = lmsreg(log_plastic ~ log_item, data=train_data)
x_new = as.numeric(c_preds_ns$pred)

pred = predict(fit_lms, list(log_item=x_new))
real_plastic = test_data$log_plastic[obs]



# Final Results -----------------------------------------------------------

# total items
predicted_TI = c(CI[1], c_preds_ns$pred, CI[2])
real_TI = real
predicted_TI
real_TI

# plastic
pred_p = pred
pred_p
real_p = real_plastic
real_p




