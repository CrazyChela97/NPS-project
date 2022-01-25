#################################################
###       CONFORMAL PREDICTION MODEL          ###
#################################################

current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list=ls())

# Packages ----------------------------------------------------------------

library(rio)
library(roahd)
library(MASS)
library(rgl)
library(DepthProc)
library(aplpack)
library(robustbase)
library(ISLR2)
library(car)
library(mgcv)
library(splines)
library(pbapply)
library(conformalInference)
library(splines)
library(plotrix)
library(fastDummies)


# Training vs Test Data ---------------------------------------------------

data = import("RegData.Rdata")

# We use data from 2016 & 2017 as training dataset for the models
# We then test the GOF of the model using 2018 data
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]


# Poly Model - NO Covariates  -----------------------------------------------

# fitting model over training data
model_poly = lm(log_item ~ poly(TotalVolunteers,degree=8), data=train_data)
summary(model_poly)

# prediction made over test data
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

preds = predict(model_poly, list(TotalVolunteers = x.grid), se=T)
plot(x.test, y.test, xlim=range(x.grid), cex = 0.5, col="darkgrey ")
lines(x.grid, preds$fit ,lwd =2, col =" blue")


 
# CONFORMAL PREDICTION
lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

x.train = train_data$TotalVolunteers
y.train = train_data$log_item
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

design_matrix = matrix(poly(x.train, degree=8), ncol=8)
pred_grid = matrix(poly(x.grid, degree=8, coefs = attr(poly(x.train, degree=8),"coefs")), ncol=8)

c_preds = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)

plot(x.test, y.test, ylim=range(c(c_preds$up,c_preds$lo)), cex = 0.5, col="gray40",
     main='Polynomial Conformal Prediction', xlab='Total Volunteers', ylab='Collected Items')
lines(x.grid, c_preds$pred, lwd=3, col="blue")
matlines(x.grid, cbind(c_preds$up,c_preds$lo), lwd=2, col="deepskyblue", lty =1)
n = length(x.grid)
polygon(x=c(x.grid[1:n], x.grid[n:1]), y=c(c_preds$up[1:n], c_preds$lo[n:1]), 
        col='lightskyblue', border = NA, density=20)
# ok nice




#split framework ?
c_preds_split=conformal.pred.split(design_matrix, log_item, pred_grid, 
                                   alpha=0.05,verbose=T,train.fun = lm_train, predict.fun = lm_predict,)


plot(TotalVolunteers, log_item, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid,c_preds_split$pred ,lwd =2, col ="red",lty=3)
matlines(volunt.grid ,cbind(c_preds_split$up,c_preds_split$lo) ,lwd =1, col =" red",lty =3)




# Poly Model - COMPLETE -------------------------------------------------------------
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)


lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

# Design Matrix with Regressors
dummies = dummy_cols(train_data, select_columns = c('EventType', 'Season'))
interaction = dummies$`EventType_Marine Debris`*dummies$Season_Summer
design_matrix = matrix(poly(x.train, degree=8), ncol=8)
design_matrix = cbind(design_matrix, dummies[ ,c(10, 12:14, 16:18)], interaction)
design_matrix = as.matrix(design_matrix)
# Design Matrix of New Obs
dummies = dummy_cols(test_data, select_columns = c('EventType', 'Season'))
interaction = dummies$`EventType_Marine Debris`*dummies$Season_Summer
pred_grid = matrix(poly(x.test, degree=8, coefs = attr(poly(x.train, degree=8), "coefs")), ncol=8)
pred_grid = cbind(pred_grid, dummies[ ,c(10, 12:14, 16:18)], interaction)
pred_grid = as.matrix(pred_grid)

c_preds_poly = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)

plot(x.test, y.test, ylim=range(c(c_preds_poly$up, c_preds_poly$lo)), cex = 0.5, col="gray40",
     main='Polynomial Conformal Prediction', xlab='Total Volunteers', ylab='Collected Items')
plotCI(x.test, c_preds_poly$pred, ui=c_preds_poly$up, li=c_preds_poly$lo, add=TRUE, pch=16,
       cex=0.7, slty=2, scol='lightskyblue', col='blue', lwd=0.8)


# GOF check
excluded = 0
for (i in 1:length(y.test)){
  excluded = excluded + (y.test[i]<c_preds_poly$lo[i]) + (y.test[i]>c_preds_poly$up[i])
}
exc.perc.poly = excluded/length(y.test)
exc.perc.poly # 5.65%




# Natural Splines - NO Covariates -----------------------------------------
knots = quantile(x.train, probs=c(seq(0.35, 0.95, by=0.1), 0.98, 0.99, 0.996))
boundary_knots = quantile(x.train, probs=c(0.01, 0.999))
knots
boundary_knots

# fitting model over training data
model_ns = lm(log_item ~ ns(TotalVolunteers, knots=knots, Boundary.knots=boundary_knots), data = train_data)
summary(model_ns)

# prediction made over test data
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

preds = predict(model_ns, list(TotalVolunteers = x.grid), se=T)
plot(x.test, y.test, xlim=range(x.grid), cex = 0.5, col="darkgrey")
lines(x.grid, preds$fit ,lwd =2, col =" blue")



# CONFORMAL PREDICTION
lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

x.train = train_data$TotalVolunteers
y.train = train_data$log_item
x.test = test_data$TotalVolunteers
y.test = test_data$log_item
x.grid = seq(range(x.test)[1], range(x.test)[2], by=1)

design_matrix = ns(x.train, knots=knots, Boundary.knots=boundary_knots)
pred_grid = matrix(ns(x.grid, knots=knots, Boundary.knots=boundary_knots), nrow=length(x.grid))

c_preds = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)

plot(x.test, y.test, ylim=range(c(c_preds$up,c_preds$lo)), cex = 0.5, col="gray40",
     main='Natural Splines Conformal Prediction', xlab='Total Volunteers', ylab='Collected Items')
lines(x.grid, c_preds$pred, lwd=3, col="orangered3")
matlines(x.grid, cbind(c_preds$up,c_preds$lo), lwd=2, col="darkorange2", lty =1)
n = length(x.grid)
polygon(x=c(x.grid[1:n], x.grid[n:1]), y=c(c_preds$up[1:n], c_preds$lo[n:1]), 
        col='tan1', border = NA, density=20)
# ok nice



# Natural Splines - COMPLETE -----------------------------------------

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

c_preds_ns = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)


plot(x.test, y.test, ylim=range(c(c_preds_ns$up, c_preds_ns$lo)), cex = 0.5, col="gray40", 
     main='Natural Splines Conformal Prediction', xlab='Total Volunteers', ylab='Collected Items')
plotCI(x.test, c_preds_ns$pred, ui=c_preds_ns$up, li=c_preds_ns$lo, add=TRUE, 
       pch=16, cex=0.7, slty=2, scol='orange', col='orangered3', lwd=0.7)


# GOF check
excluded = 0
for (i in 1:length(y.test)){
  excluded = excluded + (y.test[i]<c_preds_ns$lo[i]) + (y.test[i]>c_preds_ns$up[i])
}
exc.perc.ns = excluded/length(y.test)
exc.perc.ns # 5.61 %


