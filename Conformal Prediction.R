#################################################
###       CONFORMAL PREDICTION MODEL          ###
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

plot(x.test, y.test, ylim=range(c(c_preds$up,c_preds$lo)), cex = 0.5, col="darkgrey")
lines(x.grid, c_preds$pred, lwd=2, col="red")
matlines(x.grid, cbind(c_preds$up,c_preds$lo), lwd=2, col="blue", lty =3)
# ok nice




#split framework ?
c_preds_split=conformal.pred.split(design_matrix, log_item, pred_grid, 
                                   alpha=0.05,verbose=T,train.fun = lm_train, predict.fun = lm_predict,)


plot(TotalVolunteers, log_item, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid,c_preds_split$pred ,lwd =2, col ="red",lty=3)
matlines(volunt.grid ,cbind(c_preds_split$up,c_preds_split$lo) ,lwd =1, col =" red",lty =3)



# Poly --------------------------------------------------------------------
##model 2 - 3: Poly and Natural splines con regressori 
test_data = data[which(data$Year == 2018), ]
train_data = data[which(data$Year == 2016 | data$Year == 2017), ]

x <- train_data$TotalVolunteers
y <- train_data$log_item

model_poly <- lm(y ~ poly(x , degree=8)+ EventType + weekend + Season, data=train_data)

train_poly=function(x1,y1,out=NULL){
  lm(y1 ~ poly(x1 , degree=8) + train_data$EventType + train_data$weekend + train_data$Season)
}

predict_poly=function(obj, new_x){
  predict(obj,new_x)$y
}

#pippo=train_ss(x,y)
#predict_ss(pippo, as.data.frame(train_data$TotalVolunteers))

#questo runna in 5 ore e un quarto raga
volunt.grid = seq(range(x)[1],range(y)[2], length.out = dim(train_data)[1])

design_matrix = matrix(poly(x , degree=8), nrow=length(y))
pred_grid = matrix(poly(x,degree=8,coefs = attr(poly(x,degree=8),"coefs") ),ncol=2)

c_preds=conformal.pred(design_matrix, y, pred_grid, alpha=0.05,
                       verbose=T,train.fun = train_poly, predict.fun = predict_poly, num.grid.pts = 200)

plot(x, y, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid, c_preds$pred, lwd =2, col ="red", lty=3)
matlines(volunt.grid, cbind(c_preds$up,c_preds$lo) ,lwd =1, col =" blue",lty =3)



# Poly Model - COMPLETE -------------------------------------------------------------

lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun

# Design Matrix with Regressors
dummies = dummy_cols(train_data, select_columns = c('EventType', 'Season'))
design_matrix = matrix(poly(x.train, degree=8), ncol=8)
design_matrix = cbind(design_matrix, dummies[ ,c(10, 12:14, 16:18)], train_data$weekend)
design_matrix = as.matrix(design_matrix)
# Design Matrix of New Obs
dummies = dummy_cols(test_data, select_columns = c('EventType', 'Season'))
pred_grid = matrix(poly(x.test, degree=8, coefs = attr(poly(x.train, degree=8), "coefs")), ncol=8)
pred_grid = cbind(pred_grid, dummies[ ,c(10, 12:14, 16:18)], test_data$weekend)
pred_grid = as.matrix(pred_grid)

c_preds = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)

plot(x.test, y.test, ylim=range(c(c_preds$up, c_preds$lo)), cex = 0.5, col="darkgrey")
plotCI(x.test, c_preds$pred, ui=c_preds$up, li=c_preds$lo, add=TRUE, pch=16)



# Natural Splines - NO Covariates -----------------------------------------
knots = quantile(x, probs=c(seq(0.35, 0.95, by=0.1), 0.98, 0.99, 0.996))
boundary_knots = quantile(x, probs=c(0.01, 0.999))
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

design_matrix = ns(x.train, knots=knots, Boundary.knots=boundary_knots)
pred_grid = matrix(ns(x.grid, knots=knots, Boundary.knots=boundary_knots), nrow=length(x.grid))

c_preds = conformal.pred(design_matrix, y.train, pred_grid, alpha=0.05, verbose=T, 
                         train.fun=lm_train, predict.fun=lm_predict, num.grid.pts = 200)

plot(x.test, y.test, ylim=range(c(c_preds$up,c_preds$lo)), cex = 0.5, col="darkgrey")
lines(x.grid, c_preds$pred, lwd=2, col="red")
matlines(x.grid, cbind(c_preds$up,c_preds$lo), lwd=2, col="blue", lty =3)
# ok nice



# Natural Splines - COMPLETE -----------------------------------------
