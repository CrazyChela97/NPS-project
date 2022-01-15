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


# Data Transformation -----------------------------------------------------
CleanUsa=import("CleanUsa.Rdata")

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


#parentesi che non c'entra nulla: voglio provare a vedere come si distribuisce la nube in base a Event Type
Land=data[which(data$EventType=='Land Cleanup'),]
Marine=data[which(data$EventType=='Marine Debris'),]
Under=data[which(data$EventType=='Underwater Cleanup'),]
Water=data[which(data$EventType=='Watercraft Cleanup'),]

plot(Land$TotalVolunteers, Land$log_item, col = 'green')
points(Marine$TotalVolunteers, Marine$log_item, col = 'yellow')
points(Water$TotalVolunteers, Water$log_item, col = 'blue')
points(Under$TotalVolunteers, Under$log_item, col = 'red')

#first reaction: SHOCK 
#marine debris totalmente fuori dalla shape: forse si possono fare modelli distinti??


#  Conformal Models  -----------------------------------------------

##model 1: conformal prediction without covariates
attach(data)

model_poly=lm(log_item ~ poly(TotalVolunteers,degree=8))
summary(model_poly)

volunt.grid=seq(range(TotalVolunteers)[1],range(TotalVolunteers)[2], length.out = 300)

preds=predict(model_poly,list(TotalVolunteers=volunt.grid),se=T)

plot(TotalVolunteers, log_item, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid, preds$fit ,lwd =2, col =" blue")
#polynomial regression model 


design_matrix=matrix(poly(TotalVolunteers,degree=2),ncol=2)
pred_grid=matrix(poly(volunt.grid,degree=2,coefs = attr(poly(TotalVolunteers,degree=2),"coefs") ),ncol=2)

c_preds=conformal.pred(design_matrix, log_item, pred_grid, alpha=0.05,
                       verbose=T,train.fun = lm_train, predict.fun = lm_predict, num.grid.pts = 200)

lines(volunt.grid, c_preds$pred, lwd =2, col ="red", lty=3)
matlines(volunt.grid, cbind(c_preds$up,c_preds$lo) ,lwd =1, col =" blue",lty =3)
#questo fit non va bene, provo con più degree

#questo runna in 5 ore e un quarto raga
design_matrix=matrix(poly(TotalVolunteers,degree=8), nrow=length(log_item))
pred_grid=matrix(poly(volunt.grid,degree=8,coefs = attr(poly(TotalVolunteers,degree=8),"coefs") ),ncol=2)
volunt.grid=seq(range(TotalVolunteers)[1],range(TotalVolunteers)[2], length.out = 13354)


c_preds=conformal.pred(design_matrix, log_item, pred_grid, alpha=0.05,
                       verbose=T,train.fun = lm_train, predict.fun = lm_predict, num.grid.pts = 200)

plot(TotalVolunteers, log_item, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid, c_preds$pred, lwd =2, col ="red", lty=3)
matlines(volunt.grid, cbind(c_preds$up,c_preds$lo) ,lwd =1, col =" blue",lty =3)

#split framework
c_preds_split=conformal.pred.split(design_matrix, log_item, pred_grid, 
                                   alpha=0.05,verbose=T,train.fun = lm_train, predict.fun = lm_predict,)


plot(TotalVolunteers, log_item, xlim=range(volunt.grid), cex = 0.5, col="darkgrey ")
lines(volunt.grid,c_preds_split$pred ,lwd =2, col ="red",lty=3)
matlines(volunt.grid ,cbind(c_preds_split$up,c_preds_split$lo) ,lwd =1, col =" red",lty =3)


##model 2: conformal prediction without covariates: lm 
attach(data)

br=c(quantile(TotalVolunteers, probs = c(0.1,0.2,0.3,0.4,0.6,0.8)),15000)
model_cut=lm(log_item ~ bs(TotalVolunteers, degree=8,knots=br))

preds=predict(model_cut,list(TotalVolunteers=volunt.grid),se=T)

plot(TotalVolunteers, log_item, xlim=range(volunt.grid) ,cex =.5, col =" darkgrey " )
lines(volunt.grid, preds$fit, lwd =2, col =" blue")
#not so good


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



# Prova Michi -------------------------------------------------------------

# poly model
lm_train = lm.funs(intercept = T)$train.fun
lm_predict = lm.funs(intercept = T)$predict.fun
library(fastDummies)

prova = dummy_cols(train_data, select_columns = c('EventType', 'Season'))
design_matrix = matrix(poly(x, degree=8), ncol=8)
design_matrix = cbind(design_matrix, prova[ ,c(10, 12:14, 16:18)])
design_matrix = as.matrix(design_matrix)
prova = dummy_cols(test_data, select_columns = c('EventType', 'Season'))
pred_grid = matrix(poly(test_data$TotalVolunteers, degree=8, coefs = attr(poly(x, degree=8), "coefs")), ncol=8)
pred_grid = cbind(pred_grid, prova[ ,c(10, 12:14, 16:18)])
pred_grid = as.matrix(pred_grid)

c_preds = conformal.pred(design_matrix, y, pred_grid, alpha=0.05, train.fun = lm_train,
                         predict.fun = lm_predict, num.grid.pts = 200)

plot(test_data$TotalVolunteers, test_data$log_item, ylim=range(c(c_preds$up, c_preds$lo)))
points(test_data$TotalVolunteers, c_preds$pred, col ="red", ylim=range(c(c_preds$up, c_preds$lo)))
points(test_data$TotalVolunteers, c_preds$up, col=" blue")
points(test_data$TotalVolunteers, c_preds$lo, col=" blue")

library(plotrix)

plot(test_data$TotalVolunteers, test_data$log_item, col='red', pch=20, cex=0.5, 
     ylim=range(c(c_preds$up, c_preds$lo)))
plotCI(test_data$TotalVolunteers, c_preds$pred, ui=c_preds$up, li=c_preds$lo, add=TRUE, pch=16)






