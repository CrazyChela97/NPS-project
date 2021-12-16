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

data = CleanUsa[ , c(5,6,7,8,9,11,13,14,19,20)]

plot(data$TotalVolunteers, data$log_Items)

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


# Analysis for Categorical Regressors -------------------------------------

# EVENT TYPE
cat = levels(factor(data$EventType))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$EventType == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_Items, main=cat[k])
}


# YEAR
cat = levels(factor(data$Year))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Year == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_Items, main=cat[k])
}


# SEASON
cat = levels(factor(data$Season))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Season == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_Items, main=cat[k])
}


# MONTH
cat = levels(factor(data$Month))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$Month == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_Items, main=cat[k])
}


# WEEKEND
cat = levels(factor(data$weekend))
c = length(cat)

par(mfrow=c(2,c/2))
for (k in 1:c){
  dati = data[which(data$weekend == cat[k]), ]
  plot(dati$TotalVolunteers, dati$log_Items, main=cat[k])
}

plot(data$TotalVolunteers, data$TotalItems/data$TotalVolunteers)

# Analisi spaziale ?? -----------------------------------------------------
dati = data[which(data$Year==2018), ]
Q = cbind(lat=data$Latitude1, long=data$Longitude1, productivity=data$TotalItems/100)
head(Q)

plot3d(Q, size=3, col='darkorange', aspect = F)

d <- dist(Q)

par(mfrow=c(2,2))



clustc <- hclust(d, method='complete')
plot(clustc, hang=-0.1, labels=FALSE, main='complete', xlab='', sub='')
# rect.hclust(clustc, k=2)
# rect.hclust(clustc, k=3)

clustw <- hclust(d, method='ward.D2')
plot(clustw, hang=-0.1, labels=FALSE, main='ward', xlab='', sub='')
# rect.hclust(clustw, k=2)
# rect.hclust(clustw, k=3)



# complete linkage -> divide più in "fasce"
clusterc <- cutree(clustc, 10)
plot3d(Q, size=3, col=clusterc, aspect = F) 

# ward linkage -> divide più in zone
clusterw <- cutree(clustw, 10)
plot3d(Q, size=3, col=clusterw, aspect = F) 


dev.off()
plot(data$TotalVolunteers, data$log_Items, col = clusterc, pch=16, cex=0.5)
plot(data$TotalVolunteers, data$log_Items, col = clusterw, pch=16, cex=0.5)
# possibile offset sulle zone : clusterizzazione sulle zone
# meglio w


# provo
labels = clusterc/5
x <- data$TotalVolunteers
y <- data$log_Items
knots = quantile(x, probs=seq(0.25, 0.95, by=0.1))
boundary_knots <- quantile(x, probs=c(0.01, 0.99))

model_ns = lm(y ~ ns(x, knots=knots, Boundary.knots=boundary_knots) 
              + as.factor(EventType) + weekend + Season, offset = labels, data=data)
summary(model_ns)

preds = predict(model_ns, list(x=x, EventType=data$EventType, 
                               weekend=data$weekend, Season=data$Season, labels=labels), se=T)
se.bands = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)

plot(x, y, xlim=range(x), cex =.5, col="darkgrey")
points(x, se.bands[,1], cex=.6, col ="blue")
points(x, se.bands[,2], cex=.6, col ="blue")
points(x, preds$fit, cex=.6, col ="blue")
# visualize knots
knots_pred = predict(model_ns, list(x=knots))
points(knots, knots_pred, col='blue', pch=19)
boundary_pred = predict(model_ns, list(x=boundary_knots))
points(boundary_knots, boundary_pred, col='red', pch=19)




