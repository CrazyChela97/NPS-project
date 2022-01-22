#################################################
###       PLASTIC  PREDICTION  MODEL          ###
#################################################

CleanUsa = import("CleanUsa.Rdata")

plastic.perc = CleanUsa$`%Plastic&Foam`/100
plastic.items = round(CleanUsa$TotalClassifiedItems_EC2020 * plastic.perc)

# considerando la percentuale
plot(CleanUsa$TotalVolunteers, plastic.perc) # no sense
plot(log(CleanUsa$TotalItems), plastic.perc) # no sense

# considerando numero items plastica
plot(CleanUsa$TotalVolunteers, log(plastic.items))
# plastic vs volunteers : same shape as total.items vs volunteers
plot(log(CleanUsa$TotalItems), log(plastic.items))
# plastic vs total.items : nearly linear relation as expected


data = import("RegData.Rdata")
data$plastic_items = plastic.items

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

