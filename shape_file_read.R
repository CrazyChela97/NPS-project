rm(list=ls())

library(maps)
library(maptools)
library(rgdal)
library(sp)
library(rio)

library(shp2graph)
library(lubridate)

# shape plot usa+canada
setwd("~/Downloads/USA_States")
usa = readOGR("USA_States.shp")

setwd("~/Downloads/Canada_Provinces")
canada = readOGR("Canada_Provinces.shp")

plot(usa)
plot(canada, add=TRUE)


# provo con coordinate dati

current_path=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cleandata=import("cleandata.Rdata")
cleandata=cleandata[which(cleandata$Year!="2015"),] 
sort(table(cleandata$Country))

data = cleandata[which(cleandata$Country=='USA'), ]
lat_long_point = cbind(data$Longitude1, data$Latitude1)
x_y = lat_long_point
x_y = data.frame(lat=x_y[,1], long=x_y[,2])
coordinates(x_y) <- ~lat+long
x_y@proj4string <- usa@proj4string

quartz()
plot(usa)
plot(canada, add=TRUE)
points(x_y, col = "deeppink", cex = 0.35, pch=20)
