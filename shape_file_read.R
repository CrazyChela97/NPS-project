rm(list=ls())

library(maps)
library(maptools)
library(rgdal)
library(sp)

library(shp2graph)
library(lubridate)

setwd("~/Documents/GitHub/NPS-project/TM_WORLD_BORDERS_SIMPL-0")
shape_file <- readOGR("EC2020_All_Cleanup_Events_2015_2018.shp", GDAL1_integer64_policy = TRUE)


shape2 =readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp")
summary(shape2)
plot(shape2)
Nord_America = shape2[which(shape2@data$NAME=='Georgia'), ]
points(shape_file, pch=20)
plot(Nord_America)

##sf pacchetto per shapefiles
