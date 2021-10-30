rm(list=ls())

library(maps)
library(maptools)
library(rgdal)
library(sp)

library(shp2graph)
library(lubridate)

setwd("~/Documents/Politecnico/Magistrale/Non Parametric Statistics/PROJECT")
shape_file <- readOGR("EC2020_All_Cleanup_Events_2015_2018.shp", GDAL1_integer64_policy = TRUE)


shape2 =readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp")
plot(shape2)
points(shape_file, pch=20)


##sf pacchetto per shapefiles
