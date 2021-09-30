rm(list=ls())

library(maps)
library(maptools)
library(rgdal)
library(sp)

library(shp2graph)
library(lubridate)

setwd("/Users/michelafrigeri/Downloads/plastic_pollution_2")
shape_file <- readOGR("EC2020_All_Cleanup_Events_2015_2018.shp", GDAL1_integer64_policy = TRUE)
plot(shape_file,)
