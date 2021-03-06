################################################
###           OUTLIERS DETECTION             ###
################################################


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
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)
library(tibble)


# Import Dataset ----------------------------------------------------------

cleandata = import("cleandata.Rdata")
# View(cleandata)
USA = cleandata[which(cleandata$Country=='USA'), ]


# Data Cleaning -----------------------------------------------------------

# Deleting NA
USA = USA [- which(is.na(USA$Area) | is.na(USA$TotalVolunteers) | is.na(USA$TotalItems)), ]
# UNLIKELY OBS
# Deleting missions in which volunteers collected LESS than 5 items each
items_per_volunteer = USA$TotalItems/USA$TotalVolunteers
USA = USA[items_per_volunteer > 5, ]    # 4k obs in meno

# General Overview
numerical_usa = USA[ ,c(8,14,19)] # Items, Volontari, Area 
pairs(numerical_usa)
# Deleting visible outliers
numerical_usa = numerical_usa[which(numerical_usa$Area!=max(numerical_usa$Area)), ]
pairs(numerical_usa)

USA = USA[which(USA$Area!=max(USA$Area)), ]


# DEPTH ANALYSIS : Items vs Volunteers --------------------------------------
biv_data = USA[ , c(8,14)]

# Bagplot
BP <- bagplot(biv_data, cex=0.5, main='Bagplot')
outlying_obs <- BP$pxy.outlier
ind_outliers <- which(apply(biv_data, 1, function(x) all(x %in% outlying_obs)))
clean_data <- biv_data[-ind_outliers, ]    

bagplot(clean_data, show.whiskers = F, cex=0.6, main='Bagplot', col ) # very better

# DD plot
ddPlot(x = clean_data, y = outlying_obs, depth_params = list(method='Tukey'), scale = T)

# Clean data overview
par(mfrow=c(1,2))
plot(clean_data$TotalVolunteers, clean_data$TotalItems, pch=21, col='black', bg='orange',
     cex=0.8, main='Data without Outliers', xlab='Total Volunteers', ylab='Collected Items')
plot(clean_data$TotalVolunteers, log(clean_data$TotalItems), pch=21, col='black', bg='turquoise', 
     cex=0.8, main='Transformed Data', xlab='Total Volunteers', ylab='log( Items )')

CleanUsa = USA[-ind_outliers, ] #from 22702 to 13354 


# DEPTH ANALYSIS : Items vs Area ------------------------------------------
biv_data = CleanUsa[ , c(19,14)]

# Bagplot
BP <- bagplot(biv_data, show.whiskers = F)
outlying_obs <- BP$pxy.outlier
ind_outliers <- which(apply(biv_data, 1, function(x) all(x %in% outlying_obs)))
clean_data <- biv_data[-ind_outliers, ]

bagplot(clean_data, show.whiskers = F, cex=0.6, main='Bagplot') # very better

# DD plot
ddPlot(x = clean_data, y = outlying_obs, depth_params = list(method='Tukey'))

# Clean data overview
plot(biv_data$Area, biv_data$TotalItems)

CleanUsa = CleanUsa[-ind_outliers, ] #from 13354 to 13184

log_Items=log(CleanUsa$TotalItems)
CleanUsa=add_column(CleanUsa, log_Items, .after = "TotalItems") 

# SAVING NEW DATASET
save(CleanUsa,file="cleanUSA.Rdata")


