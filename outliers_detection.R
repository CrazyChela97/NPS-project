
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


# Import Dataset ----------------------------------------------------------

cleandata = import("cleandata.Rdata")
# View(cleandata)

#cleandata=cleandata[which(cleandata$Year!="2015"),] #dataset finale 38953 osservazioni, 2015 ha solo  1411 osservazioni
dim(cleandata[which(cleandata$Year=="2016"),])[1] #9273 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2017"),])[1] #12296 osservazioni nel 2016
dim(cleandata[which(cleandata$Year=="2018"),])[1] #17384 osservazioni nel 2016

USA = cleandata[which(cleandata$Country=='USA'),]


# Analisi Michi -----------------------------------------------------------

# levo NA
USA = USA [- which(is.na(USA$Area) | is.na(USA$TotalVolunteers) | is.na(USA$TotalItems)), ]
# levo quelli strani : se meno di 5 items a testa elimino
items_per_volunteer = USA$TotalItems/USA$TotalVolunteers
numerical_usa = USA[items_per_volunteer > 5, ]    # 4k obs in meno

# general overview
numerical_usa = USA[ ,c(8,14,19)] # Items, Volontari, Area 
pairs(numerical_usa)
# molti outliers!  ->  tolgo i più palesi
numerical_usa = numerical_usa[which(numerical_usa$Area!=max(numerical_usa$Area)), ]
pairs(numerical_usa)

numerical_usa = USA[ ,c(8,14,19)] # Items, Volontari, Area 

# DEPTH ANALYSIS

# Items vs Volunteers
biv_data = USA[ , c(8,14)]

depthContour(
  clean_data,
  depth_params = list(method = 'Tukey'),
  points = TRUE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 20,
  pdmedian = F,     # segna la mediana
  graph_params = list(cex=.01, pch=1),
  pmean = F     # segna la media
)

# Bagplot
BP <- bagplot(biv_data)
outlying_obs <- BP$pxy.outlier
ind_outliers <- which(apply(biv_data, 1, function(x) all(x %in% outlying_obs)))
clean_data <- biv_data[-ind_outliers, ]     # rimangono tipo metà delle osservazioni : è ok?

bagplot(clean_data, show.whiskers = F, cex=0.8, main='Bagplot') # molto meglio

# Clean data overview
plot(clean_data$TotalVolunteers, clean_data$TotalItems)
plot(clean_data$TotalVolunteers, log(clean_data$TotalItems))

CleanUsa = USA[-ind_outliers, ]

# Items vs Area
biv_data = CleanUsa[ , c(19,14)]

depthContour(
  biv_data,
  depth_params = list(method = 'Tukey'),
  points = TRUE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 50,
  pdmedian = F,     # segna la mediana
  graph_params = list(cex=.01, pch=1),
  pmean = F     # segna la media
)

# Bagplot
BP <- bagplot(biv_data)
outlying_obs <- BP$pxy.outlier
ind_outliers <- which(apply(biv_data, 1, function(x) all(x %in% outlying_obs)))
clean_data <- biv_data[-ind_outliers, ]     # rimangono tipo metà delle osservazioni : è ok?

bagplot(clean_data, show.whiskers = F, cex=0.8, main='Bagplot') # molto meglio

# data overview
plot(clean_data$Area, clean_data$TotalItems)





# Graphical Analysis ------------------------------------------------------

#ratio plot
ratio=rep(0,length(USA$TotalItems))
for (i in 1:length(USA$TotalItems)) {
  ratio[i]=USA$TotalItems[i]/USA$TotalVolunteers[i]
}
ratio=as.data.frame(ratio)
USA=cbind(USA,ratio)


plot(USA$Area,USA$ratio)

u=USA[,c(19,20)]

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
cleanUSA <- u[-ind_outlying_obs,] #-4639 outliers TOP

plot(cleanUSA)

#plot Items vs Volunteers

u=USA[,c(8,14)]
plot(u)

bagplot(u)
outlying_obs <- bagplot(u)$pxy.outlier

ind_outlying_obs <- which(apply(u,1,function(x) all(x %in% outlying_obs)))
cleanUSA <- u[-ind_outlying_obs,] #-9235 outliers, abbiamo comunque 15953 dati ma possiamo togliere 9k dati? 
#da chiedere e capire come risolvere

par(mfrow=c(1,2))
plot(u)
plot(cleanUSA)

CleanUsa = USA[-ind_outlying_obs,]
save(CleanUsa,file="cleanUSA.Rdata")

