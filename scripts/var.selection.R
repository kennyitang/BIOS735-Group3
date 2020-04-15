# variable selection 

library(MASS)

setwd("D:/Spring 2020/Bios735/BIOS735-Group3")
# training
trn_data <-  fread("./Data/NC_Accidents_trn.csv")
# testing
tst_data <-  fread("./Data/NC_Accidents_tst.csv")

trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

# look at correlations 
predictors <-  c("Source","Side","Temperature(F)","Humidity(%)","Pressure(in)","Visibility(mi)","Wind_Direction","Wind_Speed(mph)","Crossing","Traffic_Signal","Sunrise_Sunset") 
x <- trn_data[, predictors, with = F]
tokeep <- which(sapply(x,is.numeric))
x_numeric <- x[ , tokeep, with=FALSE]

require("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(x_numeric)

# fit the proportional odds model 
mod <- polr(Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
  `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
  Sunrise_Sunset + weekday + interstate, data = trn_data) 
summary(mod)

# get p-values 


step.model <- stepAIC(mod, direction = "both", trace = F)
summary(step.model)

# need to test for proportional odds assumption 