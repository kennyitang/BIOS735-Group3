# all packages
library(readr)
library(knitr)
library(kableExtra)
library(tidyverse)
library(caret)
library(MASS)
library(ISLR)
library(glmnet)
library(e1071)
library(broom)
library(data.table)
library(randomForest)

#' ---
#' title: "BIOS 735 Final Project: Car Accident Severity - Module 3 method: Random Forest Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---


trn_data = fread("./Data/NC_Accidents_trn.csv")
tst_data = fread("./Data/NC_Accidents_tst.csv")

trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)



#Helper function for calculating accuracy
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}


#Variable names
names(trn_data)


#5-fold cross-validation
cv_5 = trainControl(method = "cv", number = 5)
#Tuning parameter
rf_grid = expand.grid(mtry = 1:3)


#Random forest

set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
               `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
               Sunrise_Sunset, 
  data = trn_data, 
  method = "rf",
  trControl = cv_5,
  tuneGrid = rf_grid
)
end = Sys.time()
print(end - start)

rf_mod_red$results
rf_mod_red$bestTune
varImp(rf_mod_red)
varImpPlot(rf_mod_red$finalModel)
  #Obtain test accuracy
calc_acc(actual = tst_data$Severity_c, predicted = predict(rf_mod_red, newdata = tst_data))



