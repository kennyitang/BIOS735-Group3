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
# install.packages("ordinalForest")
library(ordinalForest)

# load our package 
devtools::load_all("./group3project")

#' ---
#' title: "BIOS 735 Final Project: Car Accident Severity - Module 3 method: Random Forest Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---

############# Read in Data from package
data("trn_data")
data("tst_data")

############# Create factor Variable 
trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

#Variable names
names(trn_data)


#5-fold cross-validation
cv_5 = trainControl(method = "cv", number = 5)
#Tuning parameter
rf_grid = expand.grid(mtry = 1:3)

########################################################################################
###################### Random forest on training data ##################################
########################################################################################


table(trn.data$Severity.c)

set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
               `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
               Sunrise_Sunset + weekday + interstate, 
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

# Confusion Matrix
confusionMatrix(predict(rf_mod_red, newdata = tst_data), tst_data$Severity_c)


########################################################################################
###################### Ordinal forest on training data #################################
########################################################################################

# prepare data for package 
datatrain = trn_data%>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                 Sunrise_Sunset, weekday, interstate, Severity_c)
datatest = tst_data %>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                Sunrise_Sunset, weekday, interstate, Severity_c)
# train forest 
set.seed(13847)
start = Sys.time()
ordforest <- ordfor(depvar = "Severity_c", data = datatrain)
end = Sys.time()
print(end - start)

# variable importance 
sort(ordforest$varimp, decreasing = TRUE)

# predict 
preds <- predict(of_full, newdata = datatest)

# confusion matrix
confusionMatrix(preds$ypred, datatest$Severity_c)
postResample(preds$ypred, datatest$Severity_c)



########################################################################################
###################### Random forest on down sampled training data ####################
########################################################################################

# create downsampled dataset 
set.seed(12984)
downsample.trn <- trn_data[c(which(trn_data$Severity_c == 3),
                             sample(which(trn_data$Severity_c == 1), 1081), 
                             sample(which(trn_data$Severity_c == 2), 1081)),]

# train model 
set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
    Sunrise_Sunset + weekday + interstate, 
  data = downsample.trn, 
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

# Confusion Matrix
confusionMatrix(predict(rf_mod_red, newdata = tst_data), tst_data$Severity_c)


########################################################################################
###################### Ordinal forest on down sampled training data ####################
########################################################################################

datatrain = downsample.trn %>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                      `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                      Sunrise_Sunset, weekday, interstate, Severity_c)

# run forest 
set.seed(13847)
start = Sys.time()
ordforest <- ordinalForest::ordfor(depvar = "Severity_c", data = datatrain)
end = Sys.time()
print(end - start)

# variable importance
sort(ordforest$varimp, decreasing = TRUE)

# predict 
preds <- predict(ordforest, newdata = datatest)

# confusion matrix 
confusionMatrix(preds$ypred, datatest$Severity_c)
postResample(preds$ypred, datatest$Severity_c)


########################################################################################
###################### Random forest on Map Quest data #################################
########################################################################################

# Create MapQuest Training Data 
set.seed(12984)
mq.trn <- trn_data %>% dplyr::filter(Source == "MapQuest" & !(Severity_c == "3"))
mq.test <- tst_data %>% dplyr::filter(Source == "MapQuest" & !(Severity_c == "3"))
mq.test$Severity_c <- droplevels(mq.test$Severity_c)
mq.trn$Severity_c <- droplevels(mq.trn$Severity_c)

# Run Forest 
set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
    Sunrise_Sunset + weekday + interstate, 
  data = mq.trn, 
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

# Confusion Matrix
confusionMatrix(predict(rf_mod_red, newdata = mq.test), mq.test$Severity_c)

########################################################################################
###################### Random forest on Bing data ######################################
########################################################################################

# Create Bing Training Data 
set.seed(12984)
bing.trn <- trn_data[c(which(trn_data$Source == "Bing")), ]
bing.test <- tst_data %>% dplyr::filter(Source == "Bing")

# Run Forest
set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
    Sunrise_Sunset + weekday + interstate, 
  data = bing.trn, 
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

# Confusion Matrix
confusionMatrix(predict(rf_mod_red, newdata = bing.test), bing.test$Severity_c)


########################################################################################
###################### Random forest on training data without Source ##################
########################################################################################

# Train Forest 
set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
    Sunrise_Sunset + weekday + interstate, 
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

# Confusion Matrix
confusionMatrix(predict(rf_mod_red, newdata = tst_data), tst_data$Severity_c)










