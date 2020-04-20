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

table(trn.data$Severity.c)

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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = tst_data), tst_data$Severity_c)



########################################## RF for time_diff 
# performs very poorly 

trn_data$time_diff_c = as.factor(trn_data$time_diff)
tst_data$time_diff_c = as.factor(tst_data$time_diff)

#Variable names
names(trn_data)


#5-fold cross-validation
cv_5 = trainControl(method = "cv", number = 5)
#Tuning parameter
rf_grid = expand.grid(mtry = 1:3)


#Random forest

set.seed(3)
start = Sys.time()
rf_mod_red_timediff = train(
  time_diff_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
    Sunrise_Sunset + weekday + interstate, 
  data = trn_data, 
  method = "rf",
  trControl = cv_5,
  tuneGrid = rf_grid
)
end = Sys.time()
print(end - start)

rf_mod_red_timediff$results
rf_mod_red_timediff$bestTune
varImp(rf_mod_red_timediff)

#Obtain test accuracy
calc_acc(actual = tst_data$time_diff_c, 
         predicted = predict(rf_mod_red_timediff, newdata = tst_data))

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red_timediff, newdata = tst_data), tst_data$time_diff_c)

# ggplot(trn_data, aes(time_diff)) + 
#   geom_histogram(binwidth = 1) + xlim(0,100) 
# 
# ggplot(tst_data, aes(time_diff)) + 
#   geom_histogram(binwidth = 1) + xlim(0,100)



######################## ordinalForest

# install.packages("ordinalForest")
library(ordinalForest)

trn_data_bing <- trn_data %>% dplyr::filter(Source == "Bing")
tst_data_bing <- tst_data %>% dplyr::filter(Source == "Bing")

#trn_data_bing$Severity_c <- trn_data_bing$Severity_c %>% ordered()
#tst_data_bing$Severity_c <- tst_data_bing$Severity_c %>% ordered()
datatrain = trn_data_bing %>% dplyr::select(Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                 Sunrise_Sunset, weekday, interstate, Severity_c)
datatest = tst_data %>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                Sunrise_Sunset, weekday, interstate, Severity_c)

set.seed(13847)
start = Sys.time()
classweights_test = c(1,10,10)
ordforest <- ordfor(depvar = "Severity_c", data = datatrain, 
                                   perffunction = "custom", classweights = classweights_test)
sort(ordforest$varimp, decreasing = TRUE)
end = Sys.time()
print(end - start)
preds <- predict(of_full, newdata = datatest)
confusionMatrix(preds$ypred, datatest$Severity_c)
postResample(preds$ypred, datatest$Severity_c)
# save(ordforest, file = "ordforest.RData")



############ Random forest Downsampling

set.seed(12984)
downsample.trn <- trn_data[c(which(trn_data$Severity_c == 3),
                             sample(which(trn_data$Severity_c == 1), 1081), 
                             sample(which(trn_data$Severity_c == 2), 1081)),]

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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = tst_data), tst_data$Severity_c)


############ Ordinal forest Downsampling

downsample.trn$Severity_c <- downsample.trn$Severity_c %>% as.factor()
tst_data$Severity_c <- tst_data$Severity_c %>% as.factor()
datatrain = downsample.trn %>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                Sunrise_Sunset, weekday, interstate, Severity_c)
datatest = tst_data%>% dplyr::select(Source, Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                               `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                               Sunrise_Sunset, weekday, interstate, Severity_c)

set.seed(13847)
start = Sys.time()
ordforest <- ordinalForest::ordfor(depvar = "Severity_c", data = datatrain)
sort(ordforest$varimp, decreasing = TRUE)
end = Sys.time()
print(end - start)
preds <- predict(ordforest, newdata = datatest)
confusionMatrix(preds$ypred, datatest$Severity_c)
postResample(preds$ypred, datatest$Severity_c)


############################### RF Map Quest



set.seed(12984)
mq.trn <- trn_data %>% dplyr::filter(Source == "MapQuest" & !(Severity_c == "3"))
mq.test <- tst_data %>% dplyr::filter(Source == "MapQuest" & !(Severity_c == "3"))
mq.test$Severity_c <- droplevels(mq.test$Severity_c)
mq.trn$Severity_c <- droplevels(mq.trn$Severity_c)

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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = mq.test), mq.test$Severity_c)


############ Random forest Downsampling

set.seed(12984)
downsample.trn <- trn_data[c(which(trn_data$Severity_c == 2 & trn_data$Source == "MapQuest"),
                             sample(which(trn_data$Severity_c == 1 & trn_data$Source == "MapQuest"), 
                                    8543))]
downsample.test <- tst_data %>% dplyr::filter(Source == "MapQuest" & !(Severity_c == "3"))
downsample.test$Severity_c <- droplevels(downsample.test$Severity_c)
downsample.trn$Severity_c <- droplevels(downsample.trn$Severity_c)

set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = downsample.test), downsample.test$Severity_c)

############################### RF Bing


set.seed(12984)
bing.trn <- trn_data[c(which(trn_data$Source == "Bing")), ]
bing.test <- tst_data %>% dplyr::filter(Source == "Bing")
bing.test$Severity_c <- droplevels(downsample.test$Severity_c)
bing.trn$Severity_c <- droplevels(downsample.trn$Severity_c)

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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = bing.test), bing.test$Severity_c)

############ Random forest Downsampling

set.seed(12984)
downsample.trn <- trn_data[c(which(trn_data$Severity_c == 2 & trn_data$Source == "Bing"),
                             sample(which(trn_data$Severity_c == 1 & trn_data$Source == "Bing"), 
                                    965), 
                             sample(which(trn_data$Severity_c == 3 & trn_data$Source == "Bing"), 
                                    965))]
downsample.test <- tst_data %>% dplyr::filter(Source == "Bing")
downsample.test$Severity_c <- droplevels(downsample.test$Severity_c)
downsample.trn$Severity_c <- droplevels(downsample.trn$Severity_c)

set.seed(3)
start = Sys.time()
rf_mod_red = train(
  Severity_c ~ Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
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

# Confusion Matrix, has a lot of metrics including Kappa
confusionMatrix(predict(rf_mod_red, newdata = downsample.test), downsample.test$Severity_c)





#########################

datatrain = downsample.trn %>% dplyr::select(Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                             `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                             Sunrise_Sunset, weekday, interstate, Severity_c)
datatest = downsample.test %>% dplyr::select(Side, `Temperature(F)`, `Humidity(%)`, `Pressure(in)`,
                                     `Visibility(mi)`, `Wind_Speed(mph)`, Crossing, Traffic_Signal,
                                     Sunrise_Sunset, weekday, interstate, Severity_c)

set.seed(13847)
start = Sys.time()
ordforest <- ordinalForest::ordfor(depvar = "Severity_c", data = datatrain)
# sort(ordforest$varimp, decreasing = TRUE)
end = Sys.time()
print(end - start)
preds <- predict(ordforest, newdata = datatest)
confusionMatrix(preds$ypred, datatest$Severity_c)
postResample(preds$ypred, datatest$Severity_c)










