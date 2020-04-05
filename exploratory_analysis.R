
# all packages
library(readr)
library(knitr)
library(kableExtra)
library(caret)
library(MASS)
library(ISLR)
library(glmnet)
library(e1071)
library(broom)
library(data.table)

#' ---
#' title: "BIOS 735 Final Project: Car Accident Severity - Exploratory Analysis Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---



data = read.csv("NC_Accidents.csv")
data = data.table(data)
str(data)

#Use variable Weather_Timestamp to determine year of data
  #Parsing character to Datetime variable
data$Weather_Timestamp_n = as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%OS")
data$year = lubridate::year(data$Weather_Timestamp_n)   #Year of observation recorded

summary(data$year)  #there are observations missing all weather info

#Exclude data with missing weather info
variable.names(data.complete)
data.complete = data[!is.na(Weather_Timestamp_n),]  #Remove obs. missing all weather variables (if without Timestamp)


#"Temperature.F." "Wind_Chill.F." "Humidity..." "Pressure.in." "Visibility.mi." "Wind_Direction" "Wind_Speed.mph." 
#"Precipitation.in." "Weather_Condition"

#Check proportion of missingness for each weather variable
nrow(data.complete[is.na(Temperature.F.), ])    / nrow(data.complete)
nrow(data.complete[is.na(Wind_Chill.F.), ])     / nrow(data.complete)
nrow(data.complete[is.na(Humidity...), ])       / nrow(data.complete)
nrow(data.complete[is.na(Pressure.in.), ])      / nrow(data.complete)
nrow(data.complete[is.na(Visibility.mi.), ])    / nrow(data.complete)
nrow(data.complete[is.na(Wind_Direction), ])    / nrow(data.complete)
nrow(data.complete[is.na(Wind_Speed.mph.), ])   / nrow(data.complete)
nrow(data.complete[is.na(Precipitation.in.), ]) / nrow(data.complete)
nrow(data.complete[is.na(Weather_Condition), ]) / nrow(data.complete)


#Wind Chill and Precipitation have around 60% missingness

  #What should we do with them?



data.complete$Severity_c = as.factor(data.complete$Severity)  #make severity factor variable



#Use data from Years before 2019 as traning data set, and Data from 2019 as testing data set

train.data = data.complete[year < 2019, ]
test.data = data.complete[year == 2019, ]
nrow(data.complete) == nrow(train.data) + nrow(test.data)   #Checking number of observations



#Perform Exploratory Data Analysis on Training data set

  #Response (Severity) vs. Numerical variables

featurePlot(x = train.data[, .(Temperature.F., Wind_Chill.F., Humidity..., Pressure.in.,
                               Visibility.mi., Wind_Speed.mph.)], 
            y = train.data[, Severity_c],
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")), 
            plot = "density", 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2, 2), 
            auto.key = list(columns = 4))



