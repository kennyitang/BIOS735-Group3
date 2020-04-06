
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



data = read.csv("./Data/NC_Accidents.csv")
data = data.table(data)
str(data)

#Use variable Weather_Timestamp to determine year of data
#Parsing character to Datetime variable
data$Weather_Timestamp_n = as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%OS")
data$year = lubridate::year(data$Weather_Timestamp_n)   #Year of observation recorded

summary(data$year)  #there are observations missing all weather info

#Exclude data with missing weather info
data.complete = data[!is.na(Weather_Timestamp_n),]  #Remove obs. missing all weather variables (if without Timestamp)
variable.names(data.complete)

# Weather variables:
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

#What should we do with them? Drop them: 
#Windchill is the function of temperature and wind speed
#Not much variability in Precipitation
hist(data.complete[!is.na(Wind_Chill.F.), Wind_Chill.F.], breaks = 50)
var(data.complete[!is.na(Wind_Chill.F.), Wind_Chill.F.])

hist(data.complete[!is.na(Precipitation.in.), Precipitation.in.], breaks = 50)
var(data.complete[!is.na(Precipitation.in.), Precipitation.in.])

#Remove Wind Chill and Precipitation
data.complete[, c("Wind_Chill.F.", "Precipitation.in.") := NULL]
str(data.complete)


#Checking variability in other variables:
#Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal

round(colMeans(data.complete[, .(Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, 
                                 Railway, Roundabout, Station, Stop, Traffic_Calming, 
                                 Traffic_Signal)]) * 100, 2)
summary(data.complete[, Side])

#Drop all TRUE/FALSE variables (Not much variability) except Crossing and Traffic_Signal
data.complete[, c("Amenity", "Bump",	"Give_Way",	"Junction",	"No_Exit", "Railway", 
                  "Roundabout", "Station", "Stop", "Traffic_Signal") := NULL]
str(data.complete)


#Checking missingness in the rest of the variables
colSums(apply(data.complete, MARGIN = 2, is.na))


#Drop TMC (not useful info), End_Lat (119596 missing), End_Lng (119596 missing) and Number (not useful info)
data.complete[, c("TMC", "End_Lat", "End_Lng", "Number") := NULL]

#Drop Distance.mi cuz not much variability
summary(data.complete$Distance.mi.)
sd(data.complete$Distance.mi.)
hist(data.complete$Distance.mi., breaks = 50)

data.complete[, Distance.mi. := NULL]

data.complete = data.complete[rowSums(is.na(data.complete)) == 0, ]

colSums(apply(teset, MARGIN = 2, is.na))  #No Missingness, Data with complete cases


data.complete$Severity_c = as.factor(data.complete$Severity)  #make severity factor variable


str(data.complete)
#Use data from Years before 2019 as traning data set, and Data from 2019 as testing data set

train.data = data.complete[year < 2019, ]
test.data = data.complete[year == 2019, ]
nrow(data.complete) == nrow(train.data) + nrow(test.data)   #Checking number of observations


#Write to CSV file
write.csv(data.complete, "./Data/NC_Accidents_filtered.csv")
write.csv(train.data, "./Data/NC_Accidents_trn.csv")
write.csv(test.data, "./Data/NC_Accidents_tst.csv")


#Perform Exploratory Data Analysis on Training data set

#Response (Severity) vs. Numerical variables

# featurePlot(x = train.data[, .(Temperature.F., Wind_Chill.F., Humidity..., Pressure.in.,
#                                Visibility.mi., Wind_Speed.mph.)], 
#             y = train.data[, Severity_c],
#             scales = list(x = list(relation = "free"), 
#                           y = list(relation = "free")), 
#             plot = "density", 
#             adjust = 1.5, 
#             pch = "|", 
#             layout = c(2, 2), 
#             auto.key = list(columns = 4))



