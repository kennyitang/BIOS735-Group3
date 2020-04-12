
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
#' title: "BIOS 735 Final Project: Car Accident Severity - Data Preparation Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---



data = fread("./Data/NC_Accidents.csv")
str(data)

#Use variable Weather_Timestamp to determine year of data
#Parsing character to Datetime variable
data$Weather_Timestamp_n = as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%OS")
data$year = lubridate::year(data$Weather_Timestamp_n)   #Year of observation recorded

summary(data$year)  #there are observations missing all weather info (missing Weather Timestamp)

#Exclude data with missing weather info (missing weather Timestamp)
data_filter = data[!is.na(Weather_Timestamp_n),]  #Remove obs. missing all weather variables (if without Timestamp)

names(data_filter)

# Weather variables:
#"Temperature.F." "Wind_Chill.F." "Humidity..." "Pressure.in." "Visibility.mi." "Wind_Direction" "Wind_Speed.mph." 
#"Precipitation.in." "Weather_Condition"

#Check proportion of missingness for each weather variable
colSums(apply(data_filter[, .(`Temperature(F)`, `Wind_Chill(F)`, `Humidity(%)`,
                              `Pressure(in)`, `Visibility(mi)`, Wind_Direction,
                              `Wind_Speed(mph)`, `Precipitation(in)`, Weather_Condition)], 
              MARGIN = 2, is.na)) / 
  nrow(data_filter)


#Wind Chill and Precipitation have around 60% missingness

  #What should we do with them? Drop them: 
  #   Windchill is the function of temperature and wind speed
  #   Not much variability in Precipitation
hist(data_filter[!is.na(`Wind_Chill(F)`), `Wind_Chill(F)`], breaks = 50)
var(data_filter[!is.na(`Wind_Chill(F)`), `Wind_Chill(F)`])

hist(data_filter[!is.na(`Precipitation(in)`), `Precipitation(in)`], breaks = 50)
var(data_filter[!is.na(`Precipitation(in)`), `Precipitation(in)`])

#Remove Wind Chill and Precipitation
data_filter[, c("Wind_Chill(F)", "Precipitation(in)") := NULL]
dim(data_filter)


#Weather_Condition: too many categories  How do we want to recategorize them? ignore for now
table(data_filter$Weather_Condition) %>% as.matrix() / nrow(data_filter) * 100
#Wind_Direction: too many categories  How do we want to recategorize them? ignore for now
table(data_filter$Wind_Direction) %>% as.matrix() / nrow(data_filter) * 100


#Checking variability in binary variables:
#Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, Railway, Roundabout, Station, Stop, 
#Traffic_Calming, Traffic_Signal, Turning_Loop

round(colMeans(data_filter[, .(Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, 
                                 Railway, Roundabout, Station, Stop, Traffic_Calming, 
                                 Traffic_Signal)]) * 100, 2)
summary(as.factor(data_filter[, Side]))

#Drop all TRUE/FALSE variables (Not much variability) except Crossing and Traffic_Signal
data_filter[, c("Amenity", "Bump",	"Give_Way",	"Junction",	"No_Exit", "Railway", 
                "Roundabout", "Station", "Stop", "Traffic_Calming", "Turning_Loop") := NULL]
dim(data_filter)


#Checking missingness in the rest of the variables
colSums(apply(data_filter, MARGIN = 2, is.na))


#Drop End_Lat (119596 missing), End_Lng (119596 missing) and
#TMC and Number (not useful infos) and
#State, Country, and Timezone (No variability)  and
#Civil_Twilight, Nautical_Twilight and Astronomical_Twilight (Duplicate info with Sunrise_Sunset)
data_filter[, c("End_Lat", "End_Lng", 
                "TMC", "Number", "State", "Country", "Timezone",
                "Civil_Twilight", "Nautical_Twilight", "Astronomical_Twilight") := NULL]

#Drop Distance.mi(The length of the road extent affected by the accident) 
#not much variability
summary(data_filter$`Distance(mi)`)
sd(data_filter$`Distance(mi)`)
hist(data_filter$`Distance(mi)`, xlim = c(0, 3), breaks = 50)

data_filter[, `Distance(mi)` := NULL]

#Check Sunrise_Sunset: 3 missing
table(data_filter$Sunrise_Sunset)

#Include only complete cases
data_complete = data_filter[rowSums(is.na(data_filter)) == 0, ] 
data_complete = data_complete[Sunrise_Sunset != "",]     #remove 3 obs with Sunrise_Sunset == ""

table(data_complete$Sunrise_Sunset)


#Check Missingness: No Missingness, Data with complete cases
colSums(apply(data_complete, MARGIN = 2, is.na))  

str(data_complete)



#Finally, check outcome variable: Severity
table(data_complete$Severity)  #Only 24 cases have severity 1 (least severe accident)

  #combine Severity 1 with Severity 2, and adjust scale to 1-3
data_complete[Severity == 1, Severity := 2]
data_complete[, Severity := Severity-1]
table(data_complete$Severity)

#
# #attempt to aggregate county -- 95% urban 
# urban <- c("Alamance", "Buncombe", "Craven", "Cabarrus", "Dare",
#            "Cumberland", "Durham", "Gaston", "Guilford", "Mecklenburg", 
#            "New Hanover", "Onslow", "Orange", "Wake")
# #suburban <- c("Alexander", "Brunswick", "Cabarrus", "Chatham", "Currituck", "Davidson", 
# #              "Davie", "Franklin", "Gaston", "Gates", "Haywood", "Henderson", "Hoke", 
# #              "Iredell", "Johnston", "Jones", "Lincoln", "Madison", "Pamlico", "Pender", 
# #              "Person", "Randolph", "Rockingham", "Rowan", "Stokes", "Union", "Yadkin")
# data_complete$County <- data_complete$County %>% as.character()
# data_complete$County[!(data_complete$County %in% c(urban))] <- "Rural"
# data_complete$County[data_complete$County %in% urban] <- "Urban"
# #data_complete$County[data_complete$County %in% suburban] <- "Suburban"
# data_complete$County <- data_complete$County %>% as.factor()



#Another possible outcome to predict: Duration of car accidents management (that causes traffic congestion).

data_complete$Start_Time= as.POSIXct(data_complete$Start_Time, format = "%Y-%m-%d %H:%M:%OS")
data_complete$End_Time= as.POSIXct(data_complete$End_Time, format = "%Y-%m-%d %H:%M:%OS")
data_complete$time_diff <- difftime(data_complete$End_Time,data_complete$Start_Time) %>% as.numeric()
ggplot(data_complete, aes(time_diff)) + geom_histogram(binwidth = 1) + xlim(0,100) + 
  geom_vline(xintercept = c(37,53,85), col = "red")

data_complete$time_diff[data_complete$time_diff <= 37] <- 1
data_complete$time_diff[data_complete$time_diff > 37 & data_complete$time_diff<= 53] <- 2
data_complete$time_diff[data_complete$time_diff > 53 & data_complete$time_diff<= 85] <- 3
data_complete$time_diff[data_complete$time_diff > 85 ] <- 4
data_complete$time_diff <- data_complete$time_diff %>% as.factor()
summary(data_complete$time_diff)



#Use data from Years before 2019 as traning data set, and Data from 2019 as testing data set

train_data = data_complete[year < 2019, ]
test_data = data_complete[year == 2019, ]
nrow(data_complete) == nrow(train_data) + nrow(test_data)   #Checking number of observations


#Write to CSV file
fwrite(data_complete, "./Data/NC_Accidents_filtered.csv")
fwrite(train_data, "./Data/NC_Accidents_trn.csv")
fwrite(test_data, "./Data/NC_Accidents_tst.csv")
