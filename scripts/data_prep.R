
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
data.filter = data[!is.na(Weather_Timestamp_n),]  #Remove obs. missing all weather variables (if without Timestamp)

variable.names(data.filter)

# Weather variables:
#"Temperature.F." "Wind_Chill.F." "Humidity..." "Pressure.in." "Visibility.mi." "Wind_Direction" "Wind_Speed.mph." 
#"Precipitation.in." "Weather_Condition"

#Check proportion of missingness for each weather variable
colSums(apply(data.filter[, .(`Temperature(F)`, `Wind_Chill(F)`, `Humidity(%)`,
                              `Pressure(in)`, `Visibility(mi)`, Wind_Direction,
                              `Wind_Speed(mph)`, `Precipitation(in)`, Weather_Condition)], 
              MARGIN = 2, is.na)) / 
  nrow(data.filter)


#Wind Chill and Precipitation have around 60% missingness

  #What should we do with them? Drop them: 
  #Windchill is the function of temperature and wind speed
  #Not much variability in Precipitation
hist(data.filter[!is.na(`Wind_Chill(F)`), `Wind_Chill(F)`], breaks = 50)
var(data.filter[!is.na(`Wind_Chill(F)`), `Wind_Chill(F)`])

hist(data.filter[!is.na(`Precipitation(in)`), `Precipitation(in)`], breaks = 50)
var(data.filter[!is.na(`Precipitation(in)`), `Precipitation(in)`])

#Remove Wind Chill and Precipitation
data.filter.1 = data.filter[, c("Wind_Chill(F)", "Precipitation(in)") := NULL]
str(data.filter.1)


#Weather_Condition: too many categories  How do we want to categorize them?
table(trn.data$Weather_Condition) %>% as.matrix() / nrow(data.filter.1) * 100




#Checking variability in binary variables:
#Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, Railway, Roundabout, Station, Stop, 
#Traffic_Calming, Traffic_Signal, Turning_Loop

round(colMeans(data.filter.1[, .(Amenity, Bump,	Crossing,	Give_Way,	Junction,	No_Exit, 
                                 Railway, Roundabout, Station, Stop, Traffic_Calming, 
                                 Traffic_Signal)]) * 100, 2)
summary(as.factor(data.filter.1[, Side]))

#Drop all TRUE/FALSE variables (Not much variability) except Crossing and Traffic_Signal
data.filter.2 = data.filter.1[, c("Amenity", "Bump",	"Give_Way",	"Junction",	"No_Exit", "Railway", 
                                  "Roundabout", "Station", "Stop", "Traffic_Calming", "Turning_Loop") := NULL]
str(data.filter.2)


#Checking missingness in the rest of the variables
colSums(apply(data.filter.2, MARGIN = 2, is.na))


#Drop End_Lat (119596 missing), End_Lng (119596 missing) and
#TMC and Number (not useful infos) and
#State, Country, and Timezone (No variability)  and
#Civil_Twilight, Nautical_Twilight and Astronomical_Twilight (Duplicate info with Sunrise_Sunset)
data.filter.3 = data.filter.2[, c("End_Lat", "End_Lng", 
                                  "TMC", "Number", "State", "Country", "Timezone",
                                  "Civil_Twilight", "Nautical_Twilight", "Astronomical_Twilight") := NULL]

#Drop Distance.mi(The length of the road extent affected by the accident) 
#cuz not much variability and not the data we can collect unless there's accident
summary(data.filter.3$`Distance(mi)`)
sd(data.filter.3$`Distance(mi)`)
hist(data.filter.3$`Distance(mi)`, breaks = 50)

data.filter.4 = data.filter.3[, `Distance(mi)` := NULL]

data.complete = data.filter.4[rowSums(is.na(data.filter)) == 0, ]  #Include only complete cases

colSums(apply(data.complete, MARGIN = 2, is.na))  #No Missingness, Data with complete cases

str(data.complete)



#Finally, check outcome variable: Severity
table(data.complete$Severity)  #Only 24 cases have severity 1 (least severe accident)

  #combine Severity 1 with Severity 2, and adjust scale to 1-3
data.complete[Severity == 1, Severity := 2]
data.complete[, Severity := Severity-1]
table(data.complete$Severity)

#
# #attempt to aggregate county -- 95% urban 
# urban <- c("Alamance", "Buncombe", "Craven", "Cabarrus", "Dare",
#            "Cumberland", "Durham", "Gaston", "Guilford", "Mecklenburg", 
#            "New Hanover", "Onslow", "Orange", "Wake")
# #suburban <- c("Alexander", "Brunswick", "Cabarrus", "Chatham", "Currituck", "Davidson", 
# #              "Davie", "Franklin", "Gaston", "Gates", "Haywood", "Henderson", "Hoke", 
# #              "Iredell", "Johnston", "Jones", "Lincoln", "Madison", "Pamlico", "Pender", 
# #              "Person", "Randolph", "Rockingham", "Rowan", "Stokes", "Union", "Yadkin")
# data.complete$County <- data.complete$County %>% as.character()
# data.complete$County[!(data.complete$County %in% c(urban))] <- "Rural"
# data.complete$County[data.complete$County %in% urban] <- "Urban"
# #data.complete$County[data.complete$County %in% suburban] <- "Suburban"
# data.complete$County <- data.complete$County %>% as.factor()



#Another possible outcome to predict: Duration of car accidents management (that causes traffic congestion).

data.complete$Start_Time= as.POSIXct(data.complete$Start_Time, format = "%Y-%m-%d %H:%M:%OS")
data.complete$End_Time= as.POSIXct(data.complete$End_Time, format = "%Y-%m-%d %H:%M:%OS")
data.complete$time_diff <- difftime(data.complete$End_Time,data.complete$Start_Time) %>% as.numeric()
ggplot(data.complete, aes(time_diff)) + geom_histogram(binwidth = 1) + xlim(0,100) + 
  geom_vline(xintercept = c(37,53,85), col = "red")

data.complete$time_diff[data.complete$time_diff <= 37] <- 1
data.complete$time_diff[data.complete$time_diff > 37 & data.complete$time_diff<= 53] <- 2
data.complete$time_diff[data.complete$time_diff > 53 & data.complete$time_diff<= 85] <- 3
data.complete$time_diff[data.complete$time_diff > 85 ] <- 4
data.complete$time_diff <- data.complete$time_diff %>% as.factor()
summary(data.complete$time_diff)



#Use data from Years before 2019 as traning data set, and Data from 2019 as testing data set

train.data = data.complete[year < 2019, ]
test.data = data.complete[year == 2019, ]
nrow(data.complete) == nrow(train.data) + nrow(test.data)   #Checking number of observations


#Write to CSV file
fwrite(data.complete, "./Data/NC_Accidents_filtered.csv")
fwrite(train.data, "./Data/NC_Accidents_trn.csv")
fwrite(test.data, "./Data/NC_Accidents_tst.csv")
