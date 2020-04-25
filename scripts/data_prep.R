
# all packages
library(tidyverse)
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
library(dplyr)

#' ---
#' title: "BIOS 735 Final Project: Car Accident Severity - Data Preparation Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---

# Subset the raw data to focus only on accidents in NC
raw = fread('US_Accidents_Dec19.csv')
NC = raw[State=='NC',]
fwrite(NC, 'NC_Accidents.csv') 

# Read NC accidents data
data = fread("./Data/NC_Accidents.csv")
str(data)

#======== Use variable Weather_Timestamp to determine year of data =============
#Parsing character to Datetime variable
data$Weather_Timestamp_n = as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%OS")
data$year = lubridate::year(data$Weather_Timestamp_n)   #Year of observation recorded

summary(data$year)  #there are observations missing all weather info (missing Weather Timestamp)

#Exclude data with missing weather info (missing weather Timestamp)
data_filter = data[!is.na(Weather_Timestamp_n),]  #Remove obs. missing all weather variables (if without Timestamp)

names(data_filter)

#======== Weather variables: ==============================================
#"Temperature.F." "Wind_Chill.F." "Humidity..." "Pressure.in." "Visibility.mi." "Wind_Direction" "Wind_Speed.mph." 
#"Precipitation.in." "Weather_Condition"

#Check proportion of missingness for each weather variable
colSums(apply(data_filter[, .(`Temperature(F)`, `Wind_Chill(F)`, `Humidity(%)`,
                              `Pressure(in)`, `Visibility(mi)`, Wind_Direction,
                              `Wind_Speed(mph)`, `Precipitation(in)`, Weather_Condition)], 
              MARGIN = 2, is.na)) / 
  nrow(data_filter)


#======= Wind Chill and Precipitation have around 60% missingness ===========

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

#======= Weather_Condition and Wind_Direction =============================
#Weather_Condition: too many categories  How do we want to recategorize them? ignore for now
table(data_filter$Weather_Condition) %>% as.matrix() / nrow(data_filter) * 100
#Wind_Direction: too many categories  How do we want to recategorize them? ignore for now
table(data_filter$Wind_Direction) %>% as.matrix() / nrow(data_filter) * 100

#========= Binary Variables =====================================================
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

#========= Dropping other variables =====================================
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

#=========== Include only complete cases =========================
data_complete = data_filter[rowSums(is.na(data_filter)) == 0, ] 
data_complete = data_complete[Sunrise_Sunset != "",]     #remove 3 obs with Sunrise_Sunset == ""

table(data_complete$Sunrise_Sunset)


#Check Missingness: No Missingness, Data with complete cases
colSums(apply(data_complete, MARGIN = 2, is.na))  

str(data_complete)



#========= Finally, check outcome variable: Severity ===================
table(data_complete$Severity)  #Only 24 cases have severity 1 (least severe accident)

  #combine Severity 1 with Severity 2, and adjust scale to 1-3
data_complete[Severity == 1, Severity := 2]
data_complete[, Severity := Severity-1]
table(data_complete$Severity)

#=========== county ===============================
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

# # #attempt to aggregate weather 
# clear <- c("Fair", "Mostly Cloudy", "Clear", "Cloudy", "Partly Cloudy", "Overcast",
# "Scattered Clouds")
# fog <- c("Fog", "Haze", "Patches of Fog", "Shallow Fog", "Smoke")
# precip <- c("Light Rain", "Light Rain / Windy", "Heavy Rain", "Light Drizzle",
# "Drizzle", "T-Storm", "Light Freezing Rain / Windy", "Rain / Windy", "Thunder",
# "Heavy Rain / Windy", "Light Snow", "Thunderstorms and Rain", "Light Thunderstorms and Rain",
# "Light Freezing Drizzle", "N/A Precipitation", "Heavy Snow", "Heavy Freezing Rain",
# "Heavy Thunderstorms and Rain", "Light Freezing Rain", "Ice Pellets", "Thunderstorm",
# "Light Ice Pellets", "Mist", "Rain", "Heavy Drizzle", "Wintry Mix", "Heavy T-Storm",
# "Heavy T-Storm / Windy", "Light Rain with Thunder", "Thunder in the Vicinity", "Thunder / Windy",
# "T-Storm / Windy", "Snow", "Light Rain Showers", "Blowing Snow")
# wind <- c("Mostly Cloudy / Windy", "Cloudy / Windy", "Fair / Windy", "Partly Cloudy / Windy")
# data_complete$Weather_Condition[(data_complete$Weather_Condition %in% c(fog))] <- "Fog"
# data_complete$Weather_Condition[(data_complete$Weather_Condition %in% c(precip))] <- "Precipitation"
# data_complete$Weather_Condition[(data_complete$Weather_Condition %in% c(clear))] <- "Clear"
# data_complete$Weather_Condition[(data_complete$Weather_Condition %in% c(wind))] <- "Wind"
# data_complete$Weather_Condition <- data_complete$Weather_Condition %>% as.factor()
# data_complete = data_complete[Weather_Condition != "",]  
# table(data_complete$Severity, data_complete$Weather_Condition)


#===== Another possible outcome to predict: Duration of car accidents management (that causes traffic congestion). ==============

data_complete$Start_Time= as.POSIXct(data_complete$Start_Time, format = "%Y-%m-%d %H:%M:%OS")
data_complete$End_Time= as.POSIXct(data_complete$End_Time, format = "%Y-%m-%d %H:%M:%OS")
data_complete$time_diff <- difftime(data_complete$End_Time,data_complete$Start_Time) %>% as.numeric() # time_diff in minutes
# data_complete$time_diff_days <-  difftime(data_complete$End_Time,data_complete$Start_Time, units = "days") %>% as.numeric()  # time_diff in days

#Clear divisions in the time differences
ggplot(data_complete, aes(time_diff)) + geom_histogram(binwidth = 1) + xlim(0,100) + 
  geom_vline(xintercept = c(37,53,85), col = "red")

data_complete$time_diff[data_complete$time_diff <= 37] <- 1
data_complete$time_diff[data_complete$time_diff > 37 & data_complete$time_diff<= 53] <- 2
data_complete$time_diff[data_complete$time_diff > 53 & data_complete$time_diff<= 85] <- 3
data_complete$time_diff[data_complete$time_diff > 85 ] <- 4
data_complete$time_diff <- data_complete$time_diff %>% as.factor()
summary(data_complete$time_diff)

#======= add weekday variable based on Start_Time =================
data_complete$weekday = lubridate::wday(data_complete$Start_Time) %in% 2:6

#======== mapping accidents over NC counties =========================
nc <- sf::st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = T)

countyplot = ggplot() + 
  geom_sf(data=nc, fill = "white") +
  geom_point(data = data_complete, aes(x = Start_Lng, y = Start_Lat, label = Street), alpha = 0.1, color = "red") +
  theme_bw() +
  labs(title = "NC Accidents over County Map", x = "Longitude", y = "Latitude")

countyplot

ggsave("./report/countyplot.png")

# plotly::ggplotly(countyplot, tooltip = "label")

#======== exploring highway/interstate ===================================
# # filter out known non-highway and highway labels and see what's left
# data_street = data_complete %>%
#   filter(!str_detect(Street, "Rd|Ave|Blvd|St|Dr|Ln|Cir|Pl|Ct|Way|Ter|Trl|Run|Loop|Peakway|Walk|Xing|Ext|Park")) %>%
#   filter(!str_detect(Street, "Hwy|Pkwy|Highway|Expy|Expressway|Fwy|Interstate|I(-|\\s)\\d|NC(-|\\s)|US(-|\\s)(\\d|H)")) 
# # seems like the rest are non-highway
# table(data_street$Street)
# # create T/F variable for highway
# data_complete$highway = str_detect(data_complete$Street, "Hwy|Pkwy|Highway|Expy|Expressway|Fwy|Interstate|I(-|\\s)\\d|NC(-|\\s)|US(-|\\s)(\\d|H)")

# create T/F variable for interstate
data_complete$interstate = str_detect(data_complete$Street, "Interstate|I(-|\\s)\\d") | str_detect(data_complete$Description, "Interstate|I(-|\\s)\\d")

# plotting interstate matches
interstateplot = ggplot() + 
  geom_sf(data=nc, fill = "white") +
  geom_point(data = filter(data_complete, str_detect(Street, "Interstate|I(\\s|-)")), 
             aes(x = Start_Lng, y = Start_Lat, label = Street, color = "Street"), alpha = 0.1) +
  geom_point(data = filter(data_complete, !str_detect(Street, "Interstate|I(\\s|-)") & str_detect(Description, "Interstate|I(\\s|-)")), 
             aes(x = Start_Lng, y = Start_Lat, label = Street, color = "Description"), alpha = 0.1) +
  scale_color_manual(name = "", values = c("Street" = "red", "Description" = "blue")) +
  labs(title = "Interstate matches in Street vs. only in Description",  x = "Longitude", y = "Latitude") +
  theme_bw()
plotly::ggplotly(interstateplot, tooltip = "label")

# plotting interstate matches in the same color
ggplot() + 
  geom_sf(data=nc, fill = "white") +
  geom_point(data = filter(data_complete, interstate == T), 
             aes(x = Start_Lng, y = Start_Lat), alpha = 0.1, color = "blue") +
  labs(title = "NC Interstate Accidents over County Map", x = "Longitude", y = "Latitude") +
  theme_bw()

ggsave("./report/interstateplot.png")

# # looking at severity for interstates vs. highways vs. other roads
# data_roadtype = data_complete %>%
#   mutate(road_type = case_when(
#     interstate ==T ~ "interstate",
#     interstate ==F & highway ==T ~ "highway",
#     interstate ==F & highway ==F ~ "other"
#   ))
# 
# ggplot(data_roadtype) +
#   geom_bar(aes(x = Severity, fill = road_type), position = "dodge") +
#   theme_bw()

#========= Source ============================================
bing_plot = data_complete %>%
  filter(str_detect(Source, "Bing")) %>%
  ggplot() + 
  geom_sf(data=nc, fill = "white") +
  geom_point(aes(x = Start_Lng, y = Start_Lat, label = Street), alpha = 0.1) +
  labs(title = "Bing",  x = "Longitude", y = "Latitude") +
  theme_bw()

mapquest_plot = data_complete %>%
  filter(str_detect(Source, "MapQuest")) %>%
  ggplot() + 
  geom_sf(data=nc, fill = "white") +
  geom_point(aes(x = Start_Lng, y = Start_Lat, label = Street), alpha = 0.1) +
  labs(title = "MapQuest",  x = "Longitude", y = "Latitude") +
  theme_bw()
bing_plot
mapquest_plot

#======== Write data sets to CSV ======================================

#Use data from Years before 2019 as traning data set, and Data from 2019 as testing data set

train_data = data_complete[year < 2019, ]
test_data = data_complete[year == 2019, ]
nrow(data_complete) == nrow(train_data) + nrow(test_data)   #Checking number of observations


#Write to CSV file
fwrite(data_complete, "./Data/NC_Accidents_filtered.csv")
fwrite(train_data, "./Data/NC_Accidents_trn.csv")
fwrite(test_data, "./Data/NC_Accidents_tst.csv")




