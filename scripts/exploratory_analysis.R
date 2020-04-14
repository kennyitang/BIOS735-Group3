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
library(tidyverse)
library(data.table)
library(sf)

#' ---
#' title: "BIOS 735 Final Project: Car Accident Severity - Exploratory Analysis Module"
#' output: pdf_document
#' author: "Marissa Ashner, Yen Chang, Marco Chen, Weifang Liu, Yi Tang, Joyce Yan"
#' date: "`r format(Sys.time(), '%m/%d/%Y')`"
#' ---

#EDA is part of a Training process

#'Perform Exploratory Data Analysis on the training data set

trn.data = fread("./Data/NC_Accidents_trn.csv")
str(trn.data)
variable.names(trn.data)

trn.data$Severity.c = as.factor(trn.data$Severity)  #Make severity a factor varialbe
trn.data$time_diff = as.factor(trn.data$time_diff)  #Make difference in time (categorical) a factor variable


#=========================  Response : Severity (1/2/3) =======================================

#Numerical variables

  #test = trn.data[1:100,]


  #Density plots of the Numerical variables by Severity
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)],
            y = trn.data$Severity.c,
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            plot = "density",
            adjust = 1.5,
            pch = "|",
            layout = c(2, 3),
            auto.key = list(columns = 3))
  #Densities don't differ much in terms of the location by severity for each numerical variable

  #Box plots of the NUmerical variables by Severity
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)], 
            y = trn.data$Severity.c,
            plot = "box",
            scales = list(y = list(relation = "free"),
                          x = list(rot = 90)),
            layout = c(5, 1))


  #Scatter plots of the Numerical variables by Severity - not very readable b/c of too many data points

#If the following error is generated for you:
#   Error in grid.Call.graphics(C_downviewport, name$name, strict) : 
#   Viewport 'plot_01.panel.1.1.off.vp' was not found
#Then need to install package "ellipse"
#   install.packages("ellipse")
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)], 
            y = trn.data$Severity.c,
            plot = "ellipse",
            auto.key = list(columns = 3))






#Categorical variables
# "Side" "Weather_Condition"  "Crossing" "Traffic_Signal" "Sunrise_Sunset" "weekday" "interstate"


  #Cross tabs of Severity and each categorical variable and Bar charts by Severity

#Side: the relative side of the street (Right/Left) in address field
table(trn.data$Severity.c, trn.data$Side)
ggplot(trn.data, aes(x = Side, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Weather condition: Too many categories, need to aggregate, ignore for now
# table(trn.data$Severity.c, trn.data$Weather_Condition) 
# table(trn.data$Weather_Condition)


# Indicator of presence of crossing in a nearby location.
table(trn.data$Severity.c, trn.data$Crossing)
ggplot(trn.data, aes(x = Crossing, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


# Indicator of presence of traffic signal in a nearby location.
table(trn.data$Severity.c, trn.data$Traffic_Signal)
ggplot(trn.data, aes(x = Traffic_Signal, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Sunrise_Sunset: Shows the period of day (i.e. day or night) based on sunrise/sunset.
table(trn.data$Severity.c, trn.data$Sunrise_Sunset)
ggplot(trn.data, aes(x = Sunrise_Sunset, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Weekday (Weekday?)
table(trn.data$Severity.c, trn.data$weekday)
ggplot(trn.data, aes(x = weekday, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Interstate
table(trn.data$Severity.c, trn.data$interstate)
ggplot(trn.data, aes(x = interstate, group = Severity.c)) +
  geom_bar(aes(y = ..prop.., fill = Severity.c), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~Severity.c) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()
# Clear discrepancy in the distribution of interstate by severity of accident







#========  Secondary Response: Categorized Duration of car accidents management (that causes traffic congestion) ==========

#Numerical variables

#test = trn.data[1:100,]


#Density plots of the Numerical variables by Severity
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)],
            y = trn.data$time_diff,
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            plot = "density",
            adjust = 1.5,
            pch = "|",
            layout = c(2, 3),
            auto.key = list(columns = 3))
#Densities don't differ much in terms of the location by severity for each numerical variable

#Box plots of the NUmerical variables by Severity
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)], 
            y = trn.data$time_diff,
            plot = "box",
            scales = list(y = list(relation = "free"),
                          x = list(rot = 90)),
            layout = c(5, 1))


#Scatter plots of the Numerical variables by Severity - not very readable b/c of too many data points

#If the following error is generated for you:
#   Error in grid.Call.graphics(C_downviewport, name$name, strict) : 
#   Viewport 'plot_01.panel.1.1.off.vp' was not found
#Then need to install package "ellipse"
#   install.packages("ellipse")
featurePlot(x = trn.data[, .(`Temperature(F)`, `Humidity(%)`, `Pressure(in)`, 
                             `Visibility(mi)`, `Wind_Speed(mph)`)], 
            y = trn.data$time_diff,
            plot = "ellipse",
            auto.key = list(columns = 3))






#Categorical variables
# "Side" "Weather_Condition"  "Crossing" "Traffic_Signal" "Sunrise_Sunset" "weekday" "interstate"


#Cross tabs of Severity and each categorical variable and Bar charts by Severity

#Side: the relative side of the street (Right/Left) in address field
table(trn.data$time_diff, trn.data$Side)
ggplot(trn.data, aes(x = Side, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Weather condition: Too many categories, need to aggregate, ignore for now
# table(trn.data$time_diff, trn.data$Weather_Condition) 
# table(trn.data$Weather_Condition)


# Indicator of presence of crossing in a nearby location.
table(trn.data$time_diff, trn.data$Crossing)
ggplot(trn.data, aes(x = Crossing, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


# Indicator of presence of traffic signal in a nearby location.
table(trn.data$time_diff, trn.data$Traffic_Signal)
ggplot(trn.data, aes(x = Traffic_Signal, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Sunrise_Sunset: Shows the period of day (i.e. day or night) based on sunrise/sunset.
table(trn.data$time_diff, trn.data$Sunrise_Sunset)
ggplot(trn.data, aes(x = Sunrise_Sunset, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Weekday (Weekday?)
table(trn.data$time_diff, trn.data$weekday)
ggplot(trn.data, aes(x = weekday, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()


#Interstate
table(trn.data$time_diff, trn.data$interstate)
ggplot(trn.data, aes(x = interstate, group = time_diff)) +
  geom_bar(aes(y = ..prop.., fill = time_diff), stat = "count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), 
            stat = "count", 
            vjust = -0.5, size = 3) +
  facet_grid(~time_diff) +
  labs(y = "Percent") + 
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()
# Clear discrepancy in the distribution of interstate by severity of accident

