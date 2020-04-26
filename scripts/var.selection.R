 library(MASS)
devtools::load_all("../group3project")
data("trn_data")
data("tst_data")

# code the outcome to be factors
trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

trn_data$temperature_sc = scale(trn_data$`Temperature(F)`)
trn_data$humdity_sc = scale(trn_data$`Humidity(%)`)
trn_data$pressure_sc = scale(trn_data$`Pressure(in)`)
trn_data$windspeed_sc = scale(trn_data$`Wind_Speed(mph)`)
trn_data$Visibility_sc = scale(trn_data$`Visibility(mi)`)

mod <- polr(Severity_c ~ Source + Side + temperature_sc + humdity_sc + pressure_sc + 
              Visibility_sc + windspeed_sc + Crossing + Traffic_Signal +
              Sunrise_Sunset + weekday + interstate, data = trn_data, Hess=TRUE) 
step.model <- stepAIC(mod, direction = "both", trace = F)
summary(step.model)