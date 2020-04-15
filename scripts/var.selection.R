# variable selection 
library(MASS)
library(data.table)

# training
trn_data <-  fread("./Data/NC_Accidents_trn.csv")
# testing
tst_data <-  fread("./Data/NC_Accidents_tst.csv")

trn_data$Severity_c = as.factor(trn_data$Severity)
trn_data$year_c = as.factor(trn_data$year)
tst_data$Severity_c = as.factor(tst_data$Severity)

str(trn_data)

# look at correlations 
predictors <- c("Source", "Side", "Temperature(F)", "Humidity(%)", "Pressure(in)", 
  "Visibility(mi)", "Wind_Speed(mph)", "Crossing", "Traffic_Signal",
  "Sunrise_Sunset", "weekday", "interstate")
x <- trn_data[, predictors, with = F]
tokeep <- which(sapply(x,is.numeric))
x_numeric <- x[ , tokeep, with=FALSE]

require("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(x_numeric)

# fit the proportional odds model 
time.start <- Sys.time()
mod <- polr(Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
  `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
  Sunrise_Sunset + weekday + interstate, data = trn_data, Hess=TRUE) 
time.stop <- Sys.time()
time.stop - time.start

(ctable <- coef(summary(mod)))

# get p-values
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# only wind speed has p-value > 0.05

# CIs
#(ci <- confint(mod)) did not run this
confint.default(mod) # CIs assuming normality
# only wind speed has CI crossing 0
# effect sizes on temp, humidity, visibility, wind speed are small

time.start <- Sys.time()
step.model <- stepAIC(mod, direction = "both", trace = F)
summary(step.model)
time.stop <- Sys.time()
time.stop - time.start

# get prediction accuracy
predicted <- predict(mod, 
                     newdata = tst_data, 
                     type="class")
length(predicted)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

calc_acc(actual = tst_data$Severity_c, predicted = predicted)

# [1] 0.900643



# question: if we add year as a categorical variable, it is indeed significant, but this might be due to confounders? 
# are we considering interactions between covariates? 
# do we need to standardize the variables?

# need to test for the proportional odds assumption
