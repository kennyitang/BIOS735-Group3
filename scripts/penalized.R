# load packages
library(data.table)
library(glmnetcr)
library(ordinalNet)

# training
trn_data <-  fread("./Data/NC_Accidents_trn.csv")
# testing
tst_data <-  fread("./Data/NC_Accidents_tst.csv")
# outcome and predictors 
predictors <- c("Source", "Side", "Temperature(F)", "Humidity(%)", "Pressure(in)", 
                "Visibility(mi)", "Wind_Speed(mph)", "Crossing", "Traffic_Signal",
                "Sunrise_Sunset", "weekday", "interstate")
trn_data$Severity_c <-  as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

dim(trn_data)
summary(trn_data$Severity_c)

x <- trn_data[, predictors, with = F]
y <- trn_data[, "Severity_c"]

# fit <- glmnetcr(x, y)
# print(fit)
# glmnetcr does not work since this is not a high-dimensional setting 


# try ordinalNet 

# convert categorical covariates to indicator variables 
library(fastDummies)

tokeep <- which(sapply(x, is.numeric))
x_categorical <- x[ , -tokeep, with=FALSE]
x_dummy <- dummy_cols(x, select_columns = colnames(x_categorical))

newx <- (x_dummy[, -colnames(x_categorical), with = F])
newy <- as.factor(trn_data$Severity_c)

time.start <- Sys.time()
fit1 <- ordinalNet(as.matrix(x_dummy[, -colnames(x_categorical), with = F]), newy, 
                   family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)
time.stop <- Sys.time()
time.stop - time.start

summary(fit1)
coef(fit1) 
# Temperature, wind speed, source-mapquest-bing have zero effect size

#coef(fit1, matrix=TRUE)
#predict(fit1, type="response")
#predict(fit1, type="class")

x_test <- tst_data[, predictors, with = F]
tokeep <- which(sapply(x_test, is.numeric))
x_test_categorical <- x_test[ , -tokeep, with=FALSE]
x_test_dummy <- dummy_cols(x_test, select_columns = colnames(x_test_categorical))

newx_test <- (x_test_dummy[, -colnames(x_test_categorical), with = F])

predicted <- predict(fit1, newx = as.matrix(newx_test), type="class")

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

calc_acc(actual = tst_data$Severity_c, predicted = predicted)

# [1] 0.9032935



# longitudinal? 

# summary stat analysis and visualization 

# backward selection function 

# rf with ordinal data and variable selection 