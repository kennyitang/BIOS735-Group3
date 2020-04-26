# load packages
library(ordinalNet)
library(dplyr)
library(caret)
library(ggplot2)
# load our package and datasets 
devtools::load_all("../group3project")
data("trn_data")
data("tst_data")

# code the outcome to be factors
trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

# scale continuous covariates
trn_data2 <- trn_data %>% mutate_at(c("Temperature(F)",
                                 "Humidity(%)",
                                 "Pressure(in)",
                                 "Wind_Speed(mph)",
                                 "Visibility(mi)"), ~(scale(.) %>% as.vector))

colnames(trn_data2)[3:7] =c("temperature_sc", "humdity_sc", "pressure_sc", "windspeed_sc", "Visibility_sc")

# fit the full model with 12 predictors 
formula = Severity_c ~ Source + Side + temperature_sc + humdity_sc + pressure_sc + 
  Visibility_sc + windspeed_sc + Crossing + Traffic_Signal +
  Sunrise_Sunset + weekday + interstate

x = recode.factor(formula = formula, data = trn_data2)
y = trn_data2[, "Severity_c"]

set.seed(1)
time.start = Sys.time()
fit <- ordinalNet(as.matrix(x), y, 
                   family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)
time.stop = Sys.time()
time.stop - time.start

# predict with the test dataset 
tst_data2 = tst_data %>% mutate_at(c("Temperature(F)",
                                      "Humidity(%)",
                                      "Pressure(in)",
                                      "Wind_Speed(mph)",
                                      "Visibility(mi)"), ~(scale(.) %>% as.vector))

colnames(tst_data2)[3:7] = c("temperature_sc", "humdity_sc", "pressure_sc", "windspeed_sc", "Visibility_sc")

testx = recode.factor(formula = formula, data = tst_data2)

predicted = predict(fit, newx = as.matrix(testx), type="class")

# accuracy
calc_acc(actual = tst_data$Severity_c, predicted = predicted)

# confusion matrix
confusionMatrix(as.factor(predicted), tst_data$Severity_c)


# plot coefficients from pom vs ordinalNet
trn_data2$Severity_c = factor(trn_data2$Severity, levels = c(1,2,3), 
                             labels = c(1:length(unique(trn_data$Severity))), ordered = T)
pom = pom.est(formula, data = trn_data2, SE = T, details = F)
coefs = data.frame(-coef(fit)[3:15])
colnames(coefs) <- "ordinalNet"
coefs$pom = pom$Estimates[3:15]
coefs <- coefs[order(-abs(coefs$ordinalNet)),]

df <- data.frame(model=rep(c("ordinalNet", "pom"), each=13),
                 predictor=rep(row.names(coefs), 2),
                 coef=c(coefs$ordinalNet, coefs$pom))
df$coef <- round(df$coef, digits = 2)

ggplot(df, aes(x = reorder(predictor, -abs(coef)), y = coef, fill = model)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge())+
  xlab("Predictors") + theme_bw()+
  theme(text = element_text(size=16),axis.text.x = element_text(size = 14,angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(limits = c(-5,5)) + scale_fill_manual(values=c('#ED7D31','#4472C4'))+
  ggtitle("Prop. Odds Model Coefficient Estimates") 

