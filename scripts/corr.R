# correlation
library(vcd)
library(corrplot)

# plot the correlations between continuous variables 
devtools::load_all("../group3project")
data("trn_data")
trn_data$Severity = as.factor(trn_data$Severity)
trn_data <- trn_data %>% mutate_at(c("Temperature(F)",
                                     "Humidity(%)",
                                     "Pressure(in)",
                                     "Wind_Speed(mph)",
                                     "Visibility(mi)"), ~(scale(.) %>% as.vector))
colnames(trn_data)[3:7] =c("temperature_sc", "humdity_sc", "pressure_sc", "windspeed_sc", "Visibility_sc")
tokeep <- which(sapply(trn_data,is.numeric))
x_numeric <- trn_data[ ,tokeep]

# correlation plot
corrplot(cor(x_numeric), type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 35,
         mar=c(0,0,1,0),
         title = "Correlation plot of continuous covariates")

# chi-sq test of independence of categorical variables and Cramer's V
cols <- c("Source","Side","Crossing","Traffic_Signal","Sunrise_Sunset","weekday","interstate")
pairs <- (combn(cols,2))
pairs <- rbind(pairs, pairs)

for (i in 1:ncol(pairs)) {
  source.side = table(trn_data[, pairs[1,i]],trn_data[, pairs[2,i]])
  pairs[3, i] = chisq.test(source.side)$p.value
  pairs[4, i] = assocstats(source.side)$cramer
}

# maximum of Cramer's V is about 0.4 
max(pairs$v)

# get the Cramer's V matrix
corr <- matrix(1,7,7)
colnames(corr) <- cols
rownames(corr) <- cols

for (i in 1:nrow(pairs)) {
  v1 <- as.character(pairs[i,1])
  v2 <- as.character(pairs[i,2])
  corr[v1, v2] <- pairs[i,4]
  corr[v2, v1] <- pairs[i,4]
}

# plot pairwise Cramer's V
corrplot(corr, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 35,
         mar=c(0,0,1,0),
         title = "Cramer's V for nominal covariates")

