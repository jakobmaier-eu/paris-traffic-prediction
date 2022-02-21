#-------- Start Header for any file -----------#
# First, set working directory to Folder with repo:

rm(list=objects()) # Clean the global environment
library(XML)
library(RCurl)
library(magrittr)
library(mgcv)
library(ProjetML1)
library(tidyverse)
library(dplyr)
library(lubridate)
library(weathermetrics)
library(ranger)
library(GGally)
#----------- End Header ----------------#

# Prepare data first: 
edge_index = "concorde - saint michel" # We study an arbitrary edge
df_train = data_train[[paste(edge_index)]]
df_test = data_test[[paste(edge_index)]]

daysToNumber <- function(day){
  if(day == "lundi"){return(1)}
  if(day == "mardi"){return(2)}
  if(day == "mercredi"){return(3)}
  if(day == "jeudi"){return(4)}
  if(day == "vendredi"){return(5)}
  if(day == "samedi"){return(6)}
  if(day == "dimanche"){return(7)}
}

df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
df_train$weekendsIndicator = as.factor(df_train$weekendsIndicator)
df_test$weekendsIndicator = as.factor(df_test$weekendsIndicator)

#### Assumptions:
# 1. Do not use the variables
# - year, day, covidIndex : They shouldn't matter.
# - neighbors : Let's try without them first.
# 

# TODO: 
# - Run a random GAM with little variables.
# - Compare scores with benchmark models.
# - Import benchmark models first.
# - Keep adding variables to the GAM (do a forward backwar search)
# - For each added variable tweak the k. Check out TP4_gam to generate the graphs.
# - When motivation < eps, do XGboost on the huge dataframe.

cluster_hours = function(h){
  if (h %in% c(22,23,0,1,2,3,4,5,6)){return("night")}
  if (h %in% c(6,7)){return("6_7")}
  if (h %in% c(6,7)){return("6_7")}
  if (h == 8){return("8")}
  if (h == 9){return("9")}
  if (h == 10){return("10")}
  if (h %in% c(11,12,13)){return("noon")}
  if (h %in% c(14,15,16,17)){return("afternoon")}
  if (h %in% c(18,19)){return("eve_rush")}
  if (h %in% c(20,21)){return("eve")}
  else{return("asfd")}
}
df_train$hour_groups = sapply(as.factor(df_train$hour), cluster_hours)
df_test$hour_groups = sapply(as.factor(df_test$hour), cluster_hours)

for (group in unique(df_train$hour_groups)){
  indices = df_train$hour_groups == group
  plot(df_train[indices,]$rateCar_LaggedHour, df_train[indices,]$rateCar,
       main = group, xlab = "rateCar lagged by 1 hour", ylab = "rateCar now")
}


### LinRegs and compare coefficients (ANOVA??)
hours_linregs = data.frame(
  hour = rep(0,24), 
  intercept = rep(0,24),
  slope = rep(0,24))
for (h in 0:23){
  hour_idxs = df_train$hour == h
  hour_data = subset(df_train[hour_idxs,], select=c(rateCar, rateCar_LaggedHour))
  mod = lm(rateCar ~ rateCar_LaggedHour, data = hour_data)
  hours_linregs$hour[h + 1] = h
  hours_linregs$intercept[h + 1] = round(mod$coefficients[1],3)
  hours_linregs$coeff[h + 1] = round(mod$coefficients[2],3)
}
print(hours_linregs, row.names=F)


ggpairs(subset(df_train[1:1000,], select= c(hour, rateCar_LaggedHour, rateCar))) 
          #state, rateCar_LaggedDay, rateCar_LaggedHour, rateCar_LaggedWeek )))



formula = rateCar ~ 
  s(state) +
  month +
  s(hour, bs="cc", by=weekendsIndicator)+
  time + # If there are long term effects, we suppose them to be linear
  s(toy, bs = "cc") + # Cyclic to find some seasonal changes
  s(weekdays, bs="cc", k=4) + # Can we find a weekly structure?
  weekendsIndicator + 
  temperature + # TODO: What kind of behaviour ?
  precipitation + # TODO: What kind of behaviour ?
  winterHolidaysIndicator + summerHolidaysIndicator + bankHolidaysIndicator + 
  nbCar_LaggedWeek + 
  nbCar_LaggedDay + 
  nbCar_LaggedHour + 
  rateCar_LaggedWeek +
  rateCar_LaggedDay + 
  rateCar_LaggedHour 


g = gam(data=df_train, formula = formula)
summary(g)

par(mfrow=c(2,2))
plot(g, residuals=T, rug=T, se=F, pch=20)



### Finetuning via CV:

CV_gam = function(formula, df_train, no_folds){
  K = no_folds 
  data = df_train
  folds <- cut(seq(1,nrow(data)),breaks=K,labels=FALSE)
  fold_scores = c()
  start_time = Sys.time()
  for(i in 1:K){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    g = gam(data=testData, formula=formula)
    Y_test = testData$rateCar
    Y_predict = predict(g, newdata=subset(testData, select = -c(rateCar)))
    fold_scores = c(fold_scores, rmse(Y_test, Y_predict))
  }
  exec_time = Sys.time()-start_time
  score = mean(fold_scores)
  ret = data.frame(exec_time = exec_time, score = score, 
                   formula = formula, no_folds = no_folds)
  return(ret)
}

