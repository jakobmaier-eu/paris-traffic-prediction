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
    g = gam(data=trainData, formula=formula)
    Y_test = testData$rateCar
    Y_predict = predict(g, newdata=subset(testData, select = -c(rateCar)))
    fold_scores = c(fold_scores, rmse(Y_test, Y_predict))
  }
  exec_time = Sys.time()-start_time
  score = mean(fold_scores)
  ret = data.frame(score = score, no_folds = no_folds, exec_time = exec_time, 
                   formula = paste(as.character(formula)[c(2,1,3)], collapse = " "))
  return(ret)
}

hours_linregs = data.frame(hour = 0:23)
for (h in 0:23){
  hour_idxs = df_train$hour == h
  hour_data = subset(df_train[hour_idxs,], select=c(rateCar, rateCar_LaggedHour))
  mod = lm(rateCar ~ rateCar_LaggedHour, data = hour_data)
  hours_linregs$intercept[h + 1] = round(mod$coefficients[1],3)
  hours_linregs$slope[h + 1] = round(mod$coefficients[2],3)
}
hours_linregs$hour = as.integer(hours_linregs$hour)

cluster_hours = function(h){
  if (h %in% c(0,1,2,3,4,5,6,7)){return("night")}
  if (h %in% c(7)){return("7heures")}
  if (h %in% c(8)){return("8heures")}
  if (h %in% c(9)){return("9heures")}
  if (h %in% c(10,11,12,13,14,15)){return("noon")}
  if (h %in% c(16,17)){return("afternoon")}
  if (h %in% c(18,19,20)){return("evening_rush")}
  if (h %in% c(21,22,23)){return("evening")}
  else{return("asfd")}
}
df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))


form_full = rateCar ~ 
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


# See report for tuning of main variables.

### Here, we tune some more:

for (i in c(0.2, 0.5, 0.7, 1, 1.2)){
  plot(ggplot(data=df_train, aes(x=(1/(i+precipitation)), y = rateCar))+
         geom_point())
}

plot(ggplot(data=df_train, aes(x=(1/(1.5+precipitation)), y = rateCar))+
       geom_point())

plot(ggplot(data=df_train, aes(x=(1/(0.5+precipitation)), y = rateCar))+
       geom_point())

plot(ggplot(data=df_train, aes(x=(1/(3+precipitation)), y = rateCar))+
       geom_point())

## Take 1 / 2+precip
# add s((1/(0.5+precipitation)), bs = 'cr', k = 5)

ggplot(data=df_train, aes(x=month, y = rateCar))+geom_point()
# add s(month, bs='cc', k = 10)

ggplot(data=filter(df_train, weekendsIndicator==1), 
       aes(x=rateCar_LaggedDay, y = rateCar))+geom_point()

for (i in 1:7){
  plot(ggplot(data=filter(df_train[1:1e5,], weekdays==i), 
         aes(x=rateCar_LaggedDay, y = rateCar))+geom_point())
}

#------ Model Selection with CV scores -------#


form1 = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour
CV_gam(form1, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(hour, bs="cc", k = 24, by=weekendsIndicator)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.24125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(weekdays, bs="cc", k = 7)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.41625

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(state, bs="cr", k = 5)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(toy, bs="cc", k = 20)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(nbCar_LaggedHour, bs="cr", k = 15, by=GRPS)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.3475

df_train$one_over_precip = (1/(0.5+df_train$precipitation))
form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(one_over_precip, bs = 'cr', k = 5)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(month, bs='cc', k = 10)
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  weekendsIndicator
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  summerHolidaysIndicator
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  bankHolidaysIndicator
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.42125

form2_try= rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  weekdays * rateCar_LaggedDay - rateCar_LaggedDay - weekdays
CV_gam(form2_try, df_train, no_folds = 8)[[1]]
# 2.3775

# --------------- Form 3 try -----------------

form2 = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(hour, bs="cc", k = 24, by=weekendsIndicator)
CV_gam(form2, df_train, no_folds = 8)[[1]]
# 2.24125

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(weekdays, bs="cc", k = 7)
CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.2325

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(state, bs="cr", k = 5)
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.24125

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(toy, bs="cc", k = 20)
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.24

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(nbCar_LaggedHour, bs="cr", k = 15, by=GRPS)
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.21125

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(one_over_precip, bs = 'cr', k = 5)
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.2425

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  s(month, bs='cc', k = 10)
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.24

form3_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  weekdays * rateCar_LaggedDay - rateCar_LaggedDay - weekdays
  CV_gam(form3_try, df_train, no_folds = 8)[[1]]
# 2.21875
  
# --------- Form 4 try
  
form3 = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) + s(nbCar_LaggedHour, bs="cr", k = 15, by=GRPS)
CV_gam(form3, df_train, no_folds = 8)[[1]]
# 2.21125

form4_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + s(hour, bs="cc", k = 24, by=weekendsIndicator) + s(nbCar_LaggedHour, bs="cr", k = 15, by=GRPS) + 
  weekdays * rateCar_LaggedDay - rateCar_LaggedDay - weekdays
CV_gam(form3, df_train, no_folds = 8)[[1]]
# 2.21125

form4_try = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour +
  s(hour, bs="cc", k = 24, by=weekendsIndicator) +
  nbCar_LaggedHour * GRPS - GRPS - nbCar_LaggedHour + 
  weekdays * rateCar_LaggedDay - rateCar_LaggedDay - weekdays
CV_gam(form3, df_train, no_folds = 8)[[1]]
# 2.21125

mod_final = gam(form_final, data=df_train)
summary(mod_final)




form_final = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour + 
  s(hour, bs="cc", k = 24, by=weekendsIndicator) + 
  s(nbCar_LaggedHour, bs="cr", k = 15, by=GRPS)
scores_edges = c()

for (i in 1:69){
  df_train = data_train[[i]]
  df_test = data_test[[i]]
  
  df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
  df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
  df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
  df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))
  
  mod_edge = gam(form_final, data=df_train)
  scores_edges[i] = rmse( predict(mod_edge, newdata = df_test),  
                     df_test$rateCar)
}

mean(score_edges)
saveRDS(scores_edges, file="scores_edges_GAM_final.rds")


### -------- Does simple model generalize better ?

form_simple = rateCar ~ GRPS * rateCar_LaggedHour - GRPS - rateCar_LaggedHour
scores_edges_simple = c()
for (i in 1:69){
  df_train = data_train[[i]]
  df_test = data_test[[i]]
  
  df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
  df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
  df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
  df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))
  
  mod_edge = gam(form_simple, data=df_train)
  scores_edges_simple[i] = rmse( predict(mod_edge, newdata = df_test),  
                          df_test$rateCar)
}
mean(scores_edges_simple)
saveRDS(scores_edges_simple, file="scores_edges_GAM_simple.rds")


# form_simple2 = rateCar ~ hour * rateCar_LaggedHour - hour - rateCar_LaggedHour
# scores_edges_simple2 = c()
# for (i in 1:69){
#   df_train = data_train[[i]]
#   df_test = data_test[[i]]
#   
#   df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
#   df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
#   df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
#   df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))
#   
#   mod_edge = gam(form_simple2, data=df_train)
#   scores_edges_simple2[i] = rmse( predict(mod_edge, newdata = df_test),  
#                                  df_test$rateCar)
# }
# mean(scores_edges_simple2)

# data_train = readRDS(file = "../Data/imp_edges_train.rds")
# data_test = readRDS(file = "../Data/imp_edges_test.rds")

predictionTestData_simple <- matrix(nrow = 17516, ncol = 69)
for (i in 1:69){
  df_train = data_train[[i]]
  df_test = data_test[[i]]
  
  df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
  df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
  df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
  df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))
  
  mod_edge = gam(form_simple, data=df_train)
  predictionTestData_simple[,i] <- predict(mod_edge, newdata = df_test)
}

predictionTestData_full <- matrix(nrow = 17516, ncol = 69)
for (i in 1:69){
  df_train = data_train[[i]]
  df_test = data_test[[i]]
  
  df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
  df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))
  df_train$GRPS = as.factor(sapply(as.factor(df_train$hour), cluster_hours))
  df_test$GRPS = as.factor(sapply(as.factor(df_test$hour), cluster_hours))
  
  mod_edge = gam(form_final, data=df_train)
  predictionTestData_full[,i] <- predict(mod_edge, newdata = df_test)
}

save(predictionTestData_simple, file="GAM_simpleModel_predictionTestData.RData")
save(predictionTestData_full, file="GAM_fullModel_predictionTestData.RData")
