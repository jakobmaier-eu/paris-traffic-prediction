#-------- Start Header for any file -----------#
# First, set working directory to Folder with repo:

rm(list=objects()) # Clean the global environment
library(XML)
library(RCurl)
library(magrittr)
library(mgcv)
library(tidyverse)
library(dplyr)
library(lubridate)
library(weathermetrics)
library(ranger)
library(ggplot2)

library(gbm)
library(xgboost)
library(magrittr)
library(ProjetML1)
#----------- End Header ----------------#



one_df = readRDS("Data/train_one_model.rds")
one_df_test = readRDS("Data/test_one_model.rds")

daysToNumber <- function(day){
  if(day == "lundi"){return(1)}
  if(day == "mardi"){return(2)}
  if(day == "mercredi"){return(3)}
  if(day == "jeudi"){return(4)}
  if(day == "vendredi"){return(5)}
  if(day == "samedi"){return(6)}
  if(day == "dimanche"){return(7)}
}


Ntree=100
eq<-as.formula("nexthour_rateCar~.")

for (i in dim(one_df)[1]){
  one_df$weekdays[i] = daysToNumber(one_df$weekdays[i])
  one_df_test$weekdays[i] = daysToNumber(one_df_test$weekdays[i])
}

one_df$weekdays[i] <- unlist(lapply(X = one_df[1:10^6,]$weekdays, FUN = daysToNumber))
one_df_test$weekdays <- unlist(lapply(X = one_df_test$weekdays, FUN = daysToNumber))

gbm0=gbm(eq,distribution = "gaussian" , data = data_train[[1]], n.trees = Ntree,
         interaction.depth = 2,
         n.minobsinnode = 5, 
         shrinkage = 0.05, 
         bag.fraction = 0.5, 
         train.fraction = 0.9,
         keep.data = FALSE, 
         n.cores = 4)







