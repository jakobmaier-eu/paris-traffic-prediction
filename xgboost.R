rm(list=objects())

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

# librairies
library(ProjetML1)
require(xgboost)
library(tidyverse)
library(caret)
library(readxl)

# data
data("data_test")
data("data_train")

# We need numerical variables, so we "integer encoding" the days:
daysToNumber <- function(day){
  if(day == "lundi"){return(1)}
  if(day == "mardi"){return(2)}
  if(day == "mercredi"){return(3)}
  if(day == "jeudi"){return(4)}
  if(day == "vendredi"){return(5)}
  if(day == "samedi"){return(6)}
  if(day == "dimanche"){return(7)}
}

data_train[[1]]$weekdays <- unlist(lapply(X = data_train[[1]]$weekdays, FUN = daysToNumber))
data_test[[1]]$weekdays <- unlist(lapply(X = data_test[[1]]$weekdays, FUN = daysToNumber))

# fix seed
set.seed(1)

# convert dataframes to xgb.DMatrix
data_train_ = xgb.DMatrix(as.matrix(data_train[[1]] %>% select(-rateCar)))
rateCar_train = data_train[[1]]$rateCar
data_test_ = xgb.DMatrix(as.matrix(data_test[[1]] %>% select(-rateCar)))
rateCar_test = data_test[[1]]$rateCar

# tuning parameters exclude nrounds
xgb_trcontrol = trainControl(method = "cv", number = 5, allowParallel = TRUE, 
                             verboseIter = TRUE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = 100,  
                       max_depth = c(3, 5, 7, 10),
                       colsample_bytree = seq(0.7, 0.9, length.out = 5),
                       eta = c(0.3,0.1,0.05,0.01),
                       gamma = c(0,0.1,0.2,0.3),
                       min_child_weight = seq(1,10,2),
                       subsample = seq(0.7, 0.9, length.out = 5)
)

ptm <- proc.time()
xgb_model = caret::train(data_train[[1]], data_train[[1]]$rateCar, trControl = xgb_trcontrol, tuneGrid = xgbGrid, method = "xgbTree")
print(proc.time() - ptm) 

xgb_model$bestTune

# tuning nrounds
params <- list(booster = "gbtree",
               eta=0.1, gamma=0.2, max_depth=7, 
               min_child_weight=1, subsample=0.9, 
               colsample_bytree=0.9)

ptm <- proc.time()
xgbcv <- xgb.cv(params = params, 
                data = as.matrix(data_train[[1]] %>% select(-rateCar)), 
                label = data_train[[1]]$rateCar,
                nrounds = 500, nfold = 5, showsd = T, 
                print_every_n = 10, maximize = F,
                metrics = "rmse", early_stopping_rounds=5)
print(proc.time() - ptm) 

# building model
model <- xgboost(data = as.matrix(data_train[[1]] %>% select(-rateCar)), 
                 label = data_train[[1]]$rateCar,
                 nrounds = 133,
                 booster = "gbtree",
                 metrics = "rmse",
                 eta=0.1, gamma=0.2, max_depth=7, 
                 min_child_weight=1, subsample=0.9, 
                 colsample_bytree=0.9,
                 verbose = 2)

pred <- predict(model, as.matrix(data_test[[1]] %>% select(-rateCar)))

rmse(pred,data_test[[1]]$rateCar)

#################
# Prediction of the 69 edges without neighbors
#################

rmse_list = c()
mape_list = c()

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data_tr = data_train[[i]][,c(1:22)]
  data_te = data_test[[i]][,c(1:22)]
  
  data_tr$weekdays <- unlist(lapply(X = data_tr$weekdays, FUN = daysToNumber))
  data_te$weekdays <- unlist(lapply(X = data_te$weekdays, FUN = daysToNumber))
  
  
  model <- xgboost(data = as.matrix(data_tr %>% select(-rateCar)), 
                   label = data_tr$rateCar,
                   nrounds = 133,
                   booster = "gbtree",
                   eta=0.1, gamma=0.2, max_depth=7, 
                   min_child_weight=1, subsample=0.9, 
                   colsample_bytree=0.9,
                   verbose = 0)
  
  prediction <- predict(model, as.matrix(data_te %>% select(-rateCar)))
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

print(proc.time() - ptm)

#################
# Prediction of the 69 edges with neighbors
#################

rmse_list = c()
mape_list = c()

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data_tr = data_train[[i]]
  data_te = data_test[[i]]
  
  data_tr$weekdays <- unlist(lapply(X = data_tr$weekdays, FUN = daysToNumber))
  data_te$weekdays <- unlist(lapply(X = data_te$weekdays, FUN = daysToNumber))
  
  
  model <- xgboost(data = as.matrix(data_tr %>% select(-rateCar)), 
                   label = data_tr$rateCar,
                   nrounds = 133,
                   booster = "gbtree",
                   eta=0.1, gamma=0.2, max_depth=7, 
                   min_child_weight=1, subsample=0.9, 
                   colsample_bytree=0.9,
                   verbose = 0)
  
  prediction <- predict(model, as.matrix(data_te %>% select(-rateCar)))
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

print(proc.time() - ptm)

#################
# Prediction of huge dataframe
#################

data_train <- readRDS("C:/Users/lambe/Documents/Projet_ML/données/train_one_model.rds")
data_test <- readRDS("C:/Users/lambe/Documents/Projet_ML/données/test_one_model.rds")

model <- xgboost(data = as.matrix(data_train %>% select(-nexthour_rateCar)), 
                 label = data_train$nexthour_rateCar,
                 nrounds = 133,
                 booster = "gbtree",
                 metrics = "rmse",
                 eta=0.1, gamma=0.2, max_depth=7, 
                 min_child_weight=1, subsample=0.9, 
                 colsample_bytree=0.9,
                 verbose = 2)
)






