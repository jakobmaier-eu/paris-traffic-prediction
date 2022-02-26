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
library(catboost)
library(caret)

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

# tuning parameters exclude nrounds
xgb_trcontrol = trainControl(method = "cv", number = 16, allowParallel = TRUE, 
                             verboseIter = TRUE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = 100,  
                       max_depth = c(3, 5, 7, 10),
                       colsample_bytree = seq(0.7, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma = 0.1,
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
predictionTestData <- matrix(nrow = 17516, ncol = 69)
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
  predictionTestData[,i] <- prediction
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}
save(predictionTestData, file="XGBoost_without_predictionTestData.RData")

print(proc.time() - ptm)

#################
# Prediction of the 69 edges with neighbors
#################

rmse_list = c()
mape_list = c()
predictionTestData <- matrix(nrow = 17516, ncol = 69)


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
  predictionTestData[,i] <- prediction
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

save(predictionTestData, file="XGBoost_with_predictionTestData.RData")

print(proc.time() - ptm)

#################
# Prediction of huge dataframe
#################

onedf_train <- subset(readRDS("Data/train_one_model.rds"), 
                      select=c(-year, -month, -hour, -day, -covidIndex, -precipitation, -temperature))
names(onedf_train)
garder = c()
for (i in 1:69){
  if(i%%3 != 0){
    next
  }
  garder = c(garder, unique(onedf_train$edgename)[i])
}
onedf_train_small = onedf_train %>% filter(edgename %in% garder)
saveRDS(onedf_train_small, file="Data/train_one_small.rds")

onedf_test <- subset(readRDS("Data/test_one_model.rds"), 
    select=c(-year, -month, -hour, -day, -covidIndex, -precipitation, -temperature))
garder = unique(onesmall_train$edgename)
onedf_test_small = onedf_test %>% filter(edgename %in% garder)
saveRDS(onedf_test_small, file="Data/test_one_small.rds")


onesmall_train = readRDS("/Data/train_one_small.rds")
onesmall_test = readRDS("Data/test_one_small.rds")


for (catvar in c('edgename', 'position', 'weekdays')){
    onesmall_train[[catvar]] = as.factor(onesmall_train[[catvar]])
}
for (catvar in c('edgename', 'position', 'weekdays')){
  onesmall_test[[catvar]] = as.factor(onesmall_test[[catvar]])
}

train_pool <- catboost.load_pool(
  (onesmall_train %>% select(-nexthour_rateCar)), 
  label = onesmall_train$nexthour_rateCar
  )
test_pool <- catboost.load_pool(
  onesmall_test %>% select(-nexthour_rateCar), 
  label = onesmall_test$nexthour_rateCar
)
fit_params <- list(
  iterations = 10000,
  loss_function = 'RMSE',
  border_count = 256,
  depth = 2,
  learning_rate = 0.001,
  l2_leaf_reg = 0,
  train_dir = 'train_dir')

model <- catboost.train(train_pool, params=fit_params)
Ypredict <- catboost.predict(model, pool=test_pool)

# install.packages("C:/Users/jakob/Dropbox/Uni/1_ProjetML/package/ProjetML1_0.0.tar.gz", repos = NULL, type="source")
edge_scores = data.frame(edge_name=unique(onesmall_test$edgename))
i = 1
for (edge in unique(onesmall_test$edgename)){
  break
  testdata_edge = onesmall_test%>% filter(edgename==edge)
  test_pool_edge <- catboost.load_pool(
    testdata_edge%>% select(-nexthour_rateCar), 
    label = testdata_edge$nexthour_rateCar
  )
  Ypred_edge = catboost.predict(model, pool = test_pool_edge)
  edge_scores$test_score[i] = rmse(Ypred_edge, testdata_edge$nexthour_rateCar)
  i = i + 1
}




saveRDS(model, file="1_heure/catboost_model.rds")
saveRDS(edge_scores, file = "../1_heure/catboost_scores.rds")

modalt = readRDS("1_heure/catboost_model.rds")
# Ypred_edge = catboost.predict(modalt, pool = test_pool_edge)
# rmse(Ypred_edge, testdata_edge$nexthour_rateCar)



# ###### LGBM doesn't work because it needs too much RAM.
# library(lightgbm)
# lgbm_rounds = 50
# lightgbm(data = as.matrix(onesmall_train %>% select(-nexthour_rateCar)),
#          label = onesmall_train$nexthour_rateCar,
#          nrounds = lgbm_rounds,
#          verbose =  0,
#          params = list(
#            early_stopping_round = lgbm_rounds/10,
#            lambda_l1 = 0.1,
#            lambda_l2 = 0.1,
#            num_leaves = 31,
#            max_depth = -1,
#            bagging_fraction = 1,
#            feature_fraction = 1,
#            max_bin = 255
#            ),
#          obj = regression
#          )

# ### XGBoost doesn't work with categorical variables !
# xgb_mod1 <- xgboost(data = as.matrix(onedf_train_small %>% select(-nexthour_rateCar)), 
#                  label = onedf_train_small$nexthour_rateCar,
#                  nrounds = 133,
#                  booster = "gbtree",
#                  metrics = "rmse",
#                  eta=0.1, gamma=0.2, max_depth=7, 
#                  min_child_weight=1, subsample=0.9, 
#                  colsample_bytree=0.9,
#                  verbose = 2)







