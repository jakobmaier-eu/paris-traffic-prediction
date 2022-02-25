rm(list=objects()) # Clean the global environment

# Librairies
library("ranger")
library("vip")
library("mlr")
library("caret")
library(ProjetML1)
library("stats")
library("forecast")
library("portes")
library("astsa")

# Fix seed
set.seed(1)

# Data
data("data_train")
data("data_test")

#################
# Prediction of the 69 edges without neighbors
#################

rmse_list = c()
mape_list = c()

predictionTrainData <- matrix(nrow = 35060, ncol = 69)
predictionTestData <- matrix(nrow = 17516, ncol = 69)

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data_tr = data_train[[i]][,c(1:22)]
  data_te = data_test[[i]][,c(1:22)]
  
  model <- ranger(rateCar ~ ., 
                  data=data_tr, 
                  num.trees = 150, 
                  mtry = 10,
                  min.node.size = 9)
  
  prediction <- predict(model, data=data_te)
  
  predictionTestData[,i] <- prediction$predictions
  predictionTrainData[,i] <- model$predictions
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_te$rateCar))
}

print(proc.time() - ptm) 

#################
# Prediction of the 69 edges with neighbors
#################

rmse_list = c()
mape_list = c()

predictionTrainData <- matrix(nrow = 35060, ncol = 69)
predictionTestData <- matrix(nrow = 17516, ncol = 69)

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data_tr = data_train[[i]]
  data_te = data_test[[i]]
  
  model <- ranger(rateCar ~ ., 
                  data=data_tr, 
                  num.trees = 150, 
                  mtry = 10,
                  importance='impurity')
  
  prediction <- predict(model, data=data_te)
  
  predictionTestData[,i] <- prediction$predictions
  predictionTrainData[,i] <- model$predictions
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_te$rateCar))
}

print(proc.time() - ptm) 

################
# Analysis of residuals
################

d <- dim(predictionTrainData)

ts1 <- ts(predictionTrainData[,1][(d[1]-24*150):d[1]], frequency = 24)
plot(ts1)

acf(ts1)
pacf(ts1)

acf(diff(ts1,lag = 24),lag.max=20) #qmax = 16
pacf(diff(ts1,lag = 24),lag.max = 15) #pmax = 10

acf(diff(ts1,lag=24),lag.max = 24*20) #qmax = 1
pacf(diff(ts1,lag=24),lag.max = 24*17) #pmax = 15

test <- arima(x = ts1,order = c(9,0,0),seasonal = c(5,1,0))

acf(diff(diff(diff(ts1,differences = 1, lag = 24)),lag=24*7),lag.max = 24*20)

pacf(diff(ts1,differences = 1, lag = 24),lag.max = 24)

test <- auto.arima(y=ts1,d=0,start.p = 9,D = 1,max.q = 16,max.p = 10,max.Q = 2,max.P = 15,stepwise = T,approximation = T,seasonal = T)

BoxPierce(test$residuals,lags = c(1,24))
