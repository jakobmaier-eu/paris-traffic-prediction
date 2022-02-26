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

##########################
# Functions from the course
##########################
rollArima<-function (arima.model, ynew, horizon = 1) 
{
  t <- 24
  prevARIMA <- array(0, dim = t)
  prevARIMA[1] <- forecast(arima.model, h = horizon)$mean[horizon]
  for (i in 1:(t - 1)) {
    print(i)
    ts2 <- c(predictionTrainData[,1], ynew[1:i])
    refit <- Arima(y=ts2, model = arima.model)
    prevARIMA[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevARIMA)
}

rollHW <- function (hw.model, ynew, horizon = 1) 
{
  t <- 24*30
  prevHW <- array(0, dim = t)
  prevHW[1] <- forecast(hw.model, h = horizon)$mean[horizon]
  refit <- hw.model
  for (i in 1:(t - 1)) {
    print(i)
    if(hw.model$gamma==T)
    {
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2, seasonal=hw.model$seasonal,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , s.start=refit$coefficients[grep("s",names(fit.hw$coefficients))]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = refit$gamma))
    }
    
    else{
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = F))
    }
    prevHW[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevHW)
}

rollETS <- function (ets.model, ynew, horizon = 1) 
{
  t<-24
  prevETS <- array(0, dim = t)
  prevETS[1] <- forecast(ets.model, h = horizon)$mean[horizon]
  for (i in 1:(t - 1)) {
    print(i)
    ts2 <- ts(c(ets.model$x, ynew[1:i]), frequency=frequency(ets.model$x))
    refit <- ets(ts2, model = ets.model)
    prevETS[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevETS)
}

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

load("1_heure/randomForest_with_predictionTrainData.RData")
load("1_heure/randomForest_with_predictionTestData.RData")

d <- dim(predictionTrainData)
d2 <- dim(predictionTestData)

# Convert to ts object
ts1 <- ts(predictionTrainData[,1], frequency = 24)

# SARIMA

# Plot of the ts
plot(ts1)

# acf and pacf
acf(ts1)
pacf(ts1)

# acf and pacf of the differentiated ts
p1 <- acf(diff(ts1,lag = 24),lag.max=20,plot = F) #qmax = 16
p2<-pacf(diff(ts1,lag = 24),lag.max = 15,plot=F) #pmax = 10

p3<- acf(diff(ts1,lag=24),lag.max = 24*20,plot=F) #Qmax = 1
p4<-pacf(diff(ts1,lag=24),lag.max = 24*17,plot=F) #Pmax = 15

par(mfrow=c(2,2))
plot(p1)
plot(p2)
plot(p3)
plot(p4)

# Search of optimal paramters with auto.arima
sarimaOpt <- auto.arima(y=ts1,d=0,start.p = 9,D = 1,max.q = 1,max.p = 10,max.Q = 1,max.P = 15,stepwise = T,approximation = T,seasonal = T)

# Defining sarima with our chosen parameters
test <- arima(x = ts1,order = c(9,0,0),method = "CSS",seasonal = c(4,1,0))

# Box-Pierce/Portemanteau test
BoxPierce(test$residuals,lags = c(1,24))

# Huge forecast -> useless
testForecast <- forecast(test,h=17516)

# roll prediction for the first day
rollForecast <- rollArima(test,ynew=predictionTestData[,1],horizon=1)

# New prediction
rfSarima <- rollForecast + predictionTestData[,1][1:24]

# rmse error associated -> 8.7
rmse(rfSarima,data_test[[1]]$rateCar[1:24])

# HOLT-WINTERS

# optimization of HW
fit.hw<-HoltWinters(x=ts1,)

# roll prediction
prevHW<-rollHW(fit.hw,ynew=predictionTestData[,1],horizon=1)

# New prediction
rfHW <- prevHW + predictionTestData[,1][1:24*30]

# rmse error associated -> 10.4
rmse(rfHW,data_test[[1]]$rateCar[1:24*30])

# ETS

# optimization of ets
fit.ets<-ets(ts1)

# roll prediction
prevETS<-rollETS(fit.ets,ynew=predictionTestData[,1],horizon=1)

# New prediction
rfETS <- prevETS + predictionTestData[,1][1:24]

# rmse error associated -> 5.87
rmse(rfETS,data_test[[1]]$rateCar[1:24])
