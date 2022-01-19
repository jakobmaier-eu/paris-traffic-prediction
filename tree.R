rm(list=objects())

library(ProjetML1)
library(rpart)
library(caret)

data("data_test")
data("data_train")

eq <- rateCar ~.


train_control <- trainControl(method = "timeSlice",
                           initialWindow = 2*365*24,
                           horizon = 1,
                           skip = 6*30*24,
                           fixedWindow = TRUE,
                           verboseIter = TRUE)

train_control <- trainControl(method="repeatedcv", number=10,repeats = 10)
cpGrid <- expand.grid(cp=seq(0,0.1,length=10))
set.seed(1)
rpart.CV<-train(eq, data = data_train[[1]], method ="rpart",  
                trControl=train_control, metric="RMSE",tuneGrid = cpGrid,minsplit=2)
plot(seq(0,0.1,length=10),rpart.CV$results$RMSE)

rpart1<- rpart(eq , data = data_train[[1]], method = "anova",
               control = rpart.control(minsplit = 2,
                                       cp = cpGrid[which.min(rpart.CV$results$RMSE),]))

rpart1.forecast<-predict(rpart1,newdata=data_test[[1]])

rmse(data_test[[1]]$rateCar, rpart1.forecast)



#################

rmse_list = c()
mape_list = c()

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data_tr = data_train[[i]][,-1][,c(1:22)]
  data_te = data_test[[i]][,-1][,c(1:22)]
  
  # data_tr$day <- as.factor(data_tr$day)
  # data_tr$year <- as.factor(data_tr$year)
  # data_tr$month <- as.factor(data_tr$month)
  # data_tr$hour <- as.factor(data_tr$hour)
  # 
  # data_te$day <- as.factor(data_te$day)
  # data_te$year <- as.factor(data_te$year)
  # data_te$month <- as.factor(data_te$month)
  # data_te$hour <- as.factor(data_te$hour)
  
  model <- rpart(eq , data = data_tr, method = "anova",
                 control = rpart.control(minsplit = 2,
                                         cp = cpGrid[which.min(rpart.CV$results$RMSE),]))
  
  prediction <- predict(model, newdata=data_te)
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

print(proc.time() - ptm) 


