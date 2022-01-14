rm(list=objects()) # Clean the global environment

library("ranger")
library("vip")
library("tuneRanger")
library("mlr")
library("caret")

rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=2))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}

set.seed(1)

edges = readRDS("data/imp_edges_train.rds")
edges_test = readRDS("data/imp_edges_test.rds")

res <- ranger(rateCar ~ ., 
                   data=edges[[1]], 
                   importance='impurity')

vip(res)

summary(res)

res$variable.importance[order(res$variable.importance)]

###

data = edges[[1]][,-1]
data_test = edges_test[[1]][,-1]

data$day <- as.factor(data$day)
data$year <- as.factor(data$year)
data$month <- as.factor(data$month)
data$hour <- as.factor(data$hour)

data_test$day <- as.factor(data_test$day)
data_test$year <- as.factor(data_test$year)
data_test$month <- as.factor(data_test$month)
data_test$hour <- as.factor(data_test$hour)

ptm <- proc.time()

grid <-  expand.grid(mtry = seq(2,34,by=2), 
                     min.node.size = seq(5,1000,by=200),
                     splitrule = "variance")

fitControl <- trainControl(method = "timeSlice",
                           initialWindow = 2*365*24,
                           horizon = 1,
                           skip = 6*30*24,
                           fixedWindow = TRUE,
                           verboseIter = TRUE)

fit = caret::train(
  x = data[,-2],
  y = data$rateCar,
  method = 'ranger',
  num.trees = 50,
  tuneGrid = grid,
  trControl = fitControl)

print(fit)
print(proc.time() - ptm) 

#######

model1 <- ranger(rateCar ~ ., 
               data=data, 
               mtry = 7,
               importance='impurity')

prediction <- predict(model1, data=data_test)

vip(model1, num_features = 35)

rmse(prediction$predictions-data_test$rateCar)
mape(prediction$predictions,data_test$rateCar)

plot(1:length(prediction$predictions),prediction$predictions, type = 'l')
plot(1:1000,prediction$predictions[1:1000], type = 'l')

#######
#Data frames without neighbors

data_train_bis = data[,c(1:22)]
data_test_bis = data_test[,c(1:22)]

model2 <- ranger(rateCar ~ ., 
                 data=data_train_bis, 
                 mtry = 7,
                 importance='impurity')

prediction2 <- predict(model2, data=data_test_bis)

vip(model2, num_features = 23)

rmse(prediction2$predictions-data_test_bis$rateCar)
mape(prediction2$predictions,data_test_bis$rateCar)

plot(1:length(prediction2$predictions),prediction2$predictions, type = 'l')
plot(1:1000,prediction2$predictions[1:1000], type = 'l')

#######
rmse_list = c()
mape_list = c()

for(i in 1:69){
  model <- ranger(rateCar ~ ., 
                   data=data_train, 
                   importance='impurity')
  
  prediction <- predict(model, data=data_test)
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions-data_test$rateCar))
  mape_list <- c(mape_list, rmse(prediction$predictions, data_test$rateCar))
}

#######

# edges = readRDS("data/imp_edges_train.rds")
# data = edges[[1]]
# 
# set.seed(1)
# 
# control <- trainControl(method="repeatedcv", 
#                         number=3, 
#                         repeats=1, 
#                         search="random",
#                         verboseIter = TRUE)
# 
# rf_random <- train(rateCar~., 
#                    data=data, 
#                    method="ranger", 
#                    tuneLength=5, 
#                    trControl=control,
#                    num.trees = 100,
#                    splitrule = "variance")
# 
# print(rf_random)




