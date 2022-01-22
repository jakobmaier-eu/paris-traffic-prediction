rm(list=objects()) # Clean the global environment

# Librairies
library("ranger")
library("vip")
library("tuneRanger")
library("mlr")
library("caret")
library(ProjetML1)

# Fix seed
set.seed(1)

# Data
data("data_train")
data("data_test")

###############################################
##### TEST
###############################################

res <- ranger(rateCar ~ ., 
                   data=edges[[1]], 
                   importance='impurity')

vip(res)

summary(res)

res$variable.importance[order(res$variable.importance)]

############################################
############################################
############################################

############################################
##### TUNING
############################################

data = edges[[1]]
data_test = edges_test[[1]]

data$day <- as.factor(data$day)
data$year <- as.factor(data$year)
data$month <- as.factor(data$month)
data$hour <- as.factor(data$hour)

data_test$day <- as.factor(data_test$day)
data_test$year <- as.factor(data_test$year)
data_test$month <- as.factor(data_test$month)
data_test$hour <- as.factor(data_test$hour)

ptm <- proc.time()

grid <-  expand.grid(mtry = 5, 
                     min.node.size = seq(1,50,by=1),
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
  num.trees = 100,
  tuneGrid = grid,
  trControl = fitControl)

print(fit)
print(proc.time() - ptm) 

############################################
############################################
############################################

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

ptm <- proc.time()

for(i in 1:69){
  print(i)
  
  data = edges[[i]][,-1][,c(1:22)]
  data_test = edges_test[[i]][,-1][,c(1:22)]
  
  data$day <- as.factor(data$day)
  data$year <- as.factor(data$year)
  data$month <- as.factor(data$month)
  data$hour <- as.factor(data$hour)

  data_test$day <- as.factor(data_test$day)
  data_test$year <- as.factor(data_test$year)
  data_test$month <- as.factor(data_test$month)
  data_test$hour <- as.factor(data_test$hour)
  
  model <- ranger(rateCar ~ ., 
                   data=data, 
                  num.trees = 300,
                   importance='impurity')
  
  prediction <- predict(model, data=data_test)
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_test$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_test$rateCar))
}

print(proc.time() - ptm) 

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

##############################################
##############################################
##############################################

error_list <- c()

ntree_list <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

time_list <- c()

for(n in ntree_list){
  print(n)
  
  ptm <- proc.time()
  
  res <- ranger(rateCar ~ ., 
                data=edges[[1]], 
                num.trees = n,
                importance='impurity')
  
  temp <- proc.time() - ptm
  
  time_list <- c(time_list, temp[3])
  
  error_list <- c(error_list, res$prediction.error)
}

plot(ntree_list, error_list)
plot(ntree_list, time_list)





ptm <- proc.time()

test <- proc.time() - ptm











