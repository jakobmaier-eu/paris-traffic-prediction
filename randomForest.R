rm(list=objects()) # Clean the global environment

# Librairies
library("ranger")
library("vip")
library("mlr")
library("caret")
library(ProjetML1)

# Fix seed
set.seed(1)

# Data
data("data_train")
data("data_test")

############################################
##### TUNING
############################################

# Data
data_train = data_train[[1]]
data_test = data_test[[1]]

# Convert categorical variables into numeric
data_train$day <- as.factor(data_train$day)
data_train$year <- as.factor(data_train$year)
data_train$month <- as.factor(data_train$month)
data_train$hour <- as.factor(data_train$hour)

data_test$day <- as.factor(data_test$day)
data_test$year <- as.factor(data_test$year)
data_test$month <- as.factor(data_test$month)
data_test$hour <- as.factor(data_test$hour)

# Grids
grid_mtry <-  expand.grid(mtry = seq(1,34,by=1), 
                     min.node.size = 5,
                     splitrule = "variance")

grid_minnode <-  expand.grid(mtry = 10, 
                          min.node.size = seq(1, 15, by = 1),
                          splitrule = "variance")

# timeSlice method
fitControl <- trainControl(method = "timeSlice",
                           initialWindow = 2*365*24,
                           horizon = 1,
                           skip = 3*30*24,
                           fixedWindow = TRUE,
                           verboseIter = TRUE)

# cv method
fitControl <- trainControl(method="cv", number=16,
                           verboseIter = TRUE)

# Training
ptm <- proc.time()

fit = caret::train(
  x = data_train,
  y = data_train$rateCar,
  method = 'ranger',
  num.trees = 50,
  tuneGrid = grid_minnode, # change the grid here
  trControl = fitControl)

print(fit)
print(proc.time() - ptm) 

# Plotting results
par(mfrow=c(1,2))

p1 <- plot(fit1, main = "Grille : de 1 à 34, de pas 1 \n Temps d'exécution : 22 minutes \n Méthode : validation croisé", xlab = "Paramètre mtry", ylab="RMSE")
p2 <- plot(fit2, main = "Grille : de 1 à 34, de pas 1 \n Temps d'exécution : 80 minutes \n Méthode : timeslice", xlab = "Paramètre mtry", ylab="RMSE")

print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))

############################################
############################################
############################################

model1 <- ranger(rateCar~., 
               data=data_train, 
               min.node.size = 1,
               mtry = 10,
               importance='impurity')

prediction <- predict(model1, data=data_test)

vip(model1, num_features = 35)

rmse(prediction$predictions,data_test$rateCar)
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
  
  data_tr = data_train[[i]][,c(1:22)]
  data_te = data_test[[i]][,c(1:22)]
  
  data$day <- as.factor(data$day)
  data$year <- as.factor(data$year)
  data$month <- as.factor(data$month)
  data$hour <- as.factor(data$hour)

  data_test$day <- as.factor(data_test$day)
  data_test$year <- as.factor(data_test$year)
  data_test$month <- as.factor(data_test$month)
  data_test$hour <- as.factor(data_test$hour)
  
  model <- ranger(rateCar ~ ., 
                   data=data_tr, 
                  num.trees = 100, 
                  mtry = 10,
                   importance='impurity')
  
  prediction <- predict(model, data=data_te)
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_te$rateCar))
}

print(proc.time() - ptm) 

#######

K = 16
nb_tree = 50
params = 1:10
data = data_train[[1]]
folds <- cut(seq(from=1,to=nrow(data)),breaks=K,labels=FALSE)
scores = rep(x = 0, length(params))
start_time = Sys.time()
for(i in 1:K){
  print(paste("fold:", i))
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  for(j in 1:length(params)){
    model <- ranger(rateCar ~ ., 
                    data=data, 
                    num.trees = nb_tree, 
                    mtry = params[j],
                    importance='impurity')
    Y_test = testData$rateCar
    Y_predict = predict(model, data=subset(testData, select = -c(1)))
    scores[j] <- scores[j] + ProjetML1::rmse(Y_test, Y_predict$predictions)
  }
}
CV_exec_time = Sys.time()-start_time

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

ntree_list <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120,
                130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230,
                240, 250, 260, 270, 280, 290, 300)

time_list <- c()

for(n in ntree_list){
  print(n)
  
  ptm <- proc.time()
  
  res <- ranger(rateCar ~., 
                data=data_train[[1]], 
                num.trees = n,
                mtry=10,
                importance='impurity')
  
  temp <- proc.time() - ptm
  
  time_list <- c(time_list, temp[3])
  
  error_list <- c(error_list, res$prediction.error)
}

plot(ntree_list, error_list)
plot(ntree_list, time_list)





ptm <- proc.time()

test <- proc.time() - ptm




###############################################
##### TEST
###############################################

res <- ranger(rateCar ~ ., 
              data=data_train[[1]], 
              importance='impurity')

vip(res)

summary(res)

res$variable.importance[order(res$variable.importance)]

############################################
############################################
############################################






############################################
# Number of trees
############################################

# Errors and times list
error_list <- c()
time_list <- c()

# Number of trees tested
ntree_list <- seq(10,300,10)

# For each number of trees...
for(n in ntree_list){
  print(n) # to check the loop
  
  ptm <- proc.time() # evaluate execution time
  
  # Grow a forest with n trees
  res <- ranger(rateCar ~., 
                data=data_train, 
                num.trees = n,
                mtry=10,
                importance='impurity')
  
  temp <- proc.time() - ptm
  
  time_list <- c(time_list, temp[3])
  
  error_list <- c(error_list, res$prediction.error)
}

# plotting results
par(mfrow=c(1,2))

p1 <- plot(ntree_list, error_list, 
           main = "Erreur RMSE selon le nombre d'arbres", 
           xlab = "Nombre d'arbres ntree", 
           ylab="RMSE",
           type = "o",
           pch=18,
           col="blue")
p2 <- plot(ntree_list, time_list, 
           main = "Temps d'exécution selon le nombre d'arbres", 
           xlab = "Nombre d'arbres ntree", 
           ylab="Temps d'exécution (secondes)",
           type="o",
           col="blue")

print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))

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
  
  model <- ranger(rateCar ~ ., 
                  data=data_tr, 
                  num.trees = 150, 
                  mtry = 10,
                  importance='impurity')
  
  prediction <- predict(model, data=data_te)
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_te$rateCar))
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
  
  model <- ranger(rateCar ~ ., 
                  data=data_tr, 
                  num.trees = 150, 
                  mtry = 10,
                  importance='impurity')
  
  prediction <- predict(model, data=data_te)
  
  rmse_list <- c(rmse_list, rmse(prediction$predictions, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction$predictions, data_te$rateCar))
}

print(proc.time() - ptm) 

################
# Visualizing importance
################

model <- ranger(rateCar~., 
                data=data_train[[1]],
                mtry = 10,
                importance='impurity')

prediction <- predict(model, data=data_test[[1]])

vip(model, num_features = 15)









