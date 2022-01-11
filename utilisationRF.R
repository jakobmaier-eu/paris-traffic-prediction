rm(list=objects()) # Clean the global environment

#devtools::install_github("PhilippPro/tuneRanger")

library("ranger")
library("vip")
library("tuneRanger")
library("mlr")
library("caret")

set.seed(1)

edges = readRDS("data/imp_edges_train.rds")

res <- ranger(rateCar ~ ., 
                   data=edges[[1]], 
                   importance='impurity')

vip(res)

summary(res)

res$variable.importance[order(res$variable.importance)]

###

data = edges[[1]]

data$day <- as.factor(data$day)
data$year <- as.factor(data$year)
data$month <- as.factor(data$month)
data$hour <- as.factor(data$hour)

ptm <- proc.time()

grid <-  expand.grid(mtry = seq(2,34,by=2), 
                     min.node.size = seq(2,8,by=2),
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

test1 <- ranger(rateCar ~ ., 
               data=edges[[1]], 
               importance='impurity')

test2 <- ranger(rateCar ~ ., 
                data=edges[[1]], 
                importance='impurity',
                min.node.size = 15)

test3 <- ranger(rateCar ~ ., 
                data=edges[[1]], 
                importance='impurity',
                min.node.size = 50)


#######

edges = readRDS("data/imp_edges_train.rds")
data = edges[[1]]

set.seed(1)

control <- trainControl(method="repeatedcv", 
                        number=3, 
                        repeats=1, 
                        search="random",
                        verboseIter = TRUE)

rf_random <- train(rateCar~., 
                   data=data, 
                   method="ranger", 
                   tuneLength=5, 
                   trControl=control,
                   num.trees = 100,
                   splitrule = "variance")

print(rf_random)




