rm(list=objects())

# Librairies
library(ProjetML1)
library(rpart)
library(caret)
library(UsingR)

# Data
data("data_test")
data("data_train")

# Equation of the tree
eq <- rateCar ~.

# Fix seed
set.seed(1)

####################
#Tuning of cp parameter
####################

# timeSlice method
train_control <- trainControl(method = "timeSlice",
                           initialWindow = 2*365*24,
                           horizon = 1,
                           skip = 2*30*24,
                           fixedWindow = TRUE,
                           verboseIter = TRUE)

# cv method
train_control <- trainControl(method="cv", number=16,verboseIter = TRUE)

# First grid : from 0.0 to 0.1
cpGrid1 <- expand.grid(cp=seq(0,0.1,length=10))

# Second grid : from 0.00 to 0.01
cpGrid2 <- expand.grid(cp=seq(0,0.01,length=10))

# Training
rpart.CV.1 <- caret::train(eq, data = data_train[[1]], method ="rpart",  
                trControl=train_control, metric="RMSE",tuneGrid = cpGrid1,minsplit=20)

rpart.CV.2 <- caret::train(eq, data = data_train[[1]], method ="rpart",  
                  trControl=train_control, metric="RMSE",tuneGrid = cpGrid2,minsplit=20)

# Fitting the tree with optimal cp value
rpart1 <- rpart(eq , data = data_train[[1]], method = "anova",
               control = rpart.control(minsplit = 20,
                                       cp = cpGrid1[which.min(rpart.CV.2$results$RMSE),]))

# Prediction
rpart1.forecast <- predict(rpart1,newdata=data_test[[1]])

# Rmse error
rmse(data_test[[1]]$rateCar, rpart1.forecast)

# Plotting results
par(mfrow=c(1,2))

p1 <- plot(rpart.CV.1, main = "Grille : de 0.0 à 0.1", xlab = "Paramètre cp", ylab="RMSE")
p2 <- plot(rpart.CV.2, main = "Grille : de 0.00 à 0.01", xlab = "Paramètre cp", ylab="RMSE")

print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))

#################
# Prediction of the 69 edges without neighbors
#################

rmse_list = c()
mape_list = c()

predictionTrainData <- matrix(nrow = 35060, ncol = 69)
predictionTestData <- matrix(nrow = 17516, ncol = 69)

ptm <- proc.time()

for(i in 1:69){
  print(i) # to check the progression of the loop
  
  data_tr = data_train[[i]][,c(1:22)]
  data_te = data_test[[i]][,c(1:22)]
  
  model <- rpart(eq , data = data_tr, method = "anova",
                 control = rpart.control(minsplit = 20,
                                         cp = cpGrid2[which.min(rpart.CV.2$results$RMSE),]))
  
  prediction <- predict(model, newdata=data_te)
  
  predictionTestData[,i] <- prediction$predictions
  predictionTrainData[,i] <- model$predictions
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

print(proc.time() - ptm) 

save(predictionTestData, file="CART_without_predictionTestData.RData")


#################
# Prediction of the 69 edges with neighbors
#################

rmse_list = c()
mape_list = c()

predictionTrainData <- matrix(nrow = 35060, ncol = 69)
predictionTestData <- matrix(nrow = 17516, ncol = 69)

ptm <- proc.time()

for(i in 1:69){
  print(i) # to check the progression of the loop
  
  data_tr = data_train[[i]]
  data_te = data_test[[i]]
  
  model <- rpart(eq , data = data_tr, method = "anova",
                 control = rpart.control(minsplit = 20,
                                         cp = cpGrid2[which.min(rpart.CV.2$results$RMSE),]))
  
  prediction <- predict(model, newdata=data_te)
  
  predictionTestData[,i] <- prediction$predictions
  predictionTrainData[,i] <- model$predictions
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}
save(predictionTestData, file="CART_with_predictionTestData.RData")


print(proc.time() - ptm) 


