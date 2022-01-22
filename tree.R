rm(list=objects())

# Librairies
library(ProjetML1)
library(rpart)
library(caret)

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
train_control <- trainControl(method="cv", number=16)

# First grid : from 0.0 to 0.1
cpGrid1 <- expand.grid(cp=seq(0,0.1,length=10))

# Second grid : from 0.00 to 0.01
cpGrid2 <- expand.grid(cp=seq(0,0.01,length=10))

# Training
rpart.CV <- train(eq, data = data_train[[1]], method ="rpart",  
                trControl=train_control, metric="RMSE",tuneGrid = cpGrid1,minsplit=2)

# Plotting results
plot(rpart.CV, main = "Recherche du paramètre cp")

# Fitting the tree with optimal cp value
rpart1 <- rpart(eq , data = data_train[[1]], method = "anova",
               control = rpart.control(minsplit = 2,
                                       cp = cpGrid[which.min(rpart.CV$results$RMSE),]))

# Prediction
rpart1.forecast <- predict(rpart1,newdata=data_test[[1]])

# Rmse error
rmse(data_test[[1]]$rateCar, rpart1.forecast)



#################
# Prediction of the 69 edges without neighbors
#################

rmse_list = c()
mape_list = c()

ptm <- proc.time()

for(i in 1:69){
  print(i) # to check the progression of the loop
  
  data_tr = data_train[[i]][,c(1:22)]
  data_te = data_test[[i]][,c(1:22)]
  
  model <- rpart(eq , data = data_tr, method = "anova",
                 control = rpart.control(minsplit = 2,
                                         cp = cpGrid[which.min(rpart.CV$results$RMSE),]))
  
  prediction <- predict(model, newdata=data_te)
  
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
  print(i) # to check the progression of the loop
  
  data_tr = data_train[[i]]
  data_te = data_test[[i]]
  
  model <- rpart(eq , data = data_tr, method = "anova",
                 control = rpart.control(minsplit = 2,
                                         cp = cpGrid[which.min(rpart.CV$results$RMSE),]))
  
  prediction <- predict(model, newdata=data_te)
  
  rmse_list <- c(rmse_list, rmse(prediction, data_te$rateCar))
  mape_list <- c(mape_list, mape(prediction, data_te$rateCar))
}

print(proc.time() - ptm) 


