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
data("data_train_day")
data("data_test_day")

############################################
# TUNING
############################################

# Data
data_train_day = data_train_day[[1]]
data_test_day = data_test_day[[1]]

# Convert categorical variables into numeric
data_train_day$day <- as.factor(data_train_day$day)
data_train_day$year <- as.factor(data_train_day$year)
data_train_day$month <- as.factor(data_train_day$month)
data_train_day$hour <- as.factor(data_train_day$hour)

data_test_day$day <- as.factor(data_test_day$day)
data_test_day$year <- as.factor(data_test_day$year)
data_test_day$month <- as.factor(data_test_day$month)
data_test_day$hour <- as.factor(data_test_day$hour)

# Grids
grid_mtry <-  expand.grid(mtry = seq(1,29,by=1), 
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
  x = data_train_day,
  y = data_train_day$rateCar,
  method = 'ranger',
  num.trees = 50,
  tuneGrid = grid_mtry, # change the grid here
  trControl = fitControl)

print(fit)
print(proc.time() - ptm) 

# Plotting results
par(mfrow=c(1,2))

p1 <- plot(fit1, main = "Grille : de 1 à 29, de pas 1 \n Temps d'exécution : 22 minutes \n Méthode : validation croisé", xlab = "Paramètre mtry", ylab="RMSE")
p2 <- plot(fit2, main = "Grille : de 1 à 29, de pas 1 \n Temps d'exécution : 80 minutes \n Méthode : timeslice", xlab = "Paramètre mtry", ylab="RMSE")

print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))

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
                data=data_train_day, 
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
  
  data_tr = data_train_day[[i]][,c(1:22)]
  data_te = data_test_day[[i]][,c(1:22)]
  
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
  
  data_tr = data_train_day[[i]]
  data_te = data_test_day[[i]]
  
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
                 data=data_train_day[[1]],
                 mtry = 10,
                 importance='impurity')

prediction <- predict(model, data=data_test_day[[1]])

vip(model, num_features = 15)


