rm(list=objects())

#Libraries
library(lubridate)
library(dplyr)
library(ProjetML1)

#Data sets
data("data_train_day")
data("data_test_day")
data("data_test")

#Naive functions
F1 <- function(nb, h, d, m){
  return(mean((data_train_day[[nb]] %>% filter(hour == h, day == d, month == m))$rateCar))
}

F2 <- function(nb, h, d, m){
  return(median((data_train_day[[nb]] %>% filter(hour == h, day == d, month == m))$rateCar))
}

#Naive Model
naiveModel <- function(FUN){
  pred_list = vector("list", length=69)
  for(i in 1:69){
    pred_elt <- c()
    
    # print(i) to check

    for(j in 1:dim(data_test_day[[1]])[1]){
      temp <- FUN(i, 
                  data_test_day[[1]]$hour[j],
                  data_test_day[[1]]$day[j],
                  data_test_day[[1]]$month[j])
      pred_elt <- c(pred_elt, temp)
    }

    pred_list[[i]] <- pred_elt
  }

  return(pred_list)
}

#Naive predictions (1 and 2)
naivePrediction1 <- naiveModel(F1)
naivePrediction2 <- naiveModel(F2)

# saveRDS("data/naivePrediction1_day.rds", object = naivePrediction1)
# saveRDS("data/naivePrediction2_day.rds", object = naivePrediction2)

# Naive Model bis
naiveModelBis <- function(){
  pred_list = vector("list", length=69)
  for(i in 1:69){
    pred_list[[i]] <- data_test_day[[i]]$rateCar_LaggedDay
  }
  return(pred_list)
}

# Naive Prediction (3)
naivePrediction3 <- naiveModelBis()
# saveRDS("data/naivePrediction3_day.rds", object = naivePrediction3)

############################################
############################################
############################################

# Load naive predictions
pred1 = readRDS("data/naivePrediction1_day.rds")
pred2 = readRDS("data/naivePrediction2_day.rds")
pred3 = readRDS("data/naivePrediction3_day.rds")

# rmse and mape 
rmse_list_naivePrediction3 = c()
mape_list_naivePrediction3 = c()

for(i in 1:69){
  rmse_list_naivePrediction3 <- c(rmse_list_naivePrediction3, rmse(pred3[[i]], data_test[[i]]$rateCar))
  mape_list_naivePrediction3 <- c(mape_list_naivePrediction3, mape(pred3[[i]], data_test[[i]]$rateCar))
}

# save(rmse_list_naivePrediction2, mape_list_naivePrediction2, file = "naivePrediction2_day.RData")










