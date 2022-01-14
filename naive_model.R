rm(list=objects())

library(lubridate)
library(dplyr)

data_train = readRDS("data/imp_edges_train.rds")
data_test <- readRDS("data/imp_edges_test.rds")

F1 <- function(nb, h){
  return(mean((data_train[[nb]] %>% filter(hour == h))$rateCar))
}

F2 <- function(nb, h){
  return(median((data_train[[nb]] %>% filter(hour == h))$rateCar))
}

F3 <- function(nb, h, d, m){
  return(mean((data_train[[nb]] %>% filter(hour == h, day == d, month == m))$rateCar))
}

F4 <- function(nb, h, d, m){
  return(median((data_train[[nb]] %>% filter(hour == h, day == d, month == m))$rateCar))
}

# naiveModel <- function(FUN){
#   pred_list = vector("list", length=69)
#   for(i in 1:69){
#     pred_elt <- c()
#     
#     for(j in 0:23){
#       temp <- FUN(i, j)
#       pred_elt <- c(pred_elt, temp)
#     }
#     
#     pred_list[[i]] <- pred_elt
#   }
#   
#   return(pred_list)
# }

naiveModel2 <- function(FUN){
  pred_list = vector("list", length=69)
  for(i in 1:69){
    pred_elt <- c()
    
    print(i)

    for(j in 1:dim(data_test[[1]])[1]){
      temp <- FUN(i, 
                  data_test[[1]]$hour[j],
                  data_test[[1]]$day[j],
                  data_test[[1]]$month[j])
      pred_elt <- c(pred_elt, temp)
    }

    pred_list[[i]] <- pred_elt
  }

  return(pred_list)
}



# naivePrediction1 <- naiveModel(F1)
# naivePrediction2 <- naiveModel(F2)
# 
# saveRDS("data/naivePrediction1.rds", object = naivePrediction1)
# saveRDS("data/naivePrediction2.rds", object = naivePrediction2)

# naivePrediction3 <- naiveModel2(F3)
# saveRDS("data/naivePrediction3.rds", object = naivePrediction3)

naivePrediction4 <- naiveModel2(F4)
# saveRDS("data/naivePrediction4.rds", object = naivePrediction4)

# naiveModel3 <- function(){
#   pred_list = vector("list", length=69)
#   for(i in 1:69){
# 
#     
#     pred_list[[i]] <- data_test[[i]]$rateCar_LaggedHour
#   }
#   
#   return(pred_list)
# }

naivePrediction5 <- naiveModel3()
# saveRDS("data/naivePrediction5.rds", object = naivePrediction5)
