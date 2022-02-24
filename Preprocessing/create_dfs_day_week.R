rm(list=objects())

library(ProjetML1)

###########
# Day
###########
data("data_train")
data("data_test")

length1 = dim(data_train[[1]])[1]
length2 = dim(data_test[[1]])[1]

for(i in 1:69){
  data_train[[i]] <- data_train[[i]] %>% select(-c(rateCar,temperature,precipitation,covidIndex,nbCar_LaggedHour,rateCar_LaggedHour))
  data_test[[i]] <- data_test[[i]] %>% select(-c(rateCar,temperature,precipitation,covidIndex,nbCar_LaggedHour,rateCar_LaggedHour))
  
  for(j in 17:dim(data_train[[i]])[2]){
    dij = data_train[[i]] %>% select(j)
    data_train[[i]][,j] <- rbind(dij[1:24], dij[1:(length1-24)])
    
    dij = data_test[[i]] %>% select(j)
    data_test[[i]][,j] <- rbind(dij[1:24], dij[1:(length2-24)])
  }
}

data_train_day <- data_train
data_test_day <- data_test

save(data_train_day,file="data_train_day.RData")
save(data_test_day,file="data_test_day.RData")

###########
# Week
###########
data("data_train")
data("data_test")

length1 = dim(data_train[[1]])[1]
length2 = dim(data_test[[1]])[1]

for(i in 1:69){
  data_train[[i]] <- data_train[[i]] %>% select(-c(rateCar,temperature,precipitation,covidIndex,nbCar_LaggedHour,nbCar_LaggedDay,rateCar_LaggedHour,rateCar_LaggedDay))
  data_test[[i]] <- data_test[[i]] %>% select(-c(rateCar,temperature,precipitation,covidIndex,nbCar_LaggedHour,nbCar_LaggedDay,rateCar_LaggedHour,rateCar_LaggedDay))
  
  for(j in 15:dim(data_train[[i]])[2]){
    dij = data_train[[i]] %>% select(j)
    data_train[[i]][,j] <- rbind(dij[1:24], dij[1:(length1-24)])

    dij = data_test[[i]] %>% select(j)
    data_test[[i]][,j] <- rbind(dij[1:24], dij[1:(length2-24)])
  }
}

data_train_week <- data_train
data_test_week <- data_test

save(data_train_week,file="data_train_week.RData")
save(data_test_week,file="data_test_week.RData")







