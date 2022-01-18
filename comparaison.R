rm(list=objects())

library(ProjetML1)

data_test <- readRDS("data/imp_edges_test.rds")


pred1 <- readRDS("data/naivePrediction1.rds") #5.844
pred2 <- readRDS("data/naivePrediction2.rds") #5.899
pred3 <- readRDS("data/naivePrediction3.rds") #3.955

rmse_list <- c()
for(i in 1:69){
  rmse_list <- c(rmse_list, rmse(pred3[[i]], data_test[[i]]$rateCar))
}
mean(rmse_list)

df_naiveModels <- data.frame(5.844, 5.899, 3.955)
names(df_naiveModels) <- c("naiveModel1", "naiveModel2", "naiveModel3")














