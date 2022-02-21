rm(list=objects())

library(ProjetML1)

data_test <- readRDS("data/imp_edges_test.rds")

# Naive models
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

#Tree
predTree1 <- load("Scores/treeWithNeighbors.RData")
rmse_list_predTree_with <- rmse_list
predTree2 <- load("Scores/treeWithoutNeighbors.RData")
rmse_list_predTree_without <- rmse_list

df_tree <- data.frame(mean(rmse_list_predTree_with), mean(rmse_list_predTree_without))
names(df_tree) <- c("tree_with_neighbors", "tree_without_neighbors")

# RF
predRF1 <- load("Scores/RF_with_neighbors.RData")
rmse_list_predRF_with <- rmse_list
predRF2 <- load("Scores/RF_without_neighbors.RData")
rmse_list_predRF_without <- rmse_list

df_RF <- data.frame(3.2671, 3.2666)
names(df_RF) <- c("RF_with_neighbors", "RF_without_neighbors")

#Moyenne des rmse Paris hors périphérique
mean(rmse_list_predRF_with[1:51])

#Moyenne des rmse périphérique
mean(rmse_list_predRF_with[52:69])

#Différences des erreurs RMSE
y <- rmse_list_predRF_with - rmse_list_predRF_without
plot(1:69, rmse_list_predRF_with - rmse_list_predRF_without, pch=4,
     col = ifelse(y < -0.2,'red','black'),main = "Différences des erreurs RMSE"
     ,xlab = "Arêtes", ylab = "Différence")
abline(v=52,lty=2)
abline(h=0)
abline(h = -0.2,lty=2,col='red')

#Erreurs RMSE
plot(1:69,rmse_list_predRF_with)

