rm(list=objects())

library(opera)
library(ProjetML1)

# Load data
load("1_heure/CART_with_predictionTestData.RData")
cart_with <- predictionTestData
load("1_heure/CART_without_predictionTestData.RData")
cart_without <- predictionTestData
load("1_heure/GAM_fullModel_predictionTestData.RData")
gam_full <- predictionTestData_full
load("1_heure/GAM_simpleModel_predictionTestData.RData")
gam_simple <- predictionTestData_simple
load("1_heure/Naive_predictionTestData.RData")
naive <- predictionTestData
load("1_heure/randomForest_with_predictionTestData.RData")
rf_with <- predictionTestData
load("1_heure/randomForest_without_predictionTestData.RData")
rf_without <- predictionTestData
load("1_heure/XGBoost_with_predictionTestData.RData")
xgb_with <- predictionTestData
load("1_heure/XGBoost_without_predictionTestData.RData")
xgb_without <- predictionTestData
data("data_test")

# agregation on each edges
rmse_list <- c()
for(i in 1:69){
  print(i) # to check the loop
  
  # experts
  experts <- cbind(cart_with[,i],cart_without[,i],gam_full[,i],
                   gam_simple[,i],naive[,i],rf_with[,i],rf_without[,i],
                   xgb_with[,i],xgb_without[,i])
  
  # names 
  colnames(experts)<-c("cart_with", "cart_without",
                       "gam_full", "gam_simple",
                       "naive",
                       "rf_with", "rf_without",
                       "xgb_with", "xgb_without")
  
  #agregation
  agg.online<- mixture(Y = data_test[[i]]$rateCar, experts = experts, 
                       model = 'MLpol', loss.type = "square", loss.gradient = T)

  # error
  rmse_list <- c(rmse_list,rmse(agg.online$prediction,data_test[[i]]$rateCar))
  #p<-plot(agg.online, pause=F)
}
