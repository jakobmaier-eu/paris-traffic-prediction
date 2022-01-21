rm(list=objects())

library(gbm)
library(xgboost)
library(magrittr)
library(ProjetML1)

data("data_test")
data("data_train")

daysToNumber <- function(day){
  if(day == "lundi"){return(1)}
  if(day == "mardi"){return(2)}
  if(day == "mercredi"){return(3)}
  if(day == "jeudi"){return(4)}
  if(day == "vendredi"){return(5)}
  if(day == "samedi"){return(6)}
  if(day == "dimanche"){return(7)}
}


Ntree=500
eq<-as.formula("rateCar~.")

data_train[[1]]$weekdays <- unlist(lapply(X = data_train[[1]]$weekdays, FUN = daysToNumber))

gbm0=gbm(eq,distribution = "gaussian" , data = data_train[[1]], n.trees = Ntree,interaction.depth = 2,
         n.minobsinnode = 5, shrinkage = 0.05, bag.fraction = 0.5, train.fraction = 0.9,
         keep.data = FALSE, n.cores = 4)

best.iter=gbm.perf(gbm0,method="OOB", plot.it = TRUE,oobag.curve = TRUE)    
best.iter    
which.min(-cumsum(gbm0$oobag.improve))
plot(-cumsum(gbm0$oobag.improve),type='l')



