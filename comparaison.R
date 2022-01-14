rm(list=objects())

rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

data_test <- readRDS("data/imp_edges_test.rds")
pred1 <- readRDS("data/naivePrediction1.rds")

rmse(data_test[[1]]$rateCar - pred1[[1]])



















