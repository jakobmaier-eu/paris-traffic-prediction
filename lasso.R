rm(list=objects())

library("glmnet")

edges = readRDS("data/imp_edges_train.rds")

data = edges[[1]]


x <- model.matrix(rateCar~., data)[,-1]
y <- data$rateCar
mod <- cv.glmnet(as.matrix(x), y, alpha=1, type.measure = "mse")

coef = as.matrix(coef(mod, mod$lambda.1se))

data_selected <- data[,which(coef>0)]

test <- subset(x = data, coef > 0)




