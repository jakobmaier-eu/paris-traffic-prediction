library("ranger")
library("vip")


edges = readRDS("data/imputed_edges_neigh.rds")

res <- ranger(rateCar ~ ., 
                   data=edges[[1]][-(1:2)], 
                   importance='impurity')

vip(res)







