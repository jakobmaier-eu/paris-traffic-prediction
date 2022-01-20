#-------- Start Header for any file -----------#
# First, set working directory to Folder with repo:

rm(list=objects()) # Clean the global environment
library(XML)
library(RCurl)
library(magrittr)
library(mgcv)
library(tidyverse)
library(dplyr)
library(lubridate)
library(weathermetrics)
library(ranger)
library(ggplot2)
#----------- End Header ----------------#


###---------------- Building a huge DataFrame for multivariate --------###

imp_edges_train = readRDS("Data/imp_edges_train.rds")
# imp_all_train = copy()

# Shared Variables:
min_index = 4
max_index = 17
imp_all_train = imp_edges_train[[1]][min_index:max_index]

for (i in 1:length(imp_edges_train)){
  edgename = names(imp_edges_train)[i]
  
  index_nb = max_index - min_index + (2*i - 1)
  imp_all_train[[index_nb]] = imp_edges_train[[i]]$nbCar
  names(imp_all_train)[index_nb] = 
    paste0("nbCar_", gsub(" ", "", gsub("-", "TO", edgename)))
    
  index_rate = index_nb + 1
  imp_all_train[[index_rate]] = imp_edges_train[[i]]$rateCar
  names(imp_all_train)[index_rate] = 
    paste0("rateCar_", gsub(" ", "", gsub("-", "TO", edgename)))
}

edgenames = names(imp_edges_train)

i = 1
print(edgenames[i]); print(i); i = i+1

periph_edges = 52:69
NE_edges = c(21,22,48,49,58,59,60,61,62,63,64,65) #northeast
S_edges = c(1,2,15,16,17,34,35,36,37,38,52,53,54,55,56,57) #south
C_edges = c(3,4,5,13,14,18,19,20,24,25,26,28,29,30,31,32,33,39,40,50,51) #center
W_edges = c(6,7,8,9,10,11,12,23,27,41,42,43,45,46,47,66,67,68,69) #west


for (i in 1:length(imp_edges_train)){
  edgenameTO = gsub(" ", "", gsub("-", "TO", names(imp_edges_train)[i]))
  edge_df = imp_edges_train
  edge_df$edgename = rep(edgenameTO, dim(edge_df)[1])
  break
}


