rm(list=objects())

library(XML)
library(RCurl)
library(magrittr)
library(mgcv)
library(tidyverse)
library(dplyr)
library(lubridate)
library(weathermetrics)
library(ranger)

edges = readRDS("Data/data_agg69_plain/edges_dfs_allyrs.rds")


# Counting NAs. Some q-counters (number of cars passing) have 100% missing data. Need to find the counting points with capteurs. 
for (i in 1:length(edges)){
  print(colSums(is.na(edges[[i]]))/nrow(edges[[i]]))
}

for (i in 1:length(edges)){
  if(sum(is.na(edges[[i]]$q))/nrow(edges[[i]]) > 0.1){
    print(names(edges)[i])
  }
}

# Histogram of NA
perct = c()
for(l in 1:69){
  perct = c(perct, sum(is.na(edges[[l]]$q))/nrow(edges[[i]]))
}

hist(perct, breaks=seq(0,1,0.01))

# Visualisation of NA
library(visdat)

vis_miss(edges[[3]][3:4]) + theme(axis.text.x = element_text(angle=80))

