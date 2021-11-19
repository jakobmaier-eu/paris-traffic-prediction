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
#  if(sum(is.na(edges[[i]]$q))/nrow(edges[[i]]) > 0.01 && sum(is.na(edges[[i]]$q))/nrow(edges[[i]]) < 0.1){
  if(sum(is.na(edges[[i]]$q))/nrow(edges[[i]]) > 0.05){
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

#Lot of missing values
vis_miss(edges$`jussieu - pont austerlitz`[3:4]) + theme(axis.text.x = element_text(angle=80))

#
vis_miss(edges$`porte maillot - porte asnieres`[3:4]) + theme(axis.text.x = element_text(angle=80))

#
vis_miss(edges$`pont amont - porte italie`[3:4]) + theme(axis.text.x = element_text(angle=80))

#
vis_miss(edges$`porte maillot - porte asnieres`[3:4]) + theme(axis.text.x = element_text(angle=80))


#
vis_miss(edges$`bastille - chatelet`[3:4]) + theme(axis.text.x = element_text(angle=80))


####

View(edges$`saint michel - concorde`)

sum(is.na(edges$`saint michel - concorde`[4]))

sum(edges$`saint michel - concorde`[5] == 3)

sum(is.na(edges$`saint michel - concorde`[4]) & edges$`saint michel - concorde`[5] == 3)

plot(edges$`porte maillot - porte asnieres`[10000:10500,4],type='l')





