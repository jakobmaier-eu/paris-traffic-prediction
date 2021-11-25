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

####

list_median <- c()

for (i in 1:69){
  timestamps_na <- edges[[i]]$t_1h[(is.na(edges[[i]][4]) == T)]
  
  timestamps_na <- edges[[i]]$t_1h[(is.na(edges[[i]][4]) == T)]
  
  print(median(diff(timestamps_na)))
  
  list_median <- c(list_median, as.numeric(diff(timestamps_na)))
  
  hist(as.numeric(diff(timestamps_na)),breaks=50)
}

hist(list_median,breaks=50)

####

mat <- data.frame(matrix(ncol = length(edges$`pont amont - pont austerlitz`$t_1h), nrow = 69))

for (i in 1:69){
  mat[i,] = is.na(edges[[i]][4]) 
}

library('plot.matrix')
plot(as.matrix(mat[,(61481-24):61481]))

sum((mat[,(61481-365*24):61481]))

####

library(mice)

imp = mice(edges[[1]], meth = "cart", minbucket = 4)

test = complete(imp)

#test = imp$imp

plot(edges[[1]]$k, type = 'l')
plot(test$k[400:600], type = 'l')

#

library(weathermetrics)

data("paris")



summary(edges[[1]]$t_1h)



