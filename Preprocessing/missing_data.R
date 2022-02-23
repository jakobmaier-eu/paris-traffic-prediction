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

edges = readRDS("../Data/data_agg69_plain/edges_dfs_allyrs.rds")


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
  perct = c(perct, sum(is.na(edges[[l]]$q))/nrow(edges[[l]]))
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


####  Plot some rushhours

View(edges$`saint michel - concorde`)

sum(is.na(edges$`saint michel - concorde`[4]))

sum(edges$`saint michel - concorde`[5] == 3)

sum(is.na(edges$`saint michel - concorde`[4]) & edges$`saint michel - concorde`[5] == 3)

plot(edges$`porte maillot - porte asnieres`[10000:10500,4],type='l')
for (i in c(6)){
  labase = 50*24 + 48*i
  df = data.frame(taux = edges$`porte maillot - porte asnieres`[labase:(labase+47),5],
                  heures = c(0:47))
  g = ggplot(data=df, aes(y = taux, x = heures))  
  plot(g + geom_line()
       +xlab("heures passées à partir de minuit du premier jour")
       +ylab("taux d'occupation en %") 
       +labs(title = "Occupation de l'arête 'porte maillot - porte asnieres' sur deux jours")
  )
}

plot(edges$`ch`[10000:10500,4],type='l')
for (i in c(8,9,10,11,12,14)){
  labase = 50*24 + 48*i
  df = data.frame(taux = edges$`porte italie - place italie`[labase:(labase+23),5],
                  heures = c(0:23))
  g = ggplot(data=df, aes(y = taux, x = heures))  
  plot(g + geom_line()
       +xlab("heures passées à partir de minuit")
       +ylab("taux d'occupation en %") 
       +labs(title = "Occupation de l'arête 'porte italie - place italie' à une journée ouvrière")
  )
}


####

list_median <- c()

for (i in 1:69){
  timestamps_na <- edges[[i]]$t_1h[(is.na(edges[[i]][4]) == T)]
  
  #timestamps_na <- edges[[i]]$t_1h[(is.na(edges[[i]][4]) == T)]
  
  #print(median(diff(timestamps_na)))
  
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

#### MICE TEST

library(mice)

imp = mice(edges[[1]], meth = "cart", minbucket = 4)

test = complete(imp)

#test = imp$imp

plot(edges[[1]]$k, type = 'l')
plot(test$k[400:600], type = 'l')


# #####################################################################""
# # CREATION OF COMPLETE DATAFRAMES
# 
edges = readRDS("Data/data_with_new_variables_completation.rds")

neigh_after = readRDS('Data/neigh_after.rds')
neigh_before = readRDS('Data/neigh_before.rds')

names(edges)[45] = "rond point etoile - pont alma" #correction
names(edges)[21] = "strasbourg saint denis - porte chapelle"
names(edges)[22] = "porte chapelle - strasbourg saint denis"
names(edges)[40] = "bastille - pont austerlitz"
names(edges)[48] = "porte vincennes - bastille"
names(edges)[49] = "bastille - porte vincennes"
names(edges)[62] = "bagnolet - porte chapelle"
names(edges)[63] = "porte chapelle - bagnolet"
names(edges)[64] = "porte chapelle - porte asnieres"
names(edges)[65] = "porte asnieres - porte chapelle"

saveRDS(object = edges, file = "Data/data_with_new_variables.rds")

edges_dictionnary <- names(edges)

for(name in edges_dictionnary){
  for(neigh_after_name in neigh_after[[paste(name)]]){
    edges[[paste(name)]] <- mutate(edges[[paste(name)]], edges[[paste(neigh_after_name)]]$nbCar, edges[[paste(neigh_after_name)]]$rateCar)
    names(edges[[paste(name)]])[(dim(edges[[paste(name)]])[2]-1):(dim(edges[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }

  for(neigh_before_name in neigh_before[[paste(name)]]){
    edges[[paste(name)]] <- mutate(edges[[paste(name)]], edges[[paste(neigh_before_name)]]$nbCar, edges[[paste(neigh_before_name)]]$rateCar)
    names(edges[[paste(name)]])[(dim(edges[[paste(name)]])[2]-1):(dim(edges[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
}

saveRDS(object = edges, file = "Data/edges_with_neigh.rds")

############################################################################


#### MICE TEST 2

library(mice)

edges = readRDS("Data/edges_with_neigh.rds")

test = edges[[1]][2:length(edges[[1]])]

matrix_where = matrix(F, nrow = dim(test)[1], ncol = dim(test)[2])
matrix_where[,3:4] = is.na(test)[,3:4]

imp = mice(test, meth = "cart", minbucket = 4, where = matrix_where)

testb = complete(imp)




