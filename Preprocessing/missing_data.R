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
library(missRanger)
library(caret)
library(visdat)
library(miceRanger)
library(dplyr)
#----------- End Header ----------------#


###############################################################
#      Exploration before data imputation
###############################################################

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

list_dfs = readRDS("../Data/data_agg69_plain/edges_dfs_allyrs.rds")
# Representing missing data:
missing_points = matrix(nrow=length(list_dfs), ncol = dim(list_dfs[[1]])[1])
#names(missing_points) = names(list_dfs)

for (j in 1:length(list_dfs)){
  missing_points[j,] = as.numeric(is.na(list_dfs[[j]]$q))
}


# Alternative: Only 25 edges.
edgechoice = c(15:25, 50:69)
missing_points = matrix(nrow=length(edgechoice), ncol = dim(list_dfs[[1]])[1])
#names(missing_points) = names(list_dfs)
j1 = 1
for (j in edgechoice){
  missing_points[j1,] = as.numeric(is.na(list_dfs[[j]]$q))
  j1 = j1+1
}


library(purrr)
oldpar <- par(mar = rep(15, 5)) # reducing plot margins
par(mar = c(3,3,0.7,0.7))
halfayear = 4380
image(
  t(missing_points[, (5*halfayear):(6*halfayear)]), # image() has some weird opinions about how your matrix will be plotted
  axes = FALSE,
  col = colorRampPalette(c("white", "black"))(30), # our colour palette
  breaks = c(seq(0, 3, length.out = 30), 100) # colour-to-value mapping
)





########################################################################
#-------- Filling NAs with Random Forest: EXECUTED ----------------
#########################################################################

 dfs = readRDS("Data/edges_with_neigh.rds")
# 
library(missRanger)
library(caret)
library(visdat)
library(miceRanger)
library(dplyr)
# 
imp_edges_train = vector("list", length=length(dfs))
imp_VarImportance_train = vector("list", length=length(dfs))
missing_percent_train = vector("list", length=length(dfs))
names(imp_edges_train) = names(dfs)
names(imp_VarImportance_train) = names(dfs)
names(missing_percent_train) = names(dfs)
# 
# ### Fix some type issues:
# for (i in 1:69){
#   dfs[[i]] = dfs[[i]][-(1:2)]
#   for (j in 4:7){
#     dfs[[i]][j] = list(as.integer(unlist(dfs[[i]][j])))
#   }
# }
# 
# ### Train-Test-Split:
# edges_test = vector("list", length=length(dfs))
# names(edges_test) = names(dfs)
# edges_train = vector("list", length=length(dfs))
# names(edges_train) = names(dfs)
# edges_2020 = vector("list", length=length(dfs))
# names(edges_2020) = names(dfs)
# 
# for (i in 1:length(edges_test)){
#   edges_test[[i]] = filter(dfs[[i]], year %in% c(2018, 2019))
#   edges_train[[i]] = filter(dfs[[i]], year %in% c(2014, 2015, 2016, 2017))
#   edges_2020[[i]] = filter(dfs[[i]], year == 2020)
# }
# 
# saveRDS(edges_test, "Data/edges_test.rds")
# saveRDS(edges_train, "Data/edges_train.rds")
# saveRDS(edges_2020, "Data/edges_2020.rds")

edges_test = readRDS("Data/edges_test.rds")
edges_train = readRDS("Data/edges_train.rds")
edges_2020 = readRDS("Data/edges_2020.rds")

start_time = Sys.time()
for (i in 1:length(edges_train)){
  df = edges_train[[i]]
  edgename = names(edges_train)[i]
  varnames = names(df)
  missing_percent_train[[i]] = c(round(sum(is.na(df$rateCar))/length(df$rateCar), 4)*100,
                           round(sum(is.na(df$nbCar))/length(df$nbCar), 4)*100)
  vars <- list(
    rateCar = varnames[! varnames %in% c('covidIndex', 'rateCar')],
    nbCar = varnames[! varnames %in% c('covidIndex', 'nbCar')]
  )
  mice_obj <- miceRanger(
    df,
    m = 1,
    maxiter = 5,
    vars = vars,
    verbose=TRUE,
    num.trees = 100,
    mtry = 7
  )
  imp = completeData(mice_obj)[[1]]
  imp_VarImportance_train[[i]] = mice_obj$finalImport[[1]]
  imp_edges_train[[i]] = imp
}
end_time = Sys.time()
time_elapsed = end_time - start_time




########################"

for(i in 1:69){
  imp_edges_train[[i]] <- imp_edges_train[[i]][,c(1:17)]
}

#########################

length = dim(imp_edges_train[[1]])[1]

for (i in 1:length(imp_edges_train)){ # Note: We just copy the first week due to lack of data before.
  nbCar_LaggedWeek <- c(imp_edges_train[[i]]$nbCar[1:(24*7)], imp_edges_train[[i]]$nbCar[1:(length-24*7)])
  nbCar_LaggedDay <- c(imp_edges_train[[i]]$nbCar[1:24], imp_edges_train[[i]]$nbCar[1:(length-24)])
  nbCar_LaggedHour <- c(imp_edges_train[[i]]$nbCar[1:1], imp_edges_train[[i]]$nbCar[1:(length-1)])

  rateCar_LaggedWeek <- c(imp_edges_train[[i]]$rateCar[1:(24*7)], imp_edges_train[[i]]$rateCar[1:(length-24*7)])
  rateCar_LaggedDay <- c(imp_edges_train[[i]]$rateCar[1:24], imp_edges_train[[i]]$rateCar[1:(length-24)])
  rateCar_LaggedHour <- c(imp_edges_train[[i]]$rateCar[1:1], imp_edges_train[[i]]$rateCar[1:(length-1)])

  imp_edges_train[[i]] <- mutate(imp_edges_train[[i]],
                       nbCar_LaggedWeek, nbCar_LaggedDay, nbCar_LaggedHour,
                       rateCar_LaggedWeek, rateCar_LaggedDay, rateCar_LaggedHour)
}

###############
library(dplyr)
edges_dictionnary <- names(imp_edges_train)
neigh_after = readRDS('Data/neigh_after.rds')
neigh_before = readRDS('Data/neigh_before.rds')

for(name in edges_dictionnary){
  for(neigh_after_name in neigh_after[[paste(name)]]){
    imp_edges_train[[paste(name)]] <- mutate(imp_edges_train[[paste(name)]], imp_edges_train[[paste(neigh_after_name)]]$nbCar, imp_edges_train[[paste(neigh_after_name)]]$rateCar)
    names(imp_edges_train[[paste(name)]])[(dim(imp_edges_train[[paste(name)]])[2]-1):(dim(imp_edges_train[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"))
    }
  
  for(neigh_before_name in neigh_before[[paste(name)]]){
    imp_edges_train[[paste(name)]] <- mutate(imp_edges_train[[paste(name)]], imp_edges_train[[paste(neigh_before_name)]]$nbCar, imp_edges_train[[paste(neigh_before_name)]]$rateCar)
    names(imp_edges_train[[paste(name)]])[(dim(imp_edges_train[[paste(name)]])[2]-1):(dim(imp_edges_train[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
}

for(i in 1:69){
  for(j in 24:(dim(imp_edges_train[[i]])[2])){
    str = names(imp_edges_train[[i]])[j]
    if (substring(str,1,1) == "q"){
      names(imp_edges_train[[i]])[j] = paste0("rateCar" , substring(str,2))
    }
    if (substring(str,1,1) == "k"){
      names(imp_edges_train[[i]])[j] = paste0("nbCar" , substring(str,2))
    }
  }
}

saveRDS(imp_edges_train, "Data/imp_edges_train.rds")
saveRDS(imp_VarImportance_train, "Data/imp_VarImportance_train.rds")
saveRDS(missing_percent_train, "Data/imp_missing_percent_train.rds")


#### NOT YET EXECUTED:

# Setting the blocked road's traffic to 0.
for (line in 1:length(df$state)){
  if (df$state[line] == 2){ # If blocked
    if (is.na(df$nbCar[line])){
      df$nbCar[line] = 0
      print("nb")
      print(df$t_1h[line])
    }
    if (is.na(df$rateCar[line])){
      df$rateCar[line] = 0
      print("rate")
      print(df$t_1h[line])
    }
  }
}


#------ TESTING the imputation ----------#
imp_edges_train = readRDS(file = "Data/imp_edges_train.rds")
for (i in 1:69){
  df = imp_edges_train[[i]]
  if(sum(is.na(df$nbCar)) != 0){
    print(paste0("nbCar missing in",names(imp_edges_train)[i]))
    break
  }
  if(sum(is.na(df$rateCar)) != 0){
    print(paste0("rateCar missing in",names(imp_edges_train)[i]))
    break
  }
}

imp_percentNA = readRDS(file = "Data/imputation_missing_percent.rds")
imp_varImpo = readRDS(file = "Data/imputation_VarImportance.rds")

# q importance

i = 29
for (i in 1:69){
  print(paste(imp_percentNA[[i]], "asdf ", i))
}
edge_impo = imp_varImpo[[i]]
edge_percent = imp_percentNA[[i]] # this is really in %

edge_q_impo = edge_impo[1,]
ord = order(unlist(edge_q_impo[-(1:2)]), decreasing = TRUE)
relevant_vars = edge_q_impo[-(1:2)]
relevant_vars[ord]



###################################################################
# Visualization of imputation for report
###################################################################

imp_percentNA = readRDS(file = "../Data/imp_missing_percent_train.rds")
imp_varImpo = readRDS(file = "../Data/imp_VarImportance_train.rds")

for(i in 1:69){ # First fix the names
  for(j in 24:(dim(imp_varImpo[[i]])[2])){
    str = names(imp_varImpo[[i]])[j]
    if (substring(str,1,1) == "q"){
      names(imp_varImpo[[i]])[j] = paste0("nbCar" , substring(str,2))
    }
    if (substring(str,1,1) == "k"){
      names(imp_varImpo[[i]])[j] = paste0("rateCar" , substring(str,2))
    }
  }
}

for (i in 20:29){
  impo_rateCar_raw = transpose(imp_varImpo[[i]])[[1]][-c(1,2)]
  impo_rateCar = do.call(rbind.data.frame, impo_rateCar_raw)
  rm(impo_rateCar_raw)
  colnames(impo_rateCar) = c("values")
  impo_rateCar$variables = names(imp_varImpo[[i]])[-c(1,2)]
  
  impo_nbCar_raw = transpose(imp_varImpo[[i]])[[2]][-c(1,3)]
  impo_nbCar = do.call(rbind.data.frame, impo_nbCar_raw)
  rm(impo_nbCar_raw)
  colnames(impo_nbCar) = c("values")
  impo_nbCar$variables = names(imp_varImpo[[i]])[-c(1,3)]
  
  
  g = ggplot(data = head(arrange(impo_rateCar, desc(values)), 6), 
             aes(y = reorder(variables, values), x = values))  
  plot(g + geom_bar(stat="identity")
       +xlab(paste0("RF importance scores (", imp_percentNA[[i]][1], "% NA imputed)"))
       +ylab("variables with highest scores") 
       +labs(title = paste0("rateCar for ", names(imp_varImpo[i])))
  )
  
  g = ggplot(data = head(arrange(impo_nbCar, desc(values)), 6), 
             aes(y = reorder(variables, values), x = values))  
  plot(g + geom_bar(stat="identity")
       +xlab(paste0("RF importance scores (", imp_percentNA[[i]][2], "% NA imputed)"))
       +ylab("variables (max. scores)") 
       +labs(title = paste0("nbCar for ", names(imp_varImpo[i])))
  )
}

##############################################
###### completion data test
##############################################

dfs = readRDS("Data/edges_with_neigh.rds")

imp_edges_test = vector("list", length=length(dfs))
imp_VarImportance_test = vector("list", length=length(dfs))
missing_percent_test = vector("list", length=length(dfs))
names(imp_edges_test) = names(dfs)
names(imp_VarImportance_test) = names(dfs)
names(missing_percent_test) = names(dfs)

edges_test = readRDS("Data/edges_test.rds")

start_time = Sys.time()
for (i in 1:length(edges_test)){
  df = edges_test[[i]]
  edgename = names(edges_test)[i]
  varnames = names(df)
  missing_percent_test[[i]] = c(round(sum(is.na(df$rateCar))/length(df$rateCar), 4)*100,
                                 round(sum(is.na(df$nbCar))/length(df$nbCar), 4)*100)
  vars <- list(
    rateCar = varnames[! varnames %in% c('covidIndex', 'rateCar')],
    nbCar = varnames[! varnames %in% c('covidIndex', 'nbCar')]
  )
  mice_obj <- miceRanger(
    df,
    m = 1,
    maxiter = 5,
    vars = vars,
    verbose=TRUE,
    num.trees = 100,
    mtry = 7
  )
  imp = completeData(mice_obj)[[1]]
  imp_VarImportance_test[[i]] = mice_obj$finalImport[[1]]
  imp_edges_test[[i]] = imp
}
end_time = Sys.time()
time_elapsed = end_time - start_time




########################"

for(i in 1:69){
  imp_edges_test[[i]] <- imp_edges_test[[i]][,c(1:17)]
}

#########################

length = dim(imp_edges_test[[1]])[1]

for (i in 1:length(imp_edges_test)){ # Note: We just copy the first week due to lack of data before.
  nbCar_LaggedWeek <- c(imp_edges_test[[i]]$nbCar[1:(24*7)], imp_edges_test[[i]]$nbCar[1:(length-24*7)])
  nbCar_LaggedDay <- c(imp_edges_test[[i]]$nbCar[1:24], imp_edges_test[[i]]$nbCar[1:(length-24)])
  nbCar_LaggedHour <- c(imp_edges_test[[i]]$nbCar[1:1], imp_edges_test[[i]]$nbCar[1:(length-1)])
  
  rateCar_LaggedWeek <- c(imp_edges_test[[i]]$rateCar[1:(24*7)], imp_edges_test[[i]]$rateCar[1:(length-24*7)])
  rateCar_LaggedDay <- c(imp_edges_test[[i]]$rateCar[1:24], imp_edges_test[[i]]$rateCar[1:(length-24)])
  rateCar_LaggedHour <- c(imp_edges_test[[i]]$rateCar[1:1], imp_edges_test[[i]]$rateCar[1:(length-1)])
  
  imp_edges_test[[i]] <- mutate(imp_edges_test[[i]],
                                 nbCar_LaggedWeek, nbCar_LaggedDay, nbCar_LaggedHour,
                                 rateCar_LaggedWeek, rateCar_LaggedDay, rateCar_LaggedHour)
}

###############
library(dplyr)
edges_dictionnary <- names(imp_edges_test)
neigh_after = readRDS('Data/neigh_after.rds')
neigh_before = readRDS('Data/neigh_before.rds')

for(name in edges_dictionnary){
  for(neigh_after_name in neigh_after[[paste(name)]]){
    imp_edges_test[[paste(name)]] <- mutate(imp_edges_test[[paste(name)]], imp_edges_test[[paste(neigh_after_name)]]$nbCar, imp_edges_test[[paste(neigh_after_name)]]$rateCar)
    names(imp_edges_test[[paste(name)]])[(dim(imp_edges_test[[paste(name)]])[2]-1):(dim(imp_edges_test[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
  
  for(neigh_before_name in neigh_before[[paste(name)]]){
    imp_edges_test[[paste(name)]] <- mutate(imp_edges_test[[paste(name)]], imp_edges_test[[paste(neigh_before_name)]]$nbCar, imp_edges_test[[paste(neigh_before_name)]]$rateCar)
    names(imp_edges_test[[paste(name)]])[(dim(imp_edges_test[[paste(name)]])[2]-1):(dim(imp_edges_test[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
}

for(i in 1:69){
  for(j in 24:(dim(imp_edges_test[[i]])[2])){
    str = names(imp_edges_test[[i]])[j]
    if (substring(str,1,1) == "q"){
      names(imp_edges_test[[i]])[j] = paste0("rateCar" , substring(str,2))
    }
    if (substring(str,1,1) == "k"){
      names(imp_edges_test[[i]])[j] = paste0("nbCar" , substring(str,2))
    }
  }
}

saveRDS(imp_edges_test, "Data/imp_edges_test.rds")
saveRDS(imp_VarImportance_test, "Data/imp_VarImportance_test.rds")
saveRDS(missing_percent_test, "Data/imp_missing_percent_test.rds")


##############################################
###### completion data test2020
##############################################

dfs = readRDS("Data/edges_with_neigh.rds")

imp_edges_test2020 = vector("list", length=length(dfs))
imp_VarImportance_test2020 = vector("list", length=length(dfs))
missing_percent_test2020 = vector("list", length=length(dfs))
names(imp_edges_test2020) = names(dfs)
names(imp_VarImportance_test2020) = names(dfs)
names(missing_percent_test2020) = names(dfs)

edges_test2020 = readRDS("Data/edges_2020.rds")

start_time = Sys.time()
for (i in 1:length(edges_test2020)){
  df = edges_test2020[[i]]
  edgename = names(edges_test2020)[i]
  varnames = names(df)
  missing_percent_test2020[[i]] = c(round(sum(is.na(df$rateCar))/length(df$rateCar), 4)*100,
                                round(sum(is.na(df$nbCar))/length(df$nbCar), 4)*100)
  vars <- list(
    rateCar = varnames[! varnames %in% c('covidIndex', 'rateCar')],
    nbCar = varnames[! varnames %in% c('covidIndex', 'nbCar')]
  )
  mice_obj <- miceRanger(
    df,
    m = 1,
    maxiter = 5,
    vars = vars,
    verbose=TRUE,
    num.trees = 100,
    mtry = 7
  )
  imp = completeData(mice_obj)[[1]]
  imp_VarImportance_test2020[[i]] = mice_obj$finalImport[[1]]
  imp_edges_test2020[[i]] = imp
}
end_time = Sys.time()
time_elapsed = end_time - start_time




########################"

for(i in 1:69){
  imp_edges_test2020[[i]] <- imp_edges_test2020[[i]][,c(1:17)]
}

#########################

length = dim(imp_edges_test2020[[1]])[1]

for (i in 1:length(imp_edges_test2020)){ # Note: We just copy the first week due to lack of data before.
  nbCar_LaggedWeek <- c(imp_edges_test2020[[i]]$nbCar[1:(24*7)], imp_edges_test2020[[i]]$nbCar[1:(length-24*7)])
  nbCar_LaggedDay <- c(imp_edges_test2020[[i]]$nbCar[1:24], imp_edges_test2020[[i]]$nbCar[1:(length-24)])
  nbCar_LaggedHour <- c(imp_edges_test2020[[i]]$nbCar[1:1], imp_edges_test2020[[i]]$nbCar[1:(length-1)])
  
  rateCar_LaggedWeek <- c(imp_edges_test2020[[i]]$rateCar[1:(24*7)], imp_edges_test2020[[i]]$rateCar[1:(length-24*7)])
  rateCar_LaggedDay <- c(imp_edges_test2020[[i]]$rateCar[1:24], imp_edges_test2020[[i]]$rateCar[1:(length-24)])
  rateCar_LaggedHour <- c(imp_edges_test2020[[i]]$rateCar[1:1], imp_edges_test2020[[i]]$rateCar[1:(length-1)])
  
  imp_edges_test2020[[i]] <- mutate(imp_edges_test2020[[i]],
                                nbCar_LaggedWeek, nbCar_LaggedDay, nbCar_LaggedHour,
                                rateCar_LaggedWeek, rateCar_LaggedDay, rateCar_LaggedHour)
}

###############
library(dplyr)
edges_dictionnary <- names(imp_edges_test2020)
neigh_after = readRDS('Data/neigh_after.rds')
neigh_before = readRDS('Data/neigh_before.rds')

for(name in edges_dictionnary){
  for(neigh_after_name in neigh_after[[paste(name)]]){
    imp_edges_test2020[[paste(name)]] <- mutate(imp_edges_test2020[[paste(name)]], imp_edges_test2020[[paste(neigh_after_name)]]$nbCar, imp_edges_test2020[[paste(neigh_after_name)]]$rateCar)
    names(imp_edges_test2020[[paste(name)]])[(dim(imp_edges_test2020[[paste(name)]])[2]-1):(dim(imp_edges_test2020[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_after_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
  
  for(neigh_before_name in neigh_before[[paste(name)]]){
    imp_edges_test2020[[paste(name)]] <- mutate(imp_edges_test2020[[paste(name)]], imp_edges_test2020[[paste(neigh_before_name)]]$nbCar, imp_edges_test2020[[paste(neigh_before_name)]]$rateCar)
    names(imp_edges_test2020[[paste(name)]])[(dim(imp_edges_test2020[[paste(name)]])[2]-1):(dim(imp_edges_test2020[[paste(name)]])[2])] <- c(paste("k",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"), paste("q",gsub("-", "TO", gsub(" ", "", neigh_before_name, fixed = TRUE), fixed = TRUE),sep = "_"))
  }
}

for(i in 1:69){
  for(j in 24:(dim(imp_edges_test2020[[i]])[2])){
    str = names(imp_edges_test2020[[i]])[j]
    if (substring(str,1,1) == "q"){
      names(imp_edges_test2020[[i]])[j] = paste0("rateCar" , substring(str,2))
    }
    if (substring(str,1,1) == "k"){
      names(imp_edges_test2020[[i]])[j] = paste0("nbCar" , substring(str,2))
    }
  }
}

saveRDS(imp_edges_test2020, "Data/imp_edges_test2020.rds")
saveRDS(imp_VarImportance_test2020, "Data/imp_VarImportance_test2020.rds")
saveRDS(missing_percent_test2020, "Data/imp_missing_percent_test2020.rds")


#####################################################################
# Fixing some issues with the full dataframes
#####################################################################



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
