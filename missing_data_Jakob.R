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
#----------- End Header ----------------#

list_dfs = readRDS("Data/data_agg69_plain/edges_dfs_allyrs.rds")
# Representing missing data:
missing_points = matrix(nrow=length(list_dfs), ncol = dim(list_dfs[[1]])[1])
#names(missing_points) = names(list_dfs)

for (j in 1:length(list_dfs)){
  missing_points[j,] = as.numeric(is.na(list_dfs[[j]]$q))
}

library(purrr)

oldpar <- par(mar = rep(0.2, 4)) # reducing plot margins
image(
  t(missing_points[, 61361-8760:61361]), # image() has some weird opinions about how your matrix will be plotted
  axes = FALSE,
  col = colorRampPalette(c("white", "black"))(30), # our colour palette
  breaks = c(seq(0, 3, length.out = 30), 100) # colour-to-value mapping
)

#-------- Filling NAs with Random Forest: EXECUTED ----------------

# dfs = readRDS("Data/edges_with_neigh.rds")
# 
# library(missRanger)
# library(caret)
# library(visdat)
# library(miceRanger)
# library(dplyr)
# 
# imp_edges_train = vector("list", length=length(dfs))
# imp_VarImportance_train = vector("list", length=length(dfs))
# missing_percent_train = vector("list", length=length(dfs))
# names(imp_edges_train) = names(dfs)
# names(imp_VarImportance_train) = names(dfs)
# names(missing_percent_train) = names(dfs)
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
# saveRDS(edges_test, "Data/edges_2020.rds")

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
imp_dfs = readRDS(file = "Data/imputed_edges_neigh.rds")
for (i in 1:69){
  df = imp_dfs[[i]]
  if(sum(is.na(df$nbCar)) != 0){
    print(paste0("nbCar missing in",names(imp_dfs)[i]))
    break
  }
  if(sum(is.na(df$rateCar)) != 0){
    print(paste0("rateCar missing in",names(imp_dfs)[i]))
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


