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



numbersFromLibelle <- function(libelleList){
  d <- read_delim("data/referentiel.csv", 
                  col_names =TRUE, delim=';')
  
  numbersList = "c("
  
  for(libelle in libelleList)
  {
    for(i in 1:dim(d)[1]){
      if(d$Libelle[i] == libelle){
        numbersList = paste0(numbersList, d$`Identifiant arc`[i], ", ")
      }
    }
  }
  numbersList = str_trunc(numbersList, width = nchar(numbersList)-2, side = "right", ellipsis = "")
  numbersList = paste0(numbersList, ")")
  writeClipboard(numbersList)
  return(numbersList)
}

edges_names <- read.delim("liste_aretes.txt", header = F)$V1

######


#years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
years = c(2020)

# Suppose we have the edges list of node lists.

edges_dfs = vector("list", length(edges))
names(edges_dfs) = names(edges)
for (i in 1:length(edges_dfs)){
  edges_dfs[[i]] = data.frame()
}

ptm <- proc.time()
for (year in years){
  foldername = paste("Data/data_cleaner_", year, sep="")
  filenames = list.files(foldername)
  i = 0
  for (filename in filenames){
    if (i == 3){break}
    filepath = paste(foldername, "/", filename, sep = "")
    week_df = readRDS(filepath) # Reads the dataframe we had earlier.
    for (edgename in names(edges)){
      temp_df = filter(week_df, iu_ac %in% edges[edgename][[1]])
      agg_edge = aggregate(temp_df, by=list(temp_df$t_1h), FUN=mean, na.rm = TRUE)[c("iu_ac", "t_1h", "q", "k", "etat_barre")]
      edges_dfs[edgename] = rbind(edges_dfs[edgename],agg_edge)
    }
    i = i+1
  }
}
proc.time() - ptm
listOfDataframe = append(listOfDataframe,dataframe)



#### Generalize this to all edges at the same time:

A = list(c(1,2))
B = list(c(4,2))
C = list(c(6,2))

D = append(A,B)
append(D,C)
