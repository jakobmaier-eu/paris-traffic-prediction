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

edges_name <- read.delim("liste_aretes.txt", header = F)$V1

######

listOfDataframe = list()
list_numList = 

for(k in 1:70){
  
  numList = list_numList(k)
  dataframe = data.frame()
  
  #years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
  years = c(2020)
  
  ptm <- proc.time()
  for (year in years){
    foldername = paste("data/data_raw_", year, sep="")
    filenames = list.files(foldername)
    i = 0
    for (filename in filenames){
      filepath = paste(foldername, "/", filename, sep = "")
      week_df = read_delim(filepath, col_names =TRUE, delim=';')[c("iu_ac","t_1h","q","k","etat_barre")]
      
      dataframe_temp = data.frame()
      for(j in 1:dim(week_df)[1]){
        
        if(week_df$iu_ac[j] %in% numList){
          
          dataframe_temp <- rbind(dataframe_temp,week_df[j,])
          
        }
      }
      #print("o")
      agg_edge = aggregate(dataframe_temp, by=list(dataframe_temp$t_1h), FUN=mean, na.rm = TRUE)
      dataframe = rbind(dataframe,agg_edge)
    }
  }
  proc.time() - ptm
  listOfDataframe = append(listOfDataframe,dataframe)
}



#### Generalize this to all edges at the same time:

A = list(c(1,2))
B = list(c(4,2))
C = list(c(6,2))

D = append(A,B)
append(D,C)
