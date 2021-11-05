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
  
  numbersList = c()
  
  for(libelle in libelleList)
  {
    for(i in 1:dim(d)[1]){
      if(d$Libelle[i] == libelle){
        numbersList = c(numbersList, d$`Identifiant arc`[i])
      }
    }
  }
  
  return(numbersList)
}

edges_name <- read.delim("liste_aretes.txt", header = F)$V1









