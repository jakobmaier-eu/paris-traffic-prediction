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


