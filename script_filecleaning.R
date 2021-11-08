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


years = c(2015, 2016, 2017, 2018, 2019, 2020)
years = c(2014)
years = c(2015, 2016, 2017, 2018, 2019, 2020)
for (year in years){
  foldername = paste("Data/data_raw_", year, sep="")
  filenames = list.files(foldername)
  i = 0
  for (filename in filenames){
    purename = str_trunc(filename, width = nchar(filename)-4, side = "right", ellipsis = "")
    filepath = paste(foldername, "/", filename, sep = "")
    week_df = read_delim(filepath, col_names =TRUE, delim=';')[c("iu_ac","t_1h","q","k","etat_barre")]
    saveRDS(week_df, file=paste("Data/data_cleaner_", year, "/",purename,".rds", sep = ""))
    i = i+1
  }
}

