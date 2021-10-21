#-------- Start Header for any file -----------#
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


  
#------ Reading data week by week ---------# EXECUTED AND SAVED



years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
# years = c(2020)
for (year in years){
  foldername = paste("data/data_raw_", year, sep="")
  filenames = list.files(foldername)
  i = 0
  for (filename in filenames){
    filepath = paste(foldername, "/", filename, sep = "")
    week_df = read_delim(filepath, col_names =TRUE, delim=';')
    agg_streets = aggregate(week_df$q, by=list(week_df$libelle), FUN=mean, na.rm = TRUE)
    names(agg_streets) = c("libelle", "q")
    agg_streets_no_periph = agg_streets[!(str_sub(agg_streets$libelle, 1,2) == "PE" 
                                          | str_sub(agg_streets$libelle, 1,2)== "PI"),]
    main_streets = agg_streets_no_periph[order(agg_streets_no_periph$q, decreasing = T),]
    saveRDS(main_streets, file=paste("Data/find_main_streets/", year, "_week_",i,".rds", sep = ""))
    i = i+1
  }
}


#### We don't remember whether we've used this code.
# all_main_streets = data.frame()
# foldername = "data/find_main_streets/"
# filenames = list.files(foldername)
# for (filename in filenames){
#   filepath = paste(foldername, "/", filename, sep = "")
#   week_df = read_delim(filepath, col_names =TRUE, delim=';')
#   agg_streets = aggregate(week_df$q, by=list(week_df$libelle), FUN=mean, na.rm = TRUE)
#   names(agg_streets) = c("libelle", "q")
#   agg_streets_no_periph = agg_streets[!(str_sub(agg_streets$libelle, 1,2) == "PE" 
#                                         | str_sub(agg_streets$libelle, 1,2)== "PI"),]
#   main_streets = agg_streets_no_periph[order(agg_streets_no_periph$q, decreasing = T),]
#   saveRDS(main_streets, file=paste("Data/find_main_streets/", year, "_week_",i,".rds", sep = ""))
# }
# 
# 
# w1_2014 = readRDS("data/find_main_streets/2014_week_0.rds")

#-------------------------------------------------#


