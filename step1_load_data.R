#-------- Start Header for any file -----------#
rm(list=objects()) # Clean the global environment
# First, set working directory to Folder with repo:
working_directory = "/Users/jakob/Dropbox/Uni/ProjetML/paris-traffic-prediction"
#working_directory = "/Users/lamberto_massimo"
setwd(working_directory)
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


#----------- Loading data from all years into data frames ----#

years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
no_years = 7
years = c(2014)
no_years = 1
data_raw = vector(mode="list", length=no_years)
names(data_raw) = years
for (year in years){
  foldername = paste("data/data_raw_", year, sep="")
  filenames = list.files(foldername)
  year_df = data.frame()
  for (filename in filenames){
    filepath = paste(working_directory, "/", foldername, "/", filename, sep = "")
    new_df = read_delim(filepath, col_names =TRUE, delim=';')
    year_df = merge(year_df, new_df)
  }
  data_raw[as.character(year)] = year_df
}


df = data_raw$'2014'

agg = aggregate(df, by=list(df$libelle), FUN=mean)

