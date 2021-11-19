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
# TODO aggregate paris

all_edge_df = data.frame()
for (edge_df in list_dfs){
  all_edge_df = rbind(all_edge_df, edge_df)
}
paris_df = aggregate(all_edge_df, by = list(all_edge_df$t_1h), 
                     FUN = mean, na.rm = TRUE)

saveRDS(paris_df, file="Data/paris_df.rds")


# Look, there are some days fully missing:
timecount = as.POSIXct("2014-01-01 01:00:00", tz = "UTC")
j = 1
missing_times = list()
while (j <= dim(paris_df)[1]){
  if (as.POSIXct(paris_df$t_1h[j]) == timecount){
    j = j+1
  }
  else{
    print(timecount)
  }
  timecount = timecount + 3600
}
###### Result:
# [1] "2015-05-27 06:00:00 UTC"
# [1] "2016-11-16 06:00:00 UTC"
# [1] "2017-06-27 06:00:00 UTC"
# [1] "2018-07-04 06:00:00 UTC"
# [1] "2018-08-28 06:00:00 UTC"
# [1] "2018-10-09 22:00:00 UTC"
# [1] "2019-10-31 06:00:00 UTC"

