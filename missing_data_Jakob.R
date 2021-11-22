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

