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

edges = readRDS("Data/data_agg69_plain/edges_dfs_allyrs.rds")

for (i in 1:length(edges)){
  edges[[i]] <- mutate(edges[[i]], Year = format(t_1h, "%Y"), Month = format(t_1h, "%m"),
                       Day = format(t_1h, "%d"),
                       Hour = format(t_1h, "%H"),
                       Time = 1:61481,
                       Weekdays = weekdays(t_1h))
}

head(edges[[1]])

weekendsIndicator <- function(day){
  if(day %in% c("samedi","dimanche")){
    return(1)
  }
  else{
    return(0)
  }
}

for (i in 1:length(edges)){
  edges[[i]] <- mutate(edges[[i]], 
                       weekendsIndicator = lapply(X = edges[[1]]$Weekdays, FUN = weekendsIndicator))
}


head(edges[[1]])

View(edges[[1]])


## Covid index

covid_index_data_raw <- read_delim("data/covid_index_data_raw.csv", 
                col_names =TRUE, delim=',')

covid_index_data_raw <- covid_index_data_raw[which(covid_index_data_raw$Entity == "France"),]

covid_index_data_raw <- covid_index_data_raw[which(covid_index_data_raw$Entity == "France"),][1:346,]

covid_index_data_raw = covid_index_data_raw %>% filter(Day > as.Date("2020-01-01"))

zero_vector <- rep(x = 0,times=dim(edges[[1]])[1]-dim(covid_index_data_raw)[1]*24)

covid_vector <- c()

for(d in 1:dim(covid_index_data_raw)[1]){
  covid_vector <- c(covid_vector, rep(x = covid_index_data_raw$stringency_index[d], times = 24))
}

vector <- c(zero_vector, covid_vector)

for (i in 1:length(edges)){
    edges[[i]] <- mutate(edges[[i]], 
                         vector)
}

saveRDS(edges, "Data/data_with_new_variables.rds")

#



