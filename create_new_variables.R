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

### ALREADY COMPILED
# 
# edges = readRDS("Data/data_agg69_plain/edges_dfs_allyrs.rds")
# 
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        Year = format(t_1h, "%Y"),
#                        Month = format(t_1h, "%m"),
#                        Day = format(t_1h, "%d"),
#                        Hour = format(t_1h, "%H"),
#                        Time = 1:dim(edges[[1]])[1],
#                        Weekdays = weekdays(t_1h))
# }
# 
# weekendsIndicator <- function(day){
#   if(day %in% c("samedi","dimanche")){
#     return(1)
#   }
#   else{
#     return(0)
#   }
# }
# 
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        weekendsIndicator = unlist(lapply(X = edges[[1]]$Weekdays, FUN = weekendsIndicator)))
# }
# 
# 
# ## Covid index
# 
# covid_index_data_raw <- read_delim("data/covid_index_data_raw.csv",
#                 col_names =TRUE, delim=',')
# 
# covid_index_data_raw <- covid_index_data_raw[which(covid_index_data_raw$Entity == "France"),]
# 
# covid_index_data_raw <- covid_index_data_raw[which(covid_index_data_raw$Entity == "France"),][1:346,]
# 
# covid_index_data_raw = covid_index_data_raw %>% filter(Day > as.Date("2020-01-01"))
# 
# zero_vector <- rep(x = 0,times=dim(edges[[1]])[1]-dim(covid_index_data_raw)[1]*24)
# 
# covid_vector <- c()
# 
# for(d in 1:dim(covid_index_data_raw)[1]){
#   covid_vector <- c(covid_vector, rep(x = covid_index_data_raw$stringency_index[d], times = 24))
# }
# 
# covidIndex <- c(zero_vector, covid_vector)
# 
# for (i in 1:length(edges)){
#     edges[[i]] <- mutate(edges[[i]],
#                          covidIndex)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# #
# 
# #ADD WEATHER DATA
# 
# library(lubridate)
# library(zoo)
# library(xts)
# 
# #TEMPERATURE
# 
# #Load current data
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# #Source : https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/table/?sort=date&q=%C3%AEle+de+france&refine.nom=ORLY&q.timerange.date=date:%5B2013-12-31T23:00:00Z+TO+2021-01-01T22:59:59Z%5D
# 
# #Load weather data raw
# weather_data_raw <- read_delim("data/weather_data_raw.csv",
#                 col_names =TRUE, delim=';')
# 
# #Select only temperature and date
# temp_raw <- weather_data_raw[c(2,8)] #on garde la date et la température
# 
# #Rename variables to avoid problem with accent
# names(temp_raw) = c("date", "temp")
# 
# #Order by date
# temp_ordered <- temp_raw %>% arrange(temp_raw$date)
# 
# #Create xts of weather_temp_ordered
# xtsTemp <- xts(temp_ordered$temp, order.by=temp_ordered$date)
# 
# #Merge the previous xts with dates
# xtsTempMerged <- merge(xtsTemp,xts(,edges[[1]]$t_1h), join = "right")
# 
# #Manually fill the two first NA to avoid problem with na.approx (length = length - 2)
# xtsTempMerged[1] = xtsTempMerged[3]
# xtsTempMerged[2] = xtsTempMerged[3]
# 
# #Approximation of the NA
# Temp <- as.numeric(na.approx(xtsTempMerged))
# 
# #Add the new variable
# for (i in 1:length(edges)){
#     edges[[i]] <- mutate(edges[[i]],
#                          Temp)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# #PRECIPITATION
# 
# #Load weather data raw
# weather_data_raw <- read_delim("data/weather_data_raw.csv",
#                                col_names =TRUE, delim=';')
# 
# #Select only temperature and date
# precipitation_raw <- weather_data_raw[c(2,40)] #on garde la date et les précipitations sur les 3 dernières heures
# 
# #Rename variables to avoid problem with accent
# names(precipitation_raw) = c("date", "precipitation")
# 
# #Order by date
# precipitation_ordered <- precipitation_raw %>% arrange(precipitation_raw$date)
# 
# #Create xts of weather_temp_ordered
# xtsPrecipitation <- xts(precipitation_ordered$precipitation, order.by=precipitation_ordered$date)
# 
# #Merge the previous xts with dates
# xtsPrecipitationMerged <- merge(xtsPrecipitation,xts(,edges[[1]]$t_1h), join = "right")
# 
# #Manually fill the two first NA to avoid problem with na.approx (length = length - 2)
# xtsPrecipitationMerged[1] = xtsPrecipitationMerged[3]
# xtsPrecipitationMerged[2] = xtsPrecipitationMerged[3]
# 
# #Approximation of the NA
# Precipitation <- as.numeric(na.approx(xtsPrecipitationMerged))
# 
# #Add the new variable
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        Precipitation)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")

# ## HOLIDAYS DATA
# 
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# #Source : https://www.education.gouv.fr/les-archives-du-calendrier-scolaire-12449
# 
# #Winter holidays dates
# # 01 01 2014 - 06 01 2014
# # 20 12 2015 - 03 01 2016
# # 18 01 2016 - 02 01 2017
# # 24 01 2017 - 07 01 2018
# # 23 01 2018 - 06 01 2019
# # 22 01 2019 - 05 01 2020
# # 20 01 2020 - 01 01 2021
# 
# #Initialization of the indicator
# winterHolidaysIndicator = rep(x = 0, times = dim(edges[[1]])[1])
# 
# #Dates from timestamps
# dates_vector = date(edges[[1]]$t_1h)
# 
# #Assign 1 for holidays
# winterHolidaysIndicator[which(dates_vector %within% interval(dmy("01 01 2014"), dmy("06 01 2014"))|
#                               dates_vector %within% interval(dmy("20 12 2015"), dmy("03 01 2016"))|
#                               dates_vector %within% interval(dmy("18 12 2016"), dmy("02 01 2017"))|
#                               dates_vector %within% interval(dmy("24 12 2017"), dmy("07 01 2018"))|
#                               dates_vector %within% interval(dmy("23 12 2018"), dmy("06 01 2019"))|
#                               dates_vector %within% interval(dmy("22 12 2019"), dmy("05 01 2020"))|
#                               dates_vector %within% interval(dmy("20 12 2020"), dmy("01 01 2021")))] = 1
# 
# #Add the new variable
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        winterHolidaysIndicator)
# }
# 
# #Summer
# # 05 07 2014 - 02 09 2014
# # 04 07 2015 - 02 09 2015
# # 06 07 2016 - 01 09 2016
# # 09 07 2017 - 04 09 2017
# # 08 07 2018 - 02 09 2018
# # 07 07 2019 - 02 09 2019
# # 05 07 2020 - 01 09 2020
# 
# #Initialization of the indicator
# summerHolidaysIndicator = rep(x = 0, times = dim(edges[[1]])[1])
# 
# #Dates from timestamps
# dates_vector = date(edges[[1]]$t_1h)
# 
# #Assign 1 for holidays
# summerHolidaysIndicator[which(dates_vector %within% interval(dmy("05 07 2014"), dmy("06 09 2014"))|
#                                 dates_vector %within% interval(dmy("04 07 2015"), dmy("02 09 2015"))|
#                                 dates_vector %within% interval(dmy("06 07 2016"), dmy("01 09 2016"))|
#                                 dates_vector %within% interval(dmy("09 07 2017"), dmy("04 09 2017"))|
#                                 dates_vector %within% interval(dmy("08 07 2018"), dmy("02 09 2018"))|
#                                 dates_vector %within% interval(dmy("07 07 2019"), dmy("02 09 2019"))|
#                                 dates_vector %within% interval(dmy("05 07 2020"), dmy("01 09 2020")))] = 1
# 
# #Add the new variable
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        summerHolidaysIndicator)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# 
# 
# 
# ### BANK HOLIDAYS
# 
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# #Source : https://www.data.gouv.fr/en/datasets/jours-feries-en-france/
# 
# #Load bank holidays data raw
# bank_holidays_data_raw <- read_delim("data/jours_feries_metropole.csv",
#                 col_names = TRUE, delim=',')[,1]
# 
# #Initialization of the indicator
# bankHolidaysIndicator = rep(x = 0, times = dim(edges[[1]])[1])
# 
# #Dates from timestamps
# dates_vector = date(edges[[1]]$t_1h)
# 
# #Assign 1 for bank holidays
# bankHolidaysIndicator[which(dates_vector %in% bank_holidays_data_raw$date)] = 1
# 
# #Add the new variable
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        bankHolidaysIndicator)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# 
# 
# 
# ### Time of year
# 
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# #Number of hours in each year
# numberHoursInYears = unlist(lapply(X = c(2014:2021), FUN = function(x) return(length(edges[[1]]$t_1h[which(year(edges[[1]]$t_1h)==x)]))))
# 
# #Initialization of the variable
# toy <- c()
# 
# #Append the toy of each year
# for(n in numberHoursInYears){
#   v <- (1:n)/n
#   toy <- c(toy, v)
# }
# 
# #Add the new variable
# for (i in 1:length(edges)){
#   edges[[i]] <- mutate(edges[[i]],
#                        toy)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# 
# 
###DATA LAGGED AND FUTURE (completation)
# 
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# length = dim(edges[[1]])[1]
# 
# for (i in 1:length(edges)){
#   qLaggedWeek <- c(edges[[i]]$q[1:(24*7)], edges[[i]]$q[1:(length-24*7)])
#   qLaggedDay <- c(edges[[i]]$q[1:24], edges[[i]]$q[1:(length-24)])
#   qLaggedHour <- c(edges[[i]]$q[1:1], edges[[i]]$q[1:(length-1)])
# 
#   qFuturWeek <- c(edges[[i]]$q[(24*7+1):(length)], edges[[i]]$q[(length-(24*7)+1):(length)])
#   qFuturDay <- c(edges[[i]]$q[(24+1):(length)], edges[[i]]$q[(length-24+1):(length)])
#   qFuturHour <- c(edges[[i]]$q[(1+1):(length)], edges[[i]]$q[(length-1+1):(length)])
# 
#   kLaggedWeek <- c(edges[[i]]$k[1:(24*7)], edges[[i]]$k[1:(length-24*7)])
#   kLaggedDay <- c(edges[[i]]$k[1:24], edges[[i]]$k[1:(length-24)])
#   kLaggedHour <- c(edges[[i]]$k[1:1], edges[[i]]$k[1:(length-1)])
# 
#   kFuturWeek <- c(edges[[i]]$k[(24*7+1):(length)], edges[[i]]$k[(length-24*7+1):(length)])
#   kFuturDay <- c(edges[[i]]$k[(24+1):(length)], edges[[i]]$k[(length-24+1):(length)])
#   kFuturHour <- c(edges[[i]]$k[(1+1):(length)], edges[[i]]$k[(length-1+1):(length)])
# 
#   edges[[i]] <- mutate(edges[[i]],
#                        qLaggedWeek, qLaggedDay, qLaggedHour,
#                        qFuturWeek, qFuturDay, qFuturHour,
#                        kLaggedWeek, kLaggedDay, kLaggedHour,
#                        kFuturWeek, kFuturDay, kFuturHour)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables_completation.rds")



# ###DATA LAGGED
# 
# edges = readRDS("Data/data_with_new_variables.rds")
# 
# length = dim(edges[[1]])[1]
# 
# for (i in 1:length(edges)){
#   qLaggedWeek <- c(edges[[i]]$q[1:(24*7)], edges[[i]]$q[1:(length-24*7)])
#   qLaggedDay <- c(edges[[i]]$q[1:24], edges[[i]]$q[1:(length-24)])
#   qLaggedHour <- c(edges[[i]]$q[1:1], edges[[i]]$q[1:(length-1)])
#   
#   kLaggedWeek <- c(edges[[i]]$k[1:(24*7)], edges[[i]]$k[1:(length-24*7)])
#   kLaggedDay <- c(edges[[i]]$k[1:24], edges[[i]]$k[1:(length-24)])
#   kLaggedHour <- c(edges[[i]]$k[1:1], edges[[i]]$k[1:(length-1)])
#   
#   edges[[i]] <- mutate(edges[[i]],
#                        qLaggedWeek, qLaggedDay, qLaggedHour,
#                        kLaggedWeek, kLaggedDay, kLaggedHour)
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")

# #### CHANGING NAMES
# 
# edges <- readRDS("Data/data_with_new_variables.rds")
# 
# nbParam <- dim(edges[[1]])[2]
# 
# for(i in 1:69){
#   #edges[[i]] <- edges[[i]][c(2:23,27:29)]
#   edges[[i]] <- edges[[i]][c(2:nbParam)]
#   names(edges[[i]])[3] <- "nbCar"
#   names(edges[[i]])[4] <- "rateCar"
#   names(edges[[i]])[5] <- "state"
#   names(edges[[i]])[6] <- "year"
#   names(edges[[i]])[7] <- "month"
#   names(edges[[i]])[8] <- "day"
#   names(edges[[i]])[9] <- "hour"
#   names(edges[[i]])[10] <- "time"
#   names(edges[[i]])[11] <- "weekdays"
#   names(edges[[i]])[14] <- "temperature"
#   names(edges[[i]])[15] <- "precipitation"
#   
#   names(edges[[i]])[20] <- "nbCarLaggedWeek"
#   names(edges[[i]])[21] <- "nbCarLaggedDay"
#   names(edges[[i]])[22] <- "nbCarLaggedHour"
#   # names(edges[[i]])[23] <- "nbCarFuturWeek"
#   # names(edges[[i]])[24] <- "nbCarFuturDay"
#   # names(edges[[i]])[25] <- "nbCarFuturHour"
#   names(edges[[i]])[23] <- "rateCarLaggedWeek"
#   names(edges[[i]])[24] <- "rateCarLaggedDay"
#   names(edges[[i]])[25] <- "rateCarLaggedHour"
#   # names(edges[[i]])[29] <- "rateCarFuturWeek"
#   # names(edges[[i]])[30] <- "rateCarFuturDay"
#   # names(edges[[i]])[31] <- "rateCarFuturHour"
# }
# 
# saveRDS(edges, "Data/data_with_new_variables.rds")
# 
# ###
#     
# edges <- readRDS("Data/data_with_new_variables_completation.rds")
# 
# nbParam <- dim(edges[[1]])[2]
# 
# for(i in 1:69){
#   edges[[i]] <- edges[[i]][c(2:nbParam)]
#   names(edges[[i]])[3] <- "nbCar"
#   names(edges[[i]])[4] <- "rateCar"
#   names(edges[[i]])[5] <- "state"
#   names(edges[[i]])[6] <- "year"
#   names(edges[[i]])[7] <- "month"
#   names(edges[[i]])[8] <- "day"
#   names(edges[[i]])[9] <- "hour"
#   names(edges[[i]])[10] <- "time"
#   names(edges[[i]])[11] <- "weekdays"
#   names(edges[[i]])[14] <- "temperature"
#   names(edges[[i]])[15] <- "precipitation"
#   
#   names(edges[[i]])[20] <- "nbCarLaggedWeek"
#   names(edges[[i]])[21] <- "nbCarLaggedDay"
#   names(edges[[i]])[22] <- "nbCarLaggedHour"
#   names(edges[[i]])[23] <- "nbCarFuturWeek"
#   names(edges[[i]])[24] <- "nbCarFuturDay"
#   names(edges[[i]])[25] <- "nbCarFuturHour"
#   names(edges[[i]])[26] <- "rateCarLaggedWeek"
#   names(edges[[i]])[27] <- "rateCarLaggedDay"
#   names(edges[[i]])[28] <- "rateCarLaggedHour"
#   names(edges[[i]])[29] <- "rateCarFuturWeek"
#   names(edges[[i]])[30] <- "rateCarFuturDay"
#   names(edges[[i]])[31] <- "rateCarFuturHour"
# }
# 
# saveRDS(edges, "Data/data_with_new_variables_completation.rds")






