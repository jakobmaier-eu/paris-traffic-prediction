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

d <- read_delim("data/data_raw_2014/trafic_capteurs_2014_W00_20140101_20140102.txt", 
                             col_names =TRUE, delim=';')
str(d)
# Signification de variables:
# iu_ac = graph edge unique identifier
# libelle = street name of where the graph edge is placed
# iu_nd_amont = ID of the node just before in street
# libelle_nd_amont = exact name of the node before
# iu_nd_aval = ID of the node just after in street
# libelle_nd_aval = exact name of the node after
# t_1h = timestamp of measurement
# q = number of cars passing within hour of measurement
# k = occupation of measurememnt point in percent = traffic density
# etat_trafic = a strict function of k; indicates traffic status
# etat_barre = status of the street arc (see iu_ac): constructions etc
# Dessin = code for drawing the street arc.


#### Visualize 2015 first week ####
data_2015_w00 <- read_delim("data/data_raw_2015/trafic_capteurs_2015_W00_20150101_20150108.txt",
                            col_names =TRUE, delim=';')
str(data_2015_w00)
again_799 = filter(data_2015_w00, iu_ac == 799)[c("iu_ac", "t_1h", "k", "q")]
again_799

par(mfrow=c(1,1))
plot(again_799$t_1h, again_799$k, type='l')


# #### 2020 first week: Data is not complete for node 799, so we use node 200 ####
# data_2020_w00 <- read_delim("/Users/jakob/Dropbox/Uni/ProjetML/paris-traffic-prediction/data/data_raw_2020/trafic_capteurs_2020_W00_20200101_20200108.txt", 
#                             col_names =TRUE, delim=';')
# str(data_2020_w00)
# compare_200 = filter(data_2020_w00, iu_ac == 200)[c("iu_ac", "t_1h", "k", "q")]
# compare_200
# 
# par(mfrow=c(1,1))
# plot(compare_200$t_1h, compare_200$k, type='l')


#--------------- 

data_2020_w00 <- read_delim("data/data_raw_2020/trafic_capteurs_2020_W02_20200115_20200122.txt", 
                           col_names =TRUE, delim=';')

selection = data_2020_w00[order(data_2020_w00$q, decreasing = T),]
summary(data_2020_w00 %>% drop_na(q))


# TODO: Aggr?g libell?s before identifying main axes.

d = data_2020_w00

wo_periph = d[!(str_sub(d$libelle, 1,2) == "PE" | str_sub(d$libelle, 1,2)== "PI"),]

#main_streets = wo_periph[order(wo_periph$q, decreasing = T),]

streets_frequency_rows = wo_periph[c("libelle","q")]

main_streets_aggregated_by_libelle = aggregate(streets_frequency_rows$q,
                                               by = list(streets_frequency_rows$libelle),
                                               FUN = mean,
                                               na.rm = TRUE)

main_streets_aggregated_by_libelle_decreasing = main_streets_aggregated_by_libelle[order(main_streets_aggregated_by_libelle$x, decreasing = T),]

##Aggregation of main_streets (all years) EXECUTED AND SAVED

# foldername = "./data/find_main_streets"
# filenames = list.files(foldername)
# dataframes = data.frame()
# 
# for(filename in filenames){
#   dataframes = rbind(dataframes, data.frame(readRDS(paste("./data/find_main_streets/",filename,sep=""))))
# }
#   
# main_streets = aggregate(dataframes$q, by=list(dataframes$libelle), FUN=mean, na.rm = TRUE)
# names(main_streets) = c("libelle", "q")
# main_streets = main_streets[order(main_streets$q, decreasing = T),]
# 
# saveRDS(main_streets, file=paste("data/find_main_streets/main_streets_agregated_all_years.rds", sep = ""))

## Libelle function for map

#load main_streets_agregated_all_years
main_streets = readRDS(paste("./data/find_main_streets/main_streets_agregated_all_years.rds"))

str = ""
first = T
i = 0

for (street in main_streets$libelle){
  if (first){
    str = street; first = F
  }
  str = paste(str, " OR ", street, sep="")
  
  if(i>200){
    break
  }
  
  i = i + 1
}

str

writeClipboard(str)


#--- Pour file

str = ""
first = T
i = 0

for (street in main_streets$libelle){
  if (first){
    str = street; first = F
  }
  str = paste(str, "\n\r", street, sep="")
  
  if(i>200){
    break
  }
  
  i = i + 1
}

str

writeClipboard(str)






