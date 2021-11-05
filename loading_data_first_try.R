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

########

edges_numbers <- vector("list", length(edges_name))
edges_name <- vector("list", length(edges_name))

i = 1

edges_name[[i]] = "pont amont - pont austerlitz"
a = c("Quai_de_Bercy_W","Quai_Bercy_Rapee_W")
edges_numbers[[i]] = c(4143, 6105, 4141, 1840, 4141, 1825, 1816)
i = i+1

#TODO

#pont austerlitz - pont amont
a=c("Quai_Rapee_Bercy_Y","Quai_de_Bercy_Y","Quai_de_la_Rapee_Y")
c(1817, 4140, 4140, 4140, 1822, 459, 458)

#saint michel - concorde
a=c("Quai_Anatole_France","Quai_Conti","Quai_Malaquais","Quai_Voltaire","Quai_Grands_Augustins")
c(316, 4184, 315, 314, 313, 316, 6297, 69, 70, 74, 73, 178, 206, 178, 204, 68, 6294)

#concorde - chatelet
a=c("VGP_Quai_des_Tuileries","Quai_du_Louvre","Quai_de_la_Megisserie","Quai_Tuileries","Quai_des_Tuileries","VGP_Souterrain_Tuileries","VGP_Quai_Tuileries","Quai_du_Louvre","VGP_Quai_des_Tuileries")
c(217, 217, 4, 2, 1, 4, 3, 3, 6, 5, 7, 215, 6757, 6756, 203, 216, 6758, 4, 2, 1, 4, 3, 3, 217, 217)

#saint michel - chatelet
a=c("Bd_du_Palais","Pt_au_Change","Pt_St_Michel")
c(42, 41, 43, 40)

#rond point etoile - porte maillot
a=c("Av_Grande_Armee")
c(5259, 5257, 5260, 5258)

#rond point etoile - porte asnieres
c(7175, 4418)

#porte asnieres - rond point etoile
c(7176, 4416)

#porte asnieres - saint lazare
c(6799, 6074)

#saint lazare - porte asnieres
c(6798, 6073)

#saint lazare - concorde
c(244, 239)

#concorde - saint lazare
c(245, 240)

#chatelet - pont austerlitz
a=c("Quai_de_la_Rapee_W","V.G.P","Quai_Henri_IV","VGP_Souterrain_Henri_IV")
c(442, 12, 461, 6168, 462, 460, 848, 6299, 446, 447, 6555, 448, 437, 1538)

#chatelet - saint michel
a=c("Petit_Pont","Pt_Notre_Dame","Cite")
c(55, 52, 53, 54)

#denfert rochereau - saint michel
c(585, 589)

#saint michel - denfert rochereau
c(584, 590)

#denfert rochereau - porte orleans
c(1532, 680, 1535)

#jussieu - bastille
a=c("Pt_Sully","Bd_Henri_IV")
c(428, 427, 429, 430, 431, 433, 431, 432)

#jussieu - pont austerlitz
c(518)

#pont austerlitz - jussieu
c(490)

#strasbourg saint denis - porte de la chapelle
a=c("Bd_de_Stasbourg","Bd_de_Strasbourg")
c(1644, 1551, 527, 1548, 1576, 1548)


#porte de la chapelle - strasbourg saint denis
c(1645)

#porte maillot - rond point etoile
a=c("Av_de_la_Grande_Armee")
c(4364, 4366, 4373, 4375, 4371, 4405, 4404, 4368)

#jussieu - saint michel
a=c("Quai_Tournelle","Quai_Montebello","Quai_St_Michel")
c(6296, 210, 424, 424, 65, 6295, 64, 67, 66)

#saint michel - jussieu
c(208)

#strasbourg saint denis - chatelet
a=c("Renard","Beaubourg")
c(48, 49, 156, 47, 533, 534, 6536)

#pont alma - concorde
a=c("Souterrain_Invalides_VGP")
c(271, 6793, 4621)

#concorde - saint michel
c(331, 161)

#bastille - chatelet
a=c("St_Antoine")
c(515, 375, 372, 373, 374, 376)

#pont austerlitz - chatelet
a=c("Bd_Morland")
c(443, 13, 450, 449, 451, 6575)

#chatelet - strasbourg saint denis
a=c("Bd_Sebastopol")
c(522, 526, 524, 519, 1218, 45, 523, 520, 525, 521, 46)

#saint lazare - strasbourg saint denis
a=c("4_Septembre","Pl_de_la_Bourse_cote_Reaumur")
c(6788, 6789, 6787, 4060, 1963, 1977, 1964, 1978, 1965)

#strasbourg saint denis - saint lazare
c(5118, 1946, 808, 5120)

#porte orleans - denfert rochereau
a=c("Pte_d'Orleans")
c(714, 681, 712, 773, 925, 926)


#pont austerlitz - place italie
c(505, 5067)

#place italie - pont austerlitz
c(506, 5068)

#porte italie - place italie
c(813, 6119)

#place italie - porte italie
c(812, 6118)

#chatelet - concorde
a=c("Rivoli")
c(279, 371, 26, 25, 31, 31, 281, 21, 28, 32, 282, 34, 35, 33, 283, 176, 29, 370, 27, 30, 4189, 22, 280)

#bastille - pont austerlitz 
a=c("Bd_Bourdon")
c(6576, 454, 453)

#pont aval - pont alma
a=c("Quai_St_Exupery")
c(4581, 4481, 1228, 1075, 4521, 4471, 4431, 4457, 6433, 4658)


#pont alma - pont aval
c(4580, 4480, 1229, 1074, 4522, 4473, 6796, 4454)

#concorde - pont alma
a=c("Cours Albert 1er_VGP")
c(272, 4398, 4618)

#pont austerlitz - bastille
a=c("Bd_Bastille")
c(468, 467, 469)

#round point etoile - pont alma
a=c("Av_Marceau")
c(4365, 4372, 6285, 4369, 4372, 6609, 4455, 4452)

#concorde - rond point etoile
c(4399, 224)

#rond point etoile - concorde
c(4269, 223)

#porte de vincennes - bastille
c(4928, 6713)

#bastille - porte de vincennes
c(4926, 6712)

#bastille - strasbourg saint denis
c(1380, 1374, 1366)

#strasbourg saint denis - bastille
c(1379, 1373, 1365)

#pont aval - porte orleans
a=c("PE_Parc_Expo_Ouest","PE_Plaine","PE_Chatillon")
c(5175, 5177, 5182)

#porte orleans - pont aval
a=c("PI_Parc_Expo_Ouest","PI_Plaine","PI_Chatillon")
c(5208, 5205, 5186)

#porte orleans - porte italie
a=c("PE_Gentilly")
c(5264)

#porte italie - porte orleans
a=c("PI_Gentilly")
c(5312)

#porte italie - pont amont
a=c("PE_Haubans")
c(5279)

#pont amont - porte italie
a=c("PI_Haubans")
c(5298)

#pont amont - porte vincennes
a=c("PE_Reuilly")
c(5325)

#porte vincennes - pont amont
a=c("PI_Reuilly")
c(5338)

#porte vincennes - bagnolet
a=c("PE_Louis_Lumiere")
c(5347)

#bagnolet - porte vincennes
a=c("PI_Louis_Lumiere")
c(5360)

#bagnolet - porte de la chapelle
a=c("PE_Chaumont","PE_Aubervilliers")
c(5363, 5369)

#porte de la chapelle - bagnolet
a=c("PI_Chaumont","PI_Aubervilliers")
c(5387, 5380)

#porte de la chapelle - porte asnieres
a=c("PE_Batignolles","PE_Poissonniers")
c(5406, 5390)

#porte asnieres - porte de la chapelle
a=c("PI_Batignolles","PI_Poissonniers")
c(5411, 5395)

#porte asnieres - porte maillot
a=c("PE_Champerret")
c(5418)

#porte maillot - porte asnieres
a=c("PI_Champerret")
c(5419)

#porte maillot - pont aval
a=c("PE_Parc_Princes")
c(5446)

#pont aval - porte maillot
a=c("PI_Parc_Princes")
c(5451)



