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
library(ggplot2)
#----------- End Header ----------------#


####---- Geolocalizing the edges ----####

periph_edges = 52:69

index_to_direction = function(i){
  NE_edges = c(21,22,48,49,58,59,60,61,62,63,64,65) #northeast
  S_edges = c(1,2,15,16,17,34,35,36,37,38,52,53,54,55,56,57) #south
  C_edges = c(3,4,5,13,14,18,19,20,24,25,26,28,29,30,31,32,33,39,40,44,50,51) #center
  W_edges = c(6,7,8,9,10,11,12,23,27,41,42,43,45,46,47,66,67,68,69) #west
  if (i %in% NE_edges){
    return("northeast")
  }
  if (i %in% S_edges){
    return("south")
  }
  if (i %in% C_edges){
    return("center")
  }
  if (i %in% W_edges){
    return("west")
  }
  print("ERROR in index_to_direction: Index not assigned to any region.")
  return(NULL)
}




###---------------- ONE data frame TRAIN --------###

imp_edges_train = readRDS("Data/imp_edges_train.rds")
# imp_all_train = copy()
edgenames = names(imp_edges_train)

# Shared Variables:
min_index = 4
max_index = 17
imp_all_train = imp_edges_train[[1]][,min_index:max_index]

for (i in 1:length(imp_edges_train)){
  edgename = names(imp_edges_train)[i]
  
  index_nb = max_index - min_index + (2*i - 1)
  imp_all_train[[index_nb]] = imp_edges_train[[i]]$nbCar
  names(imp_all_train)[index_nb] = 
    paste0("nbCar_", gsub(" ", "", gsub("-", "TO", edgename)))
    
  index_rate = index_nb + 1
  imp_all_train[[index_rate]] = imp_edges_train[[i]]$rateCar
  names(imp_all_train)[index_rate] = 
    paste0("rateCar_", gsub(" ", "", gsub("-", "TO", edgename)))
}

rm(imp_edges_train)
rm(min_index)
rm(max_index)
rm(index_rate)
rm(index_nb)


df_one_model = data.frame()

for (i in 1:69){
  edgenameTO = gsub(" ", "", gsub("-", "TO", edgenames[i]))
  edge_df = imp_all_train # Creates new dataframe by copying.
  nrows = dim(edge_df)[1]
  edge_df$edgename = rep(edgenameTO, nrows)
  edge_df$periph_indicator = rep(as.integer(i %in% periph_edges), nrows)
  edge_df$position = rep(index_to_direction(i), nrows)
  
  rateCar_name = paste0("rateCar_", edgenameTO)
  nexthour_values = c(edge_df[paste(rateCar_name)][2:nrows,], edge_df[paste(rateCar_name)][nrows,])
  edge_df$nexthour_rateCar = nexthour_values
  
  edge_df = edge_df[, c((dim(edge_df)[2]-3):dim(edge_df)[2], 1:(dim(edge_df)[2]-4))] #reorder columns
  df_one_model = rbind(df_one_model, edge_df)
}

saveRDS(df_one_model, "Data/train_one_model.rds")





###----------- ONE data frame TEST --------###

imp_edges_test = readRDS("Data/imp_edges_test.rds")
# imp_all_test = copy()
edgenames = names(imp_edges_test)

# Shared Variables:
min_index = 4
max_index = 17
imp_all_test = imp_edges_test[[1]][,min_index:max_index]

for (i in 1:length(imp_edges_test)){
  edgename = names(imp_edges_test)[i]
  
  index_nb = max_index - min_index + (2*i - 1)
  imp_all_test[[index_nb]] = imp_edges_test[[i]]$nbCar
  names(imp_all_test)[index_nb] = 
    paste0("nbCar_", gsub(" ", "", gsub("-", "TO", edgename)))
  
  index_rate = index_nb + 1
  imp_all_test[[index_rate]] = imp_edges_test[[i]]$rateCar
  names(imp_all_test)[index_rate] = 
    paste0("rateCar_", gsub(" ", "", gsub("-", "TO", edgename)))
}

rm(imp_edges_test)
rm(min_index)
rm(max_index)
rm(index_rate)
rm(index_nb)

df_one_model = data.frame()

for (i in 1:69){
  edgenameTO = gsub(" ", "", gsub("-", "TO", edgenames[i]))
  edge_df = imp_all_test # Creates new dataframe by copying.
  nrows = dim(edge_df)[1]
  edge_df$edgename = rep(edgenameTO, nrows)
  edge_df$periph_indicator = rep(as.integer(i %in% periph_edges), nrows)
  edge_df$position = rep(index_to_direction(i), nrows)
  
  rateCar_name = paste0("rateCar_", edgenameTO)
  nexthour_values = c(edge_df[[paste(rateCar_name)]][2:nrows], edge_df[[paste(rateCar_name)]][nrows])
  edge_df$nexthour_rateCar = nexthour_values
  
  edge_df = edge_df[1:17516, c(152:155, 1:151)] #reorder columns, chiffres bruts
  df_one_model = rbind(df_one_model, edge_df)
}

saveRDS(df_one_model, "Data/test_one_model.rds")









####----------- GAM: Read and train one model ----------#####

library(mgcv)

one_df = readRDS("Data/train_one_model.rds")
one_df_test = readRDS("Data/test_one_model.rds")


formula = nexthour_rateCar ~ edgename + s(hour, bs="cc") + weekendsIndicator
g = gam(data=one_df, formula = formula)

edge_1_5 = one_df[1:(5*35060),]

formula = nexthour_rateCar ~ s(hour, bs="cc") + 
  edgename + position + #periph_indicator +
  year + month + day + hour + time + 
  weekdays + weekendsIndicator + #covidIndex + 
  temperature + precipitation + winterHolidaysIndicator + 
  summerHolidaysIndicator + bankHolidaysIndicator + 
  nbCar_pontamontTOpontausterlitz + 
  rateCar_pontamontTOpontausterlitz + nbCar_pontausterlitzTOpontamont + 
  rateCar_pontausterlitzTOpontamont + nbCar_saintmichelTOconcorde + 
  rateCar_saintmichelTOconcorde + nbCar_concordeTOchatelet + 
  rateCar_concordeTOchatelet + nbCar_saintmichelTOchatelet + 
  rateCar_saintmichelTOchatelet + nbCar_rondpointetoileTOportemaillot + 
  rateCar_rondpointetoileTOportemaillot + nbCar_rondpointetoileTOporteasnieres + 
  rateCar_rondpointetoileTOporteasnieres + nbCar_porteasnieresTOrondpointetoile + 
  rateCar_porteasnieresTOrondpointetoile + nbCar_porteasnieresTOsaintlazare + 
  rateCar_porteasnieresTOsaintlazare + nbCar_saintlazareTOporteasnieres + 
  rateCar_saintlazareTOporteasnieres + nbCar_saintlazareTOconcorde + 
  rateCar_saintlazareTOconcorde + nbCar_concordeTOsaintlazare + 
  rateCar_concordeTOsaintlazare + nbCar_chateletTOpontausterlitz + 
  rateCar_chateletTOpontausterlitz + nbCar_chateletTOsaintmichel + 
  rateCar_chateletTOsaintmichel + nbCar_denfertrochereauTOsaintmichel + 
  rateCar_denfertrochereauTOsaintmichel + nbCar_saintmichelTOdenfertrochereau + 
  rateCar_saintmichelTOdenfertrochereau + nbCar_denfertrochereauTOporteorleans + 
  rateCar_denfertrochereauTOporteorleans + nbCar_jussieuTObastille + 
  rateCar_jussieuTObastille + nbCar_jussieuTOpontausterlitz + 
  rateCar_jussieuTOpontausterlitz + nbCar_pontausterlitzTOjussieu + 
  rateCar_pontausterlitzTOjussieu + nbCar_strasbourgsaintdenisTOportechapelle + 
  rateCar_strasbourgsaintdenisTOportechapelle + nbCar_portechapelleTOstrasbourgsaintdenis + 
  rateCar_portechapelleTOstrasbourgsaintdenis + nbCar_portemaillotTOrondpointetoile + 
  rateCar_portemaillotTOrondpointetoile + nbCar_jussieuTOsaintmichel + 
  rateCar_jussieuTOsaintmichel + nbCar_saintmichelTOjussieu + 
  rateCar_saintmichelTOjussieu + nbCar_strasbourgsaintdenisTOchatelet + 
  rateCar_strasbourgsaintdenisTOchatelet + nbCar_pontalmaTOconcorde + 
  rateCar_pontalmaTOconcorde + nbCar_concordeTOsaintmichel + 
  rateCar_concordeTOsaintmichel + nbCar_bastilleTOchatelet + 
  rateCar_bastilleTOchatelet + nbCar_pontausterlitzTOchatelet + 
  rateCar_pontausterlitzTOchatelet + nbCar_chateletTOstrasbourgsaintdenis + 
  rateCar_chateletTOstrasbourgsaintdenis + nbCar_saintlazareTOstrasbourgsaintdenis + 
  rateCar_saintlazareTOstrasbourgsaintdenis + nbCar_strasbourgsaintdenisTOsaintlazare + 
  rateCar_strasbourgsaintdenisTOsaintlazare + nbCar_porteorleansTOdenfertrochereau + 
  rateCar_porteorleansTOdenfertrochereau + nbCar_pontausterlitzTOplaceitalie +
  rateCar_pontausterlitzTOplaceitalie +nbCar_placeitalieTOpontausterlitz + 
  rateCar_placeitalieTOpontausterlitz + nbCar_porteitalieTOplaceitalie + 
  rateCar_porteitalieTOplaceitalie + nbCar_placeitalieTOporteitalie + 
  rateCar_placeitalieTOporteitalie + nbCar_chateletTOconcorde + 
  rateCar_chateletTOconcorde + nbCar_bastilleTOpontausterlitz + 
  rateCar_bastilleTOpontausterlitz + nbCar_pontavalTOpontalma + 
  rateCar_pontavalTOpontalma + nbCar_pontalmaTOpontaval + 
  rateCar_pontalmaTOpontaval + nbCar_concordeTOpontalma + 
  rateCar_concordeTOpontalma + nbCar_pontausterlitzTObastille + 
  rateCar_pontausterlitzTObastille + nbCar_rondpointetoileTOpontalma + 
  rateCar_rondpointetoileTOpontalma + nbCar_concordeTOrondpointetoile + 
  rateCar_concordeTOrondpointetoile + nbCar_rondpointetoileTOconcorde + 
  rateCar_rondpointetoileTOconcorde + nbCar_portevincennesTObastille + 
  rateCar_portevincennesTObastille + nbCar_bastilleTOportevincennes + 
  rateCar_bastilleTOportevincennes + nbCar_bastilleTOstrasbourgsaintdenis +
  rateCar_bastilleTOstrasbourgsaintdenis + nbCar_strasbourgsaintdenisTObastille + 
  rateCar_strasbourgsaintdenisTObastille + nbCar_pontavalTOporteorleans + 
  rateCar_pontavalTOporteorleans + nbCar_porteorleansTOpontaval + 
  rateCar_porteorleansTOpontaval + nbCar_porteorleansTOporteitalie + 
  rateCar_porteorleansTOporteitalie + nbCar_porteitalieTOporteorleans + 
  rateCar_porteitalieTOporteorleans + nbCar_porteitalieTOpontamont + 
  rateCar_porteitalieTOpontamont + nbCar_pontamontTOporteitalie + 
  rateCar_pontamontTOporteitalie + nbCar_pontamontTOportevincennes + 
  rateCar_pontamontTOportevincennes + nbCar_portevincennesTOpontamont + 
  rateCar_portevincennesTOpontamont + nbCar_portevincennesTObagnolet + 
  rateCar_portevincennesTObagnolet + nbCar_bagnoletTOportevincennes + 
  rateCar_bagnoletTOportevincennes + nbCar_bagnoletTOportechapelle + 
  rateCar_bagnoletTOportechapelle + nbCar_portechapelleTObagnolet + 
  rateCar_portechapelleTObagnolet + nbCar_portechapelleTOporteasnieres + 
  rateCar_portechapelleTOporteasnieres + nbCar_porteasnieresTOportechapelle + 
  rateCar_porteasnieresTOportechapelle + nbCar_porteasnieresTOportemaillot + 
  rateCar_porteasnieresTOportemaillot + nbCar_portemaillotTOporteasnieres + 
  rateCar_portemaillotTOporteasnieres + nbCar_portemaillotTOpontaval + 
  rateCar_portemaillotTOpontaval + nbCar_pontavalTOportemaillot + 
  rateCar_pontavalTOportemaillot

g = gam(data=edge_1_5, formula = formula)
plot(g, residuals=T, rug=T, se=F, pch=20)
g.forecast = predict(g, newdata = )






















