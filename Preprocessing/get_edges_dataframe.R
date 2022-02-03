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

##################################################################################
# ### Manually defining the graph's edges. Need to specify the number at start!
# 
# no_edges = 69
# edges = vector("list", length=no_edges)
# neigh_after = vector("list", length=no_edges)
# neigh_before = vector("list", length=no_edges)
# 
# i = 1
# edges[[i]] = c(4143, 6105, 4141, 1840, 4141, 1825, 1816)
# names(edges)[i] = "pont amont - pont austerlitz"
# a = c("Quai_de_Bercy_W","Quai_Bercy_Rapee_W")
# neigh_after[[i]] = c("pont austerlitz - place italie", "pont austerlitz - bastille", "pont austerlitz - jussieu", "pont austerlitz - chatelet")
# neigh_before[[i]] = c("porte italie - pont amont", "porte vincennes - pont amont")
# 
# 
# i = 2
# names(edges)[i] = "pont austerlitz - pont amont"
# a=c("Quai_Rapee_Bercy_Y","Quai_de_Bercy_Y","Quai_de_la_Rapee_Y")
# edges[[i]] = c(1817, 4140, 4140, 4140, 1822, 459, 458)
# neigh_after[[i]] = c("pont amont - porte italie", "pont amont - porte vincennes")
# neigh_before[[i]] = c("place italie - pont austerlitz", "bastille - pont austerlitz", "jussieu - pont austerlitz", "chatelet - pont austerlitz")
# 
# 
# #"saint michel - concorde" : CHECKED
# i = 3
# names(edges)[i] = "saint michel - concorde"
# a=c("Quai_Anatole_France","Quai_Conti","Quai_Malaquais","Quai_Voltaire","Quai_Grands_Augustins")
# edges[[i]] =  c(316, 4184, 315, 314, 313, 6297, 70, 74, 73, 178, 206, 178, 204, 68, 6294)
# neigh_after[[i]] = c("concorde - pont alma", "concorde - rond point etoile", "concorde - saint lazare")
# neigh_before[[i]] = c("chatelet - saint michel", "denfert rochereau - saint michel", "jussieu - saint michel")
# 
# 
# #concorde - chatelet
# i = 4
# names(edges)[i] = "concorde - chatelet"
# a=c("VGP_Quai_des_Tuileries","Quai_du_Louvre","Quai_de_la_Megisserie","Quai_Tuileries","Quai_des_Tuileries","VGP_Souterrain_Tuileries","VGP_Quai_Tuileries","Quai_du_Louvre","VGP_Quai_des_Tuileries")
# edges[[i]] = c(1, 2, 3, 4, 5, 6, 7, 215, 6757, 6756, 203, 216, 6758, 217)
# neigh_after[[i]] = c("chatelet - saint michel", "chatelet - pont austerlitz", "chatelet - strasbourg saint denis")
# neigh_before[[i]] = c("pont alma - concorde", "rond point etoile - concorde", "saint lazare - concorde")
# 
# 
# #saint michel - chatelet
# i = 5
# names(edges)[i] = "saint michel - chatelet"
# a=c("Bd_du_Palais","Pt_au_Change","Pt_St_Michel")
# edges[[i]] = c(42, 41, 43, 40)
# neigh_after[[i]] = c("chatelet - concorde", "chatelet - strasbourg saint denis")
# neigh_before[[i]] = c("concorde - saint michel", "denfert rochereau - saint michel", "jussieu - saint michel")
# 
# 
# #rond point etoile - porte maillot - CHECKED
# i = 6
# names(edges)[i] = "rond point etoile - porte maillot"
# a=c("Av_Grande_Armee")
# edges[[i]] = c(4404, 4368, 4366, 4373, 5257, 5258)
# neigh_after[[i]] = c("porte maillot - porte asnieres", "porte maillot - pont aval")
# neigh_before[[i]] = c("concorde - rond point etoile", "porte asnieres - rond point etoile")
# 
# 
# #rond point etoile - porte asnieres # PEU DE DONNEES
# i = 7
# names(edges)[i] = "rond point etoile - porte asnieres"
# edges[[i]] = c(7175, 4418)
# neigh_after[[i]] = c("porte asnieres - porte chapelle", "porte asnieres - porte maillot")
# neigh_before[[i]] = c("concorde - rond point etoile", "porte maillot - rond point etoile")
# 
# 
# #porte asnieres - rond point etoile
# i = 8
# names(edges)[i] = "porte asnieres - rond point etoile"
# edges[[i]] = c(7176, 4416, 4419, 4342, 7168, 7170, 7172, 7174)
# neigh_after[[i]] = c("rond point etoile - concorde", "rond point etoile - pont alma", "rond point etoile - porte maillot")
# neigh_before[[i]] = c("porte chapelle - porte asnieres", "porte maillot - porte asnieres")
# 
# 
# #porte asnieres - saint lazare # PEU DE DONNEES
# i = 9
# names(edges)[i] = "porte asnieres - saint lazare"
# edges[[i]] = c(6799, 6074)
# neigh_after[[i]] = c("saint lazare - concorde", "saint lazare - strasbourg saint denis")
# neigh_before[[i]] = c("porte chapelle - porte asnieres", "porte maillot - porte asnieres")
# 
# 
# #saint lazare - porte asnieres # PEU DE DONNEES
# i = 10
# names(edges)[i] = "saint lazare - porte asnieres"
# edges[[i]] = c(6798, 6073)
# neigh_after[[i]] = c("porte asnieres - porte chapelle", "porte asnieres - porte maillot")
# neigh_before[[i]] = c("concorde - saint lazare", "strasbourg saint denis - saint lazare")
# 
# 
# #saint lazare - concorde
# i = 11
# names(edges)[i] = "saint lazare - concorde"
# edges[[i]] = c(242,244, 246, 237, 239, 6021)
# neigh_after[[i]] = c("concorde - pont alma", "concorde - saint michel", "concorde - chatelet")
# neigh_before[[i]] = c("porte asnieres - saint lazare", "strasbourg saint denis - saint lazare")
# 
# 
# #concorde - saint lazare
# i = 12
# names(edges)[i] = "concorde - saint lazare"
# edges[[i]] = c(243, 245, 247, 238,240, 6020)
# neigh_after[[i]] = c("saint lazare - porte asnieres", "saint lazare - strasbourg saint denis")
# neigh_before[[i]] = c("pont alma - concorde", "saint michel - concorde", "chatelet - concorde")
# 
# 
# #chatelet - pont austerlitz
# i = 13
# names(edges)[i] = "chatelet - pont austerlitz"
# a=c("Quai_de_la_Rapee_W","V.G.P","Quai_Henri_IV","VGP_Souterrain_Henri_IV")
# edges[[i]] = c(11, 12, 438, 645, 440, 442, 444, 446, 6555, 6299, 447, 448)
# neigh_after[[i]] = c("pont austerlitz - place italie", "pont austerlitz - pont amont")
# neigh_before[[i]] = c("concorde - chatelet", "strasbourg saint denis - chatelet")
# 
# 
# #chatelet - saint michel
# i = 14
# names(edges)[i] = "chatelet - saint michel"
# a=c("Petit_Pont","Pt_Notre_Dame","Cite")
# edges[[i]] = c(51, 52, 53, 54, 55)
# neigh_after[[i]] = c("saint michel - denfert rochereau", "saint michel - jussieu")
# neigh_before[[i]] = c("bastille - chatelet", "strasbourg saint denis - chatelet")
# 
# 
# #denfert rochereau - saint michel
# i = 15
# names(edges)[i] = "denfert rochereau - saint michel"
# edges[[i]] = c(583, 585, 587, 589, 591, 593, 649, 651, 1835)
# neigh_after[[i]] = c("saint michel - concorde", "saint michel - chatelet", "saint michel - jussieu")
# neigh_before[[i]] = c("porte orleans - denfert rochereau")
# 
# #saint michel - denfert rochereau
# i = 16
# names(edges)[i] = "saint michel - denfert rochereau"
# edges[[i]] = c(584, 586, 588, 590, 592, 594, 648, 1836, 650)
# neigh_after[[i]] = c("denfert rochereau - porte orleans")
# neigh_before[[i]] = c("concorde - saint michel", "chatelet - saint michel", "jussieu - saint michel")
# 
# 
# #denfert rochereau - porte orleans
# i = 17
# names(edges)[i] = "denfert rochereau - porte orleans"
# edges[[i]] = c(678, 680, 1473, 682, 684, 1532, 1533, 1534, 1535)
# neigh_after[[i]] = c("porte orleans - pont aval", "porte orleans - porte italie")
# neigh_before[[i]] = c("saint michel - denfert rochereau")
# 
# 
# #jussieu - bastille
# i = 18
# names(edges)[i] = "jussieu - bastille"
# a=c("Pt_Sully","Bd_Henri_IV")
# edges[[i]] = c(428, 427, 429, 430, 431, 433, 431, 432)
# neigh_after[[i]] = c("bastille - porte vincennes", "bastille - strasbourg saint denis")
# neigh_before[[i]] = c("saint michel - jussieu")
# 
# 
# #jussieu - pont austerlitz
# i = 19
# names(edges)[i] = "jussieu - pont austerlitz"
# edges[[i]] = c(488, 6167, 518)
# neigh_after[[i]] = c("pont austerlitz - place italie", "pont austerlitz - pont amont", "pont austerlitz - bastille")
# neigh_before[[i]] = c("saint michel - jussieu")
# 
# 
# #pont austerlitz - jussieu
# i = 20
# names(edges)[i] = "pont austerlitz - jussieu"
# edges[[i]] = c(490, 491, 489, 6166)
# neigh_after[[i]] = c("jussieu - saint michel")
# neigh_before[[i]] = c("place italie - pont austerlitz", "pont amont - pont austerlitz")
# 
# 
# #strasbourg saint denis - porte chapelle
# i = 21
# names(edges)[i] = "strasbourg saint denis - porte chapelle"
# a=c("Bd_de_Stasbourg","Bd_de_Strasbourg")
# edges[[i]] = c(1551, 527, 1548, 1548, 1597, 1570, 1610, 1612, 1638, 1640, 1642, 1644, 1646, 1648)
# neigh_after[[i]] = c("porte chapelle - bagnolet", "porte chapelle - porte asnieres")
# neigh_before[[i]] = c("bastille - strasbourg saint denis", "saint lazare - strasbourg saint denis", "chatelet - strasbourg saint denis")
# 
# 
# 
# #porte chapelle - strasbourg saint denis
# i = 22
# names(edges)[i] = "porte chapelle - strasbourg saint denis"
# edges[[i]] = c(1639, 1641, 1643, 1645, 1647, 1649, 1613, 1611, 1571, 1598, 1547, 528)
# neigh_after[[i]] = c("strasbourg saint denis - bastille", "strasbourg saint denis - saint lazare", "strasbourg saint denis - chatelet")
# neigh_before[[i]] = c("bagnolet - porte chapelle", "porte asnieres - porte chapelle")
# 
# 
# #porte maillot - rond point etoile - CHECKED
# i = 23
# names(edges)[i] = "porte maillot - rond point etoile"
# a=c("Av_de_la_Grande_Armee")
# edges[[i]] = c(5259, 5260, 4375, 4364, 4371, 4405)
# neigh_after[[i]] = c("rond point etoile - concorde", "rond point etoile - pont alma", "rond point etoile - porte asnieres")
# neigh_before[[i]] = c("porte asnieres - porte maillot", "pont aval - porte maillot")
# 
# 
# #jussieu - saint michel
# i = 24
# names(edges)[i] = "jussieu - saint michel"
# a=c("Quai_Tournelle","Quai_Montebello","Quai_St_Michel")
# edges[[i]] = c(6296, 210, 424, 424, 65, 6295, 64, 67, 66)
# neigh_after[[i]] = c("saint michel - concorde", "saint michel - chatelet", "saint michel - denfert rochereau")
# neigh_before[[i]] = c("pont austerlitz - jussieu")
# 
# 
# #saint michel - jussieu
# i = 25
# names(edges)[i] = "saint michel - jussieu"
# edges[[i]] = c(164, 166, 207, 208, 421, 1225)
# neigh_after[[i]] = c("jussieu - pont austerlitz", "jussieu - bastille")
# neigh_before[[i]] = c("concorde - saint michel", "denfert rochereau - saint michel")
# 
# 
# #strasbourg saint denis - chatelet
# i = 26
# names(edges)[i] = "strasbourg saint denis - chatelet"
# a=c("Renard","Beaubourg")
# edges[[i]] = c(48, 49, 156, 47, 533, 534, 6536)
# neigh_after[[i]] = c("chatelet - concorde", "chatelet - saint michel", "chatelet - pont austerlitz")
# neigh_before[[i]] = c("saint lazare - strasbourg saint denis", "porte chapelle - strasbourg saint denis")
# 
# 
# #pont alma - concorde
# i = 27
# names(edges)[i] = "pont alma - concorde"
# a=c("Souterrain_Invalides_VGP")
# edges[[i]] = c(4624, 4621, 271, 4496, 4400, 6793, 4392, 321, 319, 4642, 4622, 5874, 273)
# neigh_after[[i]] = c("concorde - saint lazare", "concorde - saint michel", "concorde - chatelet")
# neigh_before[[i]] = c("pont aval - pont alma", "rond point etoile - pont alma")
# 
# 
# #concorde - saint michel
# i = 28
# names(edges)[i] = "concorde - saint michel"
# edges[[i]] = c(327, 330, 331, 332, 333, 334, 335, 336, 337, 338, 6188, 195, 196, 157, 158, 159, 160, 161, 162)
# neigh_after[[i]] = c("saint michel - denfert rochereau", "saint michel - jussieu")
# neigh_before[[i]] = c("pont alma - concorde", "rond point etoile - concorde", "saint lazare - concorde")
# 
# 
# #bastille - chatelet
# i = 29
# names(edges)[i] = "bastille - chatelet"
# a=c("St_Antoine")
# edges[[i]] = c(515, 375, 372, 373, 374, 376)
# neigh_after[[i]] = c("chatelet - concorde", "chatelet - saint michel", "chatelet - strasbourg saint denis")
# neigh_before[[i]] = c("porte vincennes - bastille", "strasbourg saint denis - bastille", "pont austerlitz - bastille")
# 
# 
# #pont austerlitz - chatelet
# i = 30
# names(edges)[i] = "pont austerlitz - chatelet"
# a=c("Bd_Morland")
# edges[[i]] = c(664, 439, 443, 445, 13, 14, 15, 450, 449, 451, 6575)
# neigh_after[[i]] = c("chatelet - concorde", "chatelet - saint michel", "chatelet - strasbourg saint denis")
# neigh_before[[i]] = c("place italie - pont austerlitz", "pont amont - pont austerlitz")
# 
# 
# #chatelet - strasbourg saint denis
# i = 31
# names(edges)[i] = "chatelet - strasbourg saint denis"
# a=c("Bd_Sebastopol")
# edges[[i]] = c(522, 526, 524, 519, 1218, 45, 523, 520, 525, 521, 46)
# neigh_after[[i]] = c("strasbourg saint denis - saint lazare", "strasbourg saint denis - porte chapelle")
# neigh_before[[i]] = c("concorde - chatelet", "saint michel - chatelet", "pont austerlitz - chatelet")
# 
# 
# #saint lazare - strasbourg saint denis - PEU DE DONNEES
# i = 32
# names(edges)[i] = "saint lazare - strasbourg saint denis"
# a=c("4_Septembre","Pl_de_la_Bourse_cote_Reaumur")
# edges[[i]] = c(6788, 6789, 6787, 4060, 1963, 1977, 1964, 1978, 1965)
# neigh_after[[i]] = c("strasbourg saint denis - bastille", "strasbourg saint denis - porte chapelle", "strasbourg saint denis - chatelet")
# neigh_before[[i]] = c("porte asnieres - saint lazare", "concorde - saint lazare")
# 
# 
# #strasbourg saint denis - saint lazare
# i = 33
# names(edges)[i] = "strasbourg saint denis - saint lazare"
# edges[[i]] = c(550, 551, 808, 807, 5118, 5120, 1947, 1946, 5743, 4069, 1944, 6008, 6009)
# neigh_after[[i]] = c("saint lazare - porte asnieres", "saint lazare - concorde")
# neigh_before[[i]] = c("bastille - strasbourg saint denis", "porte chapelle - strasbourg saint denis", "chatelet - strasbourg saint denis")
# 
# 
# #porte orleans - denfert rochereau
# i = 34
# names(edges)[i] = "porte orleans - denfert rochereau"
# a=c("Pte_d'Orleans")
# edges[[i]] = c(714, 1213, 713, 712, 685, 683, 1474, 681, 679)
# neigh_after[[i]] = c("denfert rochereau - saint michel")
# neigh_before[[i]] = c("pont aval - porte orleans", "porte italie - porte orleans")
# 
# 
# #pont austerlitz - place italie
# i = 35
# names(edges)[i] = "pont austerlitz - place italie"
# edges[[i]] = c(503,505,508, 510, 512, 5065,5066,5067)
# neigh_after[[i]] = c("place italie - porte italie")
# neigh_before[[i]] = c("pont amont - pont austerlitz", "bastille - pont austerlitz", "jussieu - pont austerlitz", "chatelet - pont austerlitz")
# 
# #place italie - pont austerlitz
# i = 36
# names(edges)[i] = "place italie - pont austerlitz"
# edges[[i]] = c(5068,5070,5071,511,509,507,506,504)
# neigh_after[[i]] = c("pont austerlitz - pont amont", "pont austerlitz - bastille", "pont austerlitz - jussieu", "pont austerlitz - chatelet")
# neigh_before[[i]] = c("porte italie - place italie")
# 
# #porte italie - place italie
# i = 37
# names(edges)[i] = "porte italie - place italie"
# edges[[i]] = c(813,5035,6245,5031,5034,5027,6119,5028)
# neigh_after[[i]] = c("place italie - pont austerlitz")
# neigh_before[[i]] = c("porte orleans - porte italie", "pont amont - porte italie")
# 
# 
# #place italie - porte italie
# i = 38
# names(edges)[i] = "place italie - porte italie"
# edges[[i]] = c(5025,6118,5026,5029,5030,5036,812)
# neigh_after[[i]] = c("porte italie - porte orleans", "porte italie - pont amont")
# neigh_before[[i]] = c("pont austerlitz - place italie")
# 
# 
# 
# #chatelet - concorde
# i = 39
# names(edges)[i] = "chatelet - concorde"
# a=c("Rivoli")
# edges[[i]] = c(279, 371, 26, 25, 31, 31, 281, 21, 28, 32, 282, 34, 35, 33, 283, 176, 29, 370, 27, 30, 4189, 22, 280)
# neigh_after[[i]] = c("concorde - pont alma", "concorde - rond point etoile", "concorde - saint lazare")
# neigh_before[[i]] = c("saint michel - chatelet", "bastille - chatelet", "pont austerlitz - chatelet", "strasbourg saint denis - chatelet")
# 
# 
# #bastille - pont austerlitz
# i = 40
# names(edges)[i] = "bastille - pont austerlitz"
# a=c("Bd_Bourdon")
# edges[[i]] = c(6576, 454, 453, 1221, 6239)
# neigh_after[[i]] = c("pont austerlitz - place italie", "pont austerlitz - pont amont")
# neigh_before[[i]] = c("porte vincennes - bastille", "strasbourg saint denis - bastille")
# 
# 
# #pont aval - pont alma
# i = 41
# names(edges)[i] = "pont aval - pont alma"
# a=c("Quai_St_Exupery")
# edges[[i]] = c(1226, 1228,1230,6185,44814479,4477,4473,4468,4457,4460,4434,5053,4431,4428,6433,6432,4697,4709,4528,4535,6389,6390,4561,4572,4573,4563,4578,4581,4643)
# neigh_after[[i]] = c("pont alma - concorde")
# neigh_before[[i]] = c("porte orleans - pont aval", "porte maillot - pont aval")
# 
# 
# 
# #pont alma - pont aval
# i = 42
# names(edges)[i] = "pont alma - pont aval"
# edges[[i]] = c(4645,4580,4575,4543,4547,4532,6161,4530,4524,4521,4499,4505,1227,1229,1231,4734,4480,4478,4476,4471,4465,4454,4451,4440,4444,4429,6796,4424)
# neigh_after[[i]] = c("pont aval - porte orleans", "pont aval - porte maillot")
# neigh_before[[i]] = c("concorde - pont alma", "rond point etoile - pont alma")
# 
# 
# #concorde - pont alma
# i = 43
# names(edges)[i] = "concorde - pont alma"
# a=c("Cours Albert 1er_VGP")
# edges[[i]] = c(274, 5873, 4407, 4414, 4623, 4618, 272, 5054, 4494, 6792, 4398, 5055, 322, 320, 318)
# neigh_after[[i]] = c("pont alma - pont aval")
# neigh_before[[i]] = c("saint lazare - concorde", "saint michel - concorde", "chatelet - concorde")
# 
# 
# #pont austerlitz - bastille
# i = 44
# names(edges)[i] = "pont austerlitz - bastille"
# a=c("Bd_Bastille")
# edges[[i]] = c(468, 467, 469)
# neigh_after[[i]] = c("bastille - porte vincennes", "bastille - strasbourg saint denis")
# neigh_before[[i]] = c("place italie - pont austerlitz", "pont amont - pont austerlitz", "chatelet - pont austerlitz")
# 
# 
# #rond point etoile - pont alma
# i = 45
# names(edges)[i] = "rond point etoile - pont alma"
# a=c("Av_Marceau")
# edges[[i]] = c(4365, 4372, 6285, 4369, 4372, 6609, 4455, 4452)
# neigh_after[[i]] = c("pont alma - pont aval", "pont alma - concorde")
# neigh_before[[i]] = c("porte maillot - rond point etoile", "porte asnieres - rond point etoile")
# 
# 
# 
# #concorde - rond point etoile
# i = 46
# names(edges)[i] = "concorde - rond point etoile"
# edges[[i]] = c(224,269,4282,4279,4275,4272,4262,4265,4399,4401)
# neigh_after[[i]] = c("rond point etoile - porte maillot", "rond point etoile - porte asnieres")
# neigh_before[[i]] = c("saint lazare - concorde", "saint michel - concorde", "chatelet - concorde")
# 
# #rond point etoile - concorde
# i = 47
# names(edges)[i] = "rond point etoile - concorde"
# edges[[i]] = c(4397,4267,4264,4274,4276,4281,4284,270,223,6815)
# neigh_after[[i]] = c("concorde - saint lazare", "concorde - saint michel", "concorde - chatelet")
# neigh_before[[i]] = c("porte maillot - rond point etoile", "porte asnieres - rond point etoile")
# 
# 
# #porte vincennes - bastille
# i = 48
# names(edges)[i] = "porte vincennes - bastille"
# edges[[i]] = c(6713, 910,6714,4928,4929,4931,4932,4689,4695,4684,4682,4732,4679,4675,4661,4667,5116)
# neigh_after[[i]] = c("bastille - strasbourg saint denis", "bastille - pont austerlitz", "bastille - chatelet")
# neigh_before[[i]] = c("pont amont - porte vincennes", "bagnolet - porte vincennes")
# 
# 
# #bastille - porte vincennes
# i = 49
# names(edges)[i] = "bastille - porte vincennes"
# edges[[i]] = c(5114, 4665,4662, 4680, 4731,4688,4686,4693,4691, 4934,4918, 4922, 4924,4926, 909, 6426, 6712, 914)
# neigh_after[[i]] = c("porte vincennes - pont amont", "porte vincennes - bagnolet")
# neigh_before[[i]] = c("strasbourg saint denis - bastille", "pont austerlitz - bastille", "jussieu - bastille")
# 
# 
# #bastille - strasbourg saint denis
# i = 50
# names(edges)[i] = "bastille - strasbourg saint denis"
# edges[[i]] = c(401, 403, 405, 407, 1380, 1378, 1374, 1372, 1368, 1366, 1364, 547, 1211, 6742, 1240, 1245)
# neigh_after[[i]] = c("strasbourg saint denis - saint lazare", "strasbourg saint denis - porte chapelle")
# neigh_before[[i]] = c("porte vincennes - bastille", "pont austerlitz - bastille", "jussieu - bastille")
# 
# 
# #strasbourg saint denis - bastille
# i = 51
# names(edges)[i] = "strasbourg saint denis - bastille"
# edges[[i]] = c(6744,6745,6746,1363,1365,1367,1371,1373,1377,1379,408,406,404,402,400)
# neigh_after[[i]] = c("bastille - porte vincennes", "bastille - pont austerlitz", "bastille - chatelet")
# neigh_before[[i]] = c("saint lazare - strasbourg saint denis", "porte chapelle - strasbourg saint denis")
# 
# 
# #pont aval - porte orleans
# i = 52
# names(edges)[i] = "pont aval - porte orleans"
# a=c("PE_Parc_Expo_Ouest","PE_Plaine","PE_Chatillon")
# edges[[i]] = c(5175, 5177, 5182)
# neigh_after[[i]] = c("porte orleans - denfert rochereau", "porte orleans - porte italie")
# neigh_before[[i]] = c("pont alma - pont aval", "porte maillot - pont aval")
# 
# 
# #porte orleans - pont aval
# i = 53
# names(edges)[i] = "porte orleans - pont aval"
# a=c("PI_Parc_Expo_Ouest","PI_Plaine","PI_Chatillon")
# edges[[i]] = c(5208, 5205, 5186)
# neigh_after[[i]] = c("pont aval - pont alma", "pont aval - porte maillot")
# neigh_before[[i]] = c("denfert rochereau - porte orleans", "porte italie - porte orleans")
# 
# 
# #porte orleans - porte italie
# i = 54
# names(edges)[i] = "porte orleans - porte italie"
# a=c("PE_Gentilly")
# edges[[i]] = c(5264)
# neigh_after[[i]] = c("porte italie - place italie", "porte italie - pont amont")
# neigh_before[[i]] = c("denfert rochereau - porte orleans", "pont aval - porte orleans")
# 
# 
# #porte italie - porte orleans
# i = 55
# names(edges)[i] = "porte italie - porte orleans"
# a=c("PI_Gentilly")
# edges[[i]] = c(5312)
# neigh_after[[i]] = c("porte orleans - denfert rochereau", "porte orleans - pont aval")
# neigh_before[[i]] = c("place italie - porte italie", "pont amont - porte italie")
# 
# 
# #porte italie - pont amont
# i = 56
# names(edges)[i] = "porte italie - pont amont"
# a=c("PE_Haubans")
# edges[[i]] = c(5279)
# neigh_after[[i]] = c("pont amont - pont austerlitz", "pont amont - porte vincennes")
# neigh_before[[i]] = c("place italie - porte italie", "porte orleans - porte italie")
# 
# 
# #pont amont - porte italie
# i = 57
# names(edges)[i] = "pont amont - porte italie"
# a=c("PI_Haubans")
# edges[[i]] = c(5298)
# neigh_after[[i]] = c("porte italie - place italie", "porte italie - porte orleans")
# neigh_before[[i]] = c("pont austerlitz - pont amont", "porte vincennes - pont amont")
# 
# 
# #pont amont - porte vincennes
# i = 58
# names(edges)[i] = "pont amont - porte vincennes"
# a=c("PE_Reuilly")
# edges[[i]] = c(5325)
# neigh_after[[i]] = c("porte vincennes - bastille", "porte vincennes - bagnolet")
# neigh_before[[i]] = c("pont austerlitz - pont amont", "porte italie - pont amont")
# 
# 
# 
# #porte vincennes - pont amont
# i = 59
# names(edges)[i] = "porte vincennes - pont amont"
# a=c("PI_Reuilly")
# edges[[i]] = c(5338)
# neigh_after[[i]] = c("pont amont - pont austerlitz", "pont amont - porte italie")
# neigh_before[[i]] = c("bastille - porte vincennes", "bagnolet - porte vincennes")
# 
# 
# 
# #porte vincennes - bagnolet
# i = 60
# names(edges)[i] = "porte vincennes - bagnolet"
# a=c("PE_Louis_Lumiere") # et autres!
# edges[[i]] = c(5347, 5330, 5331, 5347, 5350, 5351)
# neigh_after[[i]] = c("bagnolet - porte chapelle")
# neigh_before[[i]] = c("bastille - porte vincennes", "pont amont - porte vincennes")
# 
# 
# #bagnolet - porte vincennes
# i = 61
# names(edges)[i] = "bagnolet - porte vincennes"
# a=c("PI_Louis_Lumiere")
# edges[[i]] = c(5360)
# neigh_after[[i]] = c("porte vincennes - bastille", "porte vincennes - pont amont")
# neigh_before[[i]] = c("porte chapelle - bagnolet")
# 
# #bagnolet - porte chapelle
# i = 62
# names(edges)[i] = "bagnolet - porte chapelle"
# a=c("PE_Chaumont","PE_Aubervilliers")
# edges[[i]] = c(5363, 5369)
# neigh_after[[i]] = c("porte chapelle - strasbourg saint denis", "porte chapelle - porte asnieres")
# neigh_before[[i]] = c("porte vincennes - bagnolet")
# 
# 
# #porte chapelle - bagnolet
# i = 63
# names(edges)[i] = "porte chapelle - bagnolet"
# a=c("PI_Chaumont","PI_Aubervilliers")
# edges[[i]] = c(5387, 5380)
# neigh_after[[i]] = c("bagnolet - porte vincennes")
# neigh_before[[i]] = c("strasbourg saint denis - porte chapelle", "porte asnieres - porte chapelle")
# 
# 
# #porte chapelle - porte asnieres
# i = 64
# names(edges)[i] = "porte chapelle - porte asnieres"
# a=c("PE_Batignolles","PE_Poissonniers")
# edges[[i]] = c(5406, 5390)
# neigh_after[[i]] = c("porte asnieres - rond point etoile", "porte asnieres - saint lazare", "porte asnieres - porte maillot")
# neigh_before[[i]] = c("strasbourg saint denis - porte chapelle", "bagnolet - porte chapelle")
# 
# 
# #porte asnieres - porte chapelle
# i = 65
# names(edges)[i] = "porte asnieres - porte chapelle"
# a=c("PI_Batignolles","PI_Poissonniers")
# edges[[i]] = c(5411, 5395)
# neigh_after[[i]] = c("porte chapelle - strasbourg saint denis", "porte chapelle - bagnolet")
# neigh_before[[i]] = c("rond point etoile - porte asnieres", "saint lazare - porte asnieres", "porte maillot - porte asnieres")
# 
# 
# #porte asnieres - porte maillot
# i = 66
# names(edges)[i] = "porte asnieres - porte maillot"
# a=c("PE_Champerret") # et plus.
# edges[[i]] = c(5416, 5418, 5420, 5422, 5423)
# neigh_after[[i]] = c("porte maillot - rond point etoile", "porte maillot - pont aval")
# neigh_before[[i]] = c("saint lazare - porte asnieres", "porte chapelle - porte asnieres")
# 
# 
# #porte maillot - porte asnieres
# i = 67
# names(edges)[i] = "porte maillot - porte asnieres"
# a=c("PI_Champerret")
# edges[[i]] = c(5419)
# neigh_after[[i]] = c("porte asnieres - rond point etoile", "porte asnieres - saint lazare", "porte asnieres - porte chapelle")
# neigh_before[[i]] = c("rond point etoile - porte maillot", "pont aval - porte maillot")
# 
# 
# #porte maillot - pont aval
# i = 68
# names(edges)[i] = "porte maillot - pont aval"
# a=c("PE_Parc_Princes")
# edges[[i]] = c(5446)
# neigh_after[[i]] = c("pont aval - pont alma", "pont aval - porte orleans")
# neigh_before[[i]] = c("rond point etoile - porte maillot", "porte asnieres - porte maillot")
# 
# 
# #pont aval - porte maillot
# i = 69
# names(edges)[i] = "pont aval - porte maillot"
# a=c("PI_Parc_Princes") # et plus car donnees manquantes
# edges[[i]] = c(5451, 5217, 5449, 5450, 5452, 5453, 5454, 5455, 5440, 5439, 5436, 5434, 5432)
# neigh_after[[i]] = c("porte maillot - rond point etoile", "porte maillot - porte asnieres")
# neigh_before[[i]] = c("pont alma - pont aval", "porte orleans - pont aval")
# 
# names(neigh_after) = names(edges)
# names(neigh_before) = names(edges)
# saveRDS(object = edges, file = "Data/edges.rds")
# saveRDS(object = neigh_after, file = "Data/neigh_after.rds")
# saveRDS(object = neigh_before, file = "Data/neigh_before.rds")

########################################################################################
##### Now build DATAFRAME with the above edges.

edges = readRDS("Data/edges.rds")

years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
ptm <- proc.time()
for (year in years){
  edges_dfs = vector("list", length(edges))
  names(edges_dfs) = names(edges)
  for (i in 1:length(edges_dfs)){
    edges_dfs[[i]] <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(edges_dfs[[i]]) <- c("iu_ac", "t_1h", "q", "k", "etat_barre")
  }
  foldername = paste("Data/data_cleaner_", year, sep="")
  filenames = list.files(foldername)
  # i = 0
  for (filename in filenames){
    # if (i == 3){break}
    filepath = paste(foldername, "/", filename, sep = "")
    week_df = readRDS(filepath) # Reads the dataframe we had earlier.
    for (edgename in names(edges)){
      temp_df = filter(week_df, iu_ac %in% edges[edgename][[1]])
      agg_edge = aggregate(temp_df, by=list(temp_df$t_1h), FUN=mean, na.rm = TRUE)[c("iu_ac", "t_1h", "q", "k", "etat_barre")]
      edges_dfs[edgename][[1]] = rbind(edges_dfs[edgename][[1]],agg_edge)
    }
    i = i+1
  }
  saveRDS(edges_dfs, file=paste0("Data/data_agg69_plain/edges_dfs_", year,".rds"))
}
proc.time() - ptm

#utilisateur     système      écoulé 
#1108.59       50.57     1177.37 


# Now throw it all into a single dataframe again:
years = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)
ptm <- proc.time()
edges = readRDS("Data/edges.rds")
edges_dfs_allyrs = vector("list", length(edges))
names(edges_dfs_allyrs) = names(edges)
for (i in 1:length(edges_dfs_allyrs)){
  edges_dfs_allyrs[[i]] <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(edges_dfs_allyrs[[i]]) <- c("iu_ac", "t_1h", "q", "k", "etat_barre")
}
for (year in years){
  edges_dfs_year = readRDS(paste0("Data/data_agg69_plain/edges_dfs_", year, ".rds"))
  for (i in 1:length(edges_dfs_year)){
    edges_dfs_allyrs[[i]] = rbind(edges_dfs_allyrs[[i]], edges_dfs_year[[i]])
  }
}
proc.time() - ptm

# Fix double days at year change (120 hours too many) LONG EXEC TIME
for (j in 1:length(edges_dfs_allyrs)){
  print(names(edges_dfs_allyrs)[j])
  edges_dfs_allyrs[[j]] = aggregate(edges_dfs_allyrs[[j]], 
                 by = list(edges_dfs_allyrs[[j]]$t_1h), FUN = mean, na.rm = TRUE)
}

saveRDS(edges_dfs_allyrs, "Data/data_agg69_plain/edges_dfs_allyrs.rds")



