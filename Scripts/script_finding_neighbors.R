library(tidyverse)

after = "pont austerlitz"
find_after = function(after){
  aretes = read.delim("Graphe/liste_aretes.txt", header = FALSE)
  str2 = ''
  for (elem in aretes$V1){
    if (substr(elem, 1, nchar(after)) == after){
      str2 = paste0(str2, "\"",  elem, "\", ")
    }
  }
  writeClipboard(substr(str2, 1, nchar(str2)-2))
  return(str2)
}
find_after(after)

before = "pont amont"
find_before = function(before){
  aretes = read.delim("Graphe/liste_aretes.txt", header = FALSE)
  str2 = ''
  for (elem in aretes$V1){
    if (substr(elem, nchar(elem) - nchar(before) + 1, nchar(elem)) == before){
      str2 = paste0(str2, "\"",  elem, "\", ")
    }
  }
  writeClipboard(substr(str2, 1, nchar(str2)-2))
  return(str2)
}
find_before(before)

#"pont amont - pont austerlitz"
find_after_and_before = function(edgename){
  aretes = read.delim("Graphe/liste_aretes.txt", header = FALSE)$V1
  split = str_split(edgename, " - ")
  before = split[[1]][1]
  after = split[[1]][2]
  reverse = paste0(after, " - ", before)
  str_after = ""
  for (elem in aretes){
    if (substr(elem, 1, nchar(after)) == after && elem != edgename && elem != reverse){
      str_after = paste0(str_after, "\"",  elem, "\", ")
    }
  }
  str_after = substr(str_after, 1, nchar(str_after)-2)
  str_before = ''
  for (elem in aretes){
    if (substr(elem, nchar(elem) - nchar(before) + 1, nchar(elem)) == before && elem != edgename && elem != reverse){
      str_before = paste0(str_before, "\"",  elem, "\", ")
    }
  }
  str_before = substr(str_before, 1, nchar(str_before)-2)
  firstline = paste0("neigh_after[i] = c(", str_after,")")
  secondline = paste0("neigh_before[i] = c(", str_before,")")
  finalstr = paste0(firstline, "\n", secondline)
  writeClipboard(finalstr)
  return(finalstr)
}
