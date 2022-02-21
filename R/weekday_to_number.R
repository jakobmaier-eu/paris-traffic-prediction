daysToNumber <- function(day){
  if(day == "lundi"){return(1)}
  if(day == "mardi"){return(2)}
  if(day == "mercredi"){return(3)}
  if(day == "jeudi"){return(4)}
  if(day == "vendredi"){return(5)}
  if(day == "samedi"){return(6)}
  if(day == "dimanche"){return(7)}
}

for (i in dim(df)[1]){
  df$weekdays[i] = daysToNumber(df$weekdays[i])
}

