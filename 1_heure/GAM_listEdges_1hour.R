library(ProjetML1)
library(mgcv)

# Prepare data first: 
edge_index = "concorde - saint michel" # We study an arbitrary edge
df_train = data_train[[paste(edge_index)]]
df_test = data_test[[paste(edge_index)]]

df_train$weekdays <- unlist(lapply(X = df_train$weekdays, FUN = daysToNumber))
df_test$weekdays <- unlist(lapply(X = df_test$weekdays, FUN = daysToNumber))


formula = rateCar ~ 
  state + 
  # year + 
  month + 
  day + 
  s(hour, bs="cc") + 
  time + toy +
  weekdays+ weekendsIndicator + 
  # covidIndex + 
  temperature + precipitation + 
  winterHolidaysIndicator + summerHolidaysIndicator + bankHolidaysIndicator + 
  nbCar_LaggedWeek + 
  nbCar_LaggedDay + 
  nbCar_LaggedHour + 
  rateCar_LaggedWeek + 
  rateCar_LaggedDay + 
  rateCar_LaggedHour + 
  # nbCar_saintmichelTOdenfertrochereau + rateCar_saintmichelTOdenfertrochereau + 
  # nbCar_saintmichelTOjussieu + rateCar_saintmichelTOjussieu + 
  # nbCar_pontalmaTOconcorde + rateCar_pontalmaTOconcorde + 
  # nbCar_rondpointetoileTOconcorde + rateCar_rondpointetoileTOconcorde + 
  # nbCar_saintlazareTOconcorde + rateCar_saintlazareTOconcorde


g = gam(data=edge_1_5, formula = formula)
plot(g, residuals=T, rug=T, se=F, pch=20)
g.forecast = predict(g, newdata = )

