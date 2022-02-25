rm(list=objects())

library(ProjetML1)
library(corrplot)
library(RColorBrewer)
library("dplyr")

# data
data("data_train")
data <- data_train[[5]] %>% select(1,24,26,28,30,32)

# correlation plot
corrplot(cor(data), type="upper", order="hclust",
         col=brewer.pal(n=10, name="RdYlBu"),tl.cex = 0.5)


# plot
plot(data$rateCar[104:(104+(24*7))],type='l',
     ylab = "rateCar",
     xlab = "numéro de l'observation",
     main = "Taux d'occupation de l'arête ''saint-michel châtelet'' la semaine \n du lundi 6 janvier 2014")


