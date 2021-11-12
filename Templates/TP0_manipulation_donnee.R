##################################################
#####web scraping avec XML et RCurl
##################################################
rm(list=objects())
library(XML)
library(RCurl)
library(magrittr)
library(mgcv)
library(tidyverse)
library(lubridate)
library(weathermetrics)
library(ranger)

#install.packages("ranger")


###############Import data (méteo)

data_temp<-read_delim("/Users/yannig/Documents/Enseignement/2019_2020/M2statML/Datasets/montsouris.csv"
                      , col_names =TRUE, delim=',')
data_temp



data_temp
names(data_temp)[which(names(data_temp)=="DATE")]<-c("Date")
summary(data_temp)
str(data_temp$Date)
is.Date(data_temp$Date)




###############convertion en ° celsius
data_temp$TAVG<-fahrenheit.to.celsius(data_temp$TAVG, round = 2)
data_temp$TMIN<-fahrenheit.to.celsius(data_temp$TMIN, round = 2)
data_temp$TMAX<-fahrenheit.to.celsius(data_temp$TMAX, round = 2)
dim(data_temp)
###############visualisation globale
par(mfrow=c(1,1))
plot(data_temp$Date, data_temp$TAVG, type='l')
lines(data_temp$Date, data_temp$TMIN, col='blue')
lines(data_temp$Date, data_temp$TMAX, col='red')

###############visualisation février 2012
a<-ymd("2012-02-01")
b<-ymd("2012-03-01")
sel<-which(data_temp$Date>=a & data_temp$Date<=b)
o<-order(data_temp$Date[sel])
plot(data_temp$Date[sel[o]], data_temp$TAVG[sel[o]], type='l', ylim=range(data_temp$TMIN, data_temp$TMAX,
                                                                         na.rm = T))
lines(data_temp$Date[sel[o]], data_temp$TMIN[sel[o]], col='blue')
lines(data_temp$Date[sel[o]], data_temp$TMAX[sel[o]], col='red')


a<-ymd("2017-08-20")
b<-ymd("2017-09-15")
sel<-which(data_temp$Date>=a & data_temp$Date<=b)

o<-order(data_temp$Date[sel])
plot(data_temp$Date[sel[o]], data_temp$TAVG[sel[o]], type='l', ylim=range(data_temp$TMIN, data_temp$TMAX,
                                                                          na.rm = T))
lines(data_temp$Date[sel[o]], data_temp$TMIN[sel[o]], col='blue')
lines(data_temp$Date[sel[o]], data_temp$TMAX[sel[o]], col='red')

###############quand a lieu le minimum
min(data_temp$TMIN, na.rm=T)
data_temp$Date[which.min(data_temp$TMIN)]



###############gestion des valeurs manquantes

###interpolation
a<-ymd("2012-02-01")
b<-ymd("2012-03-01")
sel<-which(data_temp$Date>=a & data_temp$Date<=b)

Temp.interpol <- data_temp$TMIN[sel[o]]
dat.interpol <- data_temp$Date[sel[o]]
interpol<-spline(dat.interpol, Temp.interpol , method = c("natural"), ties = mean, xout=dat.interpol)

plot(dat.interpol, interpol$y, col='blue', pch=20)
sel.na <- which(is.na(Temp.interpol))
#lines(dat.interpol, Temp.interpol, col='blue')
points(dat.interpol[sel.na], interpol$y[sel.na], pch=20, col='red', cex=2)
lines(dat.interpol, interpol$y, col='blue')


###modélisation par RF "automatique"
summary(data_temp)

data_temp2 <- data_temp[,-5]
summary(data_temp2)

na.row <- rowSums(data_temp2[,-c(1:3)])
which(is.na(na.row))%>%length

data_temp2 <- data_temp2[-which(is.na(na.row)), ]
summary(data_temp2)
dim(data_temp2)


rg <- ranger(TMAX ~ TMIN+TAVG+PRCP, data=data_temp2)
rg

plot(data_temp2$TMAX, type='l')
lines(rg$predictions, col='red')

plot(data_temp2$TMAX%>%head(,n=100), type='l')
lines(rg$predictions%>%head(,n=100), col='red')

mean((data_temp2$TMAX-rg$predictions)^2)%>%sqrt

hist(data_temp2$TMAX-rg$predictions, breaks=50)
plot(data_temp2$TMAX-rg$predictions, type='l')
plot(data_temp2$TMAX, rg$predictions, pch=20)


reg1 <- lm(TMAX ~ TMIN+TAVG+PRCP, data=data_temp2)
mean((data_temp2$TMAX-reg1$fitted.values)^2)%>%sqrt
plot(data_temp2$TMAX%>%head(,n=100), type='l')
lines(reg1$fitted.values%>%head(,n=100), col='red')
lines(rg$predictions%>%head(,n=100), col='blue')

hist(data_temp2$TMAX-reg1$fitted.values, breaks=50)

plot(data_temp2$TMAX-rg$predictions, type='l')
lines(data_temp2$TMAX-reg1$fitted.values, type='l', col='grey')

plot(data_temp2$TMAX, reg1$fitted.values, pch=20)


TMAX.interpol <- data_temp$TMAX

sel.na <- which(is.na(TMAX.interpol))
TMAX.interpol[sel.na] <- predict(reg1, newdata=data_temp[sel.na,])

sum(is.na(data_temp$TMAX))
sum(is.na(TMAX.interpol))
data_temp[is.na(TMAX.interpol), ]


plot(data_temp2$TAVG, data_temp2$TMAX, pch=20)
reg2 <- lm(TMAX ~ TAVG, data=data_temp2)
sel.na <- which(is.na(TMAX.interpol))
TMAX.interpol[sel.na] <- predict(reg2, newdata=data_temp[sel.na,])
sum(is.na(TMAX.interpol))

###############visualisation février 2012
a<-ymd("2012-02-01")
b<-ymd("2012-03-01")
sel<-which(data_temp$Date>=a & data_temp$Date<=b)
o<-order(data_temp$Date[sel])
plot(data_temp$Date[sel[o]], data_temp$TAVG[sel[o]], type='l', ylim=range(data_temp$TMIN, data_temp$TMAX,
                                                                          na.rm = T))
lines(data_temp$Date[sel[o]], data_temp$TMAX[sel[o]], col='red')
points(data_temp$Date[sel[o]], TMAX.interpol[sel[o]], col='red', pch=20)



###############Import data (conso)
data_conso<-read_delim("/Users/yannig/Documents/Enseignement/2017-2018/M2statML/Datasets/cdc_conso.csv", 
                       col_names =TRUE, delim=';')
names(data_conso)
summary(data_conso)
names(data_conso)<-c("DateH", "Date", "Heure", "Conso", "Qualite")
is.Date(data_conso$Date)

o <- order(data_conso$DateH)
plot(data_conso$DateH[o], data_conso$Conso[o], type='l')

#####moyenne par jour, par heure
Dow<-wday(data_conso$DateH, label=T)
meanDow<-tapply(data_conso$Conso, Dow, mean, na.rm=T)
meanDow

barplot(meanDow)


Hour<-hour(data_conso$DateH)
meanHour<-tapply(data_conso$Conso, Hour, mean, na.rm=T)
plot(unique(Hour)%>%sort, meanHour, type='b', pch=20, xlab='Hour', ylab='consumption')

max(data_conso$Conso)
data_conso$DateH[which.max(data_conso$Conso)]


daily_conso <- data_conso%>% group_by(Date = format(Date, "%Y-%m-%d"))%>%summarise_at("Conso", mean, na.rm = TRUE)
daily_conso$Date<-ymd(daily_conso$Date)
is.Date(daily_conso$Date)

dim(data_conso)/48
dim(daily_conso)

summary(daily_conso)
summary(data_conso)


data<-inner_join(data_temp, daily_conso, by="Date")
names(data)

plot(data$TAVG, data$Conso, pch=20)


######
sel<-which(month(data_conso$Date)==2 & year(data_conso$Date)==2012)

#ou
a<-ymd_hms("2012-02-01 00:00:00")
b<-ymd_hms("2012-03-01 00:00:00")
sel<-which(data_conso$DateH>=a & data_conso$DateH<=b)
o<-order(data_conso$DateH[sel])
plot(data_conso$DateH[sel[o]], data_conso$Conso[sel[o]], type='l')


sel<-which(months(data$Date)=="February" & year(data$Date)==2012)
o<-order(data$Date[sel])
plot(data$Date[sel[o]], data$Conso[sel[o]], type='l')
par(new=T)
plot(data$Date[sel[o]], data$TAVG[sel[o]], col='red', type='l', axes=F
     , xlab='',ylab='')




################################################################################
##########population mondiale
################################################################################
u<-"https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
theurl <- getURL(u, .opts = list(ssl.verifypeer = FALSE) )
tables = readHTMLTable(theurl)
names(tables)

Data<-tables[[1]]
head(Data)
names(Data)
Data <- Data[-c(1:2),1:4]
#Data  <- Data[1:123,]
Data$Population<-gsub(",", "", Data$V4)%>%as.numeric
head(Data)
sum(Data$Population)


######autre façon
library(rvest)
url <- u<-"https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
paragraphs <- read_html(url)%>% html_nodes("table")
Data <- paragraphs %>% `[[`(1) %>% html_table(fill = TRUE)



url <- "https://en.wikipedia.org/wiki/World_population"
ten_most_df <- read_html(url) 

ten_most_populous <- ten_most_df %>% 
  html_nodes("table") %>% `[[`(6) %>% html_table()


################################################################################
##########élections
################################################################################
theurl <- getURL("https://en.wikipedia.org/wiki/Opinion_polling_for_the_French_presidential_election,_2017"
                 ,.opts = list(ssl.verifypeer = FALSE) )
Data <- readHTMLTable(theurl, stringsAsFactors = FALSE, which=1)
Data
#regexpr(pattern="26_January_to_16_March_2017", text = theurl)

str(Data)
dim(Data)
head(Data)
Data[1:3,2]

Date<-Data[3:nrow(Data),2]
Date
#Date<-Date[length(Date)]

p<-dim(Data)[2]
NomCandidat<-Data[1, 5:p]
colnames(NomCandidat)<-NULL
row.names(NomCandidat)<-NULL
NomCandidat

Polls<-Data[-c(1:2), 5:p]
#Polls<-Polls[-nrow(Polls),]
Polls



names(Polls)<-NomCandidat
Polls<-lapply(Polls, function(x){gsub('%', '', x)})
Polls <- lapply(Polls, function(x){gsub('<', '', x)})
Polls <- lapply(Polls, function(x){as.numeric(x)})
lapply(Polls, mean)

#Polls <- Polls%>%unlist%>%matrix(, nrow=length(Polls[[1]]), ncol=length(Polls), byrow=F)

Polls<- sapply(Polls, `[`)

DataPolls<-data.frame(Date, Polls)
head(DataPolls)

#gestion de la date
findMonth<-function(x)
{
  x<-as.character(x)
  r<-substr(x, nchar(x)-7, nchar(x)-5)
  r[which(r=="Apr")]<-4
  r[which(r=="Mar")]<-3
  
  s<-which(nchar(as.character(x))==17)
  r[s]<-3
  return(as.numeric(r))
}



Day<-substr(DataPolls$Date,1,2)
Day<-gsub("_", "",Day)
Month<-findMonth(DataPolls$Date)
Month[which(is.na(Month))]<-3
Year<-rep(2017,nrow(DataPolls))

Date2 <- paste(Year,Month,Day, sep='-')
Date2 <- as.POSIXct(strptime(Date2, "%Y-%m-%d"))

DataPolls$Date2<-Date2
plot(DataPolls$Date2, DataPolls$MélenchonFI, pch=20, col='red')

names(DataPolls)



NomCandidat<-names(DataPolls)[2:(ncol(DataPolls)-1)]
s<-seq(nrow(DataPolls)-5, 5, length.out = length(NomCandidat))%>%floor

col<-c('dark red', 'dark red', 'red', 'salmon', 'orange', 'yellow3', 'blue', 
       'royalblue', 'purple', 'dark blue', 'grey')
color_transparent <- adjustcolor(col, alpha.f = 0.5) 

plot(DataPolls$Date2, DataPolls[,NomCandidat[[1]]], ylim=range(DataPolls[, 2:(ncol(DataPolls)-1)]), 
     col=color_transparent[1]
     , pch=16, cex=0.5, xlab='Date', ylab='Polls')
text(DataPolls$Date2[s[1]],DataPolls[s[1],NomCandidat[1]], NomCandidat[1], col= col[1], font=2)
g<-gam(DataPolls[,NomCandidat[[1]]]~s(as.numeric(DataPolls$Date2)))
lines(DataPolls$Date2, g$fitted, col=col[1])

for(i in c(2:length(NomCandidat)))
{
  print(NomCandidat[i])
  points(DataPolls$Date2,  DataPolls[,NomCandidat[i]], col=color_transparent[i],, pch=16, cex=0.5) 
  text(DataPolls$Date2[s[i]],DataPolls[s[i],NomCandidat[i]], NomCandidat[i], col= col[i], font=2)
  g<-gam(DataPolls[,NomCandidat[[i]]]~s(as.numeric(DataPolls$Date2)))
  lines(DataPolls$Date2, g$fitted, col=col[i])
}
















