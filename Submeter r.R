#Submeter Analysis
install.packages("dplyr")
library(dplyr)
install.packages(c("tidyr", "devtools"))
library(tidyr)
library(devtools)

#Data Install
setwd("~/Desktop/IOT Analytics")
library(readr)
submeter_energy <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", fill = TRUE)
attributes(submeter_energy)
View(submeter_energy)
summary(submeter_energy)
str(submeter_energy)

#One column for Date and Time Stamp
submeter_combined <-cbind(submeter_energy, paste(submeter_energy$Date,submeter_energy$Time), stringsAsFactors=FALSE)
colnames(submeter_combined)[10] <-"DateTime"
View(submeter_combined)
resident_energy <- submeter_combined[,c(ncol(submeter_combined), 1:(ncol(submeter_combined)-1))]
head(resident_energy)
View(resident_energy)

#Convert Date and Time
resident_energy$DateTime <- strptime(resident_energy$DateTime, "%d/%m/%Y %H:%M:%S")
resident_energy$Date<- as.Date(resident_energy$Date, "%d/%m/%Y")
head(resident_energy)
View(resident_energy)
str(resident_energy)
summary(resident_energy)

#Attributes redefined
resident_energy$Sub_metering_3 <- as.factor(resident_energy$Sub_metering_3)
str(resident_energy)

#Group Data and transform Posix _ dec 
library(dplyr)

resident_energy$Date <- as.POSIXct(resident_energy$Date)
resident_energy$DateTime <- as.POSIXct(resident_energy$DateTime)
View(resident_energy)

#Descriptive Stats
summary(resident_energy)
is.na(resident_energy)
write_csv(resident_energy, "resident_energy.csv")

#Feature Engineering
resident_energy$Global_active_power<-NULL
resident_energy$Global_reactive_power<-NULL
resident_energy$Voltage<-NULL
resident_energy$Global_intensity<- NULL

#Remove submeters 1 and 3
str(resident_energy)
sub_two <- resident_energy[c(-4,-6)]
str(sub_two)
summary(sub_two)
write_csv(sub_two, "Sub2stats.csv")
mean(sub_two)

#Remove submeter 2 and 3
sub_one <- resident_energy[c(-5,-6)]
str(sub_one)

#Remove submeter 1 and 2
sub_three <- resident_energy[c(-4,-5)]
str(sub_three)


#Subset Date for March 2007 _ submeter 2
March_2007<- subset(sub_two, Date>="2007-03-01" & Date<="2007-03-31")
house_60_march <- March_2007[seq(1,nrow(March_2007),60),]
sub2_summ <-summary(house_60_march)
str(house_60_march)

time_March <- house_60_march[c(-1,-3)]
View(time_March)
plot(time_March)
March_60 <- March_2007[seq(1,nrow(March_2007),60),]
View(house_60_march)
house_60_march$DateTime<-NULL
March_60$Date<-NULL
March_60$Time<-NULL
str(March_60)
View(March_60)
plot(March_60)

#Timeseries for March 2007
March_housets <- ts(house_60_march, frequency = 31)
plot(March_housets)


#Forecast March 2007 submeter 2
library(forecast)
library(ggplot2)
?plot
March_fore <- forecast(March_housets, h=24)
View(March_fore)
plot(March_fore)

plot(March_fore,xlim=c(25,26),ylim=c(0,30))
plot(March_fore, ylim=c(0,50))

#Decompose and Holt Winters March 2007 submeter 2
?decompose
March_dec<- decompose(March_housets, type = "additive", filter = NULL)
March_dec$seasonal
March_dec$trend
March_dec$random
plot(March_dec$seasonal)
plot(March_dec$trend)
plot(March_dec$random)
summary(March_dec)

?HoltWinters
March_HW <- HoltWinters(March_fore, beta = FALSE, gamma = FALSE)
plot(March_HW)

#March 2007 Submeter one

March_2007_subone<- subset(sub_one, Date>="2007-03-01" & Date<="2007-03-31")
house_60_march_subone <- March_2007_subone[seq(1,nrow(March_2007_subone),60),]
View(house_60_march_subone)
house_60_march_subone$DateTime<-NULL
house_60_march_subone$Date<-NULL
house_60_march_subone$Time<-NULL
plot(house_60_march_subone)

#Timeseries for March 2007-subone
March_housets_subone <- ts(house_60_march_subone, frequency = 31)
plot(March_housets_subone)


#Forecast March 2007 submeter 1
library(forecast)
library(ggplot2)
?plot
March_fore_subone <- forecast(March_housets_subone, h=24)
View(March_fore_subone)
plot(March_fore_subone)

plot(March_fore_subone,xlim=c(25,26),ylim=c(0,20))
plot(March_fore_subone, ylim=c(0,30))

#Decompose and Holt Winters March 2007 submeter 1
?decompose
March_dec_one<- decompose(March_housets_subone, type = "additive", filter = NULL)
March_dec_one$seasonal
March_dec_one$trend
March_dec_one$random
plot(March_dec_one$seasonal)
plot(March_dec_one$trend)
plot(March_dec_one$random)
summary(March_dec)

?HoltWinters
March_HW_subone <- HoltWinters(March_fore_subone, beta = FALSE, gamma = FALSE)
plot(March_HW)

#March 2007 Submeter three 
March_2007_subthree<- subset(sub_three, Date>="2007-03-01" & Date<="2007-03-31")
house_60_march_three <- March_2007_subthree[seq(1,nrow(March_2007_subthree),60),]
View(house_60_march_three)
house_60_march_three$DateTime<-NULL
house_60_march_three$Date<-NULL
house_60_march_three$Time<-NULL

#Timeseries for March 2007-sub 3
March_housets_subthree <- ts(house_60_march_three, frequency = 31)
plot(March_housets_subthree)


#Forecast March 2007 submeter 3
library(forecast)
library(ggplot2)
?plot
March_fore_subthree <- forecast(March_housets_subthree, h=24)
View(March_fore_subthree)
plot(March_fore_subthree)

plot(March_fore_subone,xlim=c(25,26),ylim=c(0,20))
plot(March_fore_subone, ylim=c(0,30))


#Subset August 2008
August_2008<- subset(sub_two, Date>="2008-08-01" & Date<="2008-08-31")
house_60_august <- August_2008[seq(1,nrow(August_2008),60),]
View(house_60_august)
plot(house_60_august)
house_60_august$DateTime<-NULL
house_60_august$Date<-NULL
house_60_august$Time<-NULL
summary(house_60_august)
write_csv(house_60_august, "auguststats.csv")

#Timeseries for august 2008
august_housets <- ts(house_60_august, frequency = 31)
plot(august_housets)


#Forecast August 2008
library(forecast)
library(ggplot2)
?plot
August_fore <- forecast(august_housets, h=24)
View(August_fore)
plot(August_fore)

plot(August_fore,xlim=c(25,26),ylim=c(0,20))


#August 2008 decompose and Holt Winters
August_decompose<- decompose(august_housets, type = "additive", filter = NULL)
August_decompose$seasonal
August_decompose$trend
August_decompose$random
plot(August_decompose$seasonal)
plot(August_decompose$trend)
plot(August_decompose$random)
summary(August_decompose)

August_HW<-HoltWinters(August_fore, beta = FALSE, gamma = FALSE)
plot(August_HW)

#August 2008 Submeter one

#August 2008 Submeter three

#Dec 2009 submeter two

Dec_2009<- subset(sub_two, Date>="2009-12-01" & Date<="2009-12-31")
house_60_dec <- Dec_2009[seq(1,nrow(Dec_2009),60),]
View(house_60_dec)
plot(house_60_dec)
house_60_dec$DateTime<-NULL
house_60_dec$Date<-NULL
house_60_dec$Time<-NULL
summary(house_60_dec)
write_csv(house_60_dec, "decstats.csv")

#Timeseries for Dec 2009
dec_housets <- ts(house_60_dec, frequency = 31)
plot(dec_housets)


#Forecast Dec 2009
library(forecast)
library(ggplot2)
?plot
Dec_fore <- forecast(dec_housets, h=24)
View(Dec_fore)
plot(Dec_fore)

plot(Dec_fore,xlim=c(25,26),ylim=c(0,20))


#Decompose and Holt Winters Dec 2009

Dec_decompose<- decompose(dec_housets, type = "additive", filter = NULL)
Dec_decompose$seasonal
Dec_decompose$trend
Dec_decompose$random
plot(Dec_decompose$seasonal)
plot(Dec_decompose$trend)
plot(Dec_decompose$random)
summary(Dec_decompose)

Dec_HW<-HoltWinters(Dec_fore, beta = FALSE, gamma = FALSE)
plot(Dec_HW)



  
  
  
  
  


