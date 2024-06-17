
#Statistics Final Project
#Jordan Yoder



#Loading Data and Packages
library(tidyverse)
library(ggplot2)
bikeData = read.csv("C:/Users/12147/Desktop/Statistics/Capital Bike Sharing data by hour.csv")

#Question 1 Month Trends
monthly = bikeData %>% group_by(mnth) %>% summarise(totalAverage = mean(cnt, na.rm = T),
                                            casualAverage = mean(casual, na.rm = T),
                                            registeredAverage = mean(registered, na.rm = T))
monthly

#setting up data for 3 bar graph
test3bar = bikeData %>% select(mnth, casual, registered, cnt)
colnames(test3bar) = c("mnth", "casual", "registered", "total")
test3bar = aggregate(test3bar, by=list(test3bar$mnth),mean)
test3bar<-test3bar[,2:length(test3bar)]
library(reshape2)
bar3 = melt(test3bar, id.vars = "mnth")
bar3

#3 bar graph
ggplot(bar3, aes(x = factor(mnth), y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = "dodge") +
  facet_wrap(~variable,scales="free_x") +
  scale_fill_discrete(name = "User Type") +
  labs(title = "Average Number of Bikes Rented per Month", subtitle = "Months Start with January as Month 1",
       x = "Month", y = "Average Number of Bikes Rented")



#Question 2 Hourly Trends/Weekend vs Weekday

#part A

options(scipen = 999)
ggplot(bikeData, aes(x = factor(hr), y = cnt)) + 
  geom_bar(stat = 'identity', fill = "darkred") +
  labs(title = "Total Number of Bikes Rented Per Hour of Time", 
       subtitle = "Times are in Military Time(ex. 0 - 12am, 1 - 1am...)", 
       x = "Hour", 
       y ="Total Number of Bikes")



#Part B
bikeData$workingday = recode_factor(bikeData$workingday, '0' = "Weekend/Holliday", '1' = "Working Day")
temp = bikeData %>% group_by(workingday) %>% summarise(mr = mean(registered,na.rm = t), mc = mean(casual,na.rm = t))
ggplot(temp, aes(x = workingday, y = mr, fill = workingday)) + 
  geom_bar(stat = 'identity', width = .5) + theme(legend.position = "none") + 
  labs(title = "Bikes Rented by Registered Users on Working and Non-Working Days", x = "", y = "Average Number of Bikes Rented")
ggplot(temp, aes(x = workingday, y = mc, fill = workingday)) + 
  geom_bar(stat = 'identity', width = .5) + theme(legend.position = "none") +
  labs(title = "Bikes Rented by Casual Users on Working and Non-Working Days", x = "", y = "Average Number of Bikes Rented")



#setting up data for 3 bar graph
test2bar = bikeData %>% select(workingday, casual, registered)
colnames(test2bar) = c("workingday", "casual", "registered")
test2bar = aggregate(test2bar, by=list(test2bar$workingday),mean)
test2bar
#test2bar<-test2bar[,2:length(test2bar)]
test2bar = test2bar %>% select(-workingday)
library(reshape2)
bar2 = melt(test2bar, id.vars = "Group.1")
bar2

#2 bar graph
ggplot(bar2, aes(x = Group.1, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = "dodge") +
  facet_wrap(~variable) +
  scale_fill_discrete(name = "User Type") +
  labs(title = "Average Number of Bikes Rented by Users on Working and Non-Working Days
       ", x = "", y = "Average Number of Bikes Rented")








#Question 3 Season vs Bike Rental

bikeData$seasonN = recode_factor(bikeData$season, '1'="Winter", '2'="Spring", '3'="Summer", '4'="Fall")

seasons = bikeData %>% group_by(seasonN) %>% summarise(totalAverage = mean(cnt, na.rm = T),
                                                    casualAverage = mean(casual, na.rm = T),
                                                    registeredAverage = mean(registered, na.rm = T))

bikeData %>% ggplot(aes(x = factor(seasonN), y = cnt, fill = factor(seasonN))) + 
  geom_boxplot() + 
  labs(title = "Bikes Rented by Season",subtitle = "Takes into account all registered and casual users", x = "", y = "Average Number of Bikes Rented") +
  scale_fill_manual(name = "Season", values = c("blue", "orange", "yellow", "brown"))

  #geom_text(aes(label=round(mean(cnt,na.rm = T),2)), position=position_dodge(width=0.9), vjust=-.500)


#Question 4 Weather vs Bike Rental
library(agricolae)
bikeData$weathersit2 = recode_factor(bikeData$weathersit, 
                                     '1'="Clear Skies", '2'="Misty", '3'="Light Rain/Snow", '4'="Heavy Rain/Snow/Thunderstorm")
weather.aov = aov(cnt~factor(weathersit2), bikeData)
summary(weather.aov)
TukeyHSD(weather.aov, conf.level = .95)



bikeData %>% ggplot(aes(x = factor(weathersit2), y = cnt)) + geom_boxplot()
bikeData %>% ggplot(aes(x = factor(weathersit2), y = casual)) + geom_boxplot() + 
  labs(title = "Bikes Rented by Weather",
       subtitle = "Takes into account all Casual users", 
       x = "", 
       y = "Number of Bikes Rented")
bikeData %>% ggplot(aes(x = factor(weathersit2), y = registered)) + geom_boxplot() + 
  labs(title = "Bikes Rented by Weather",
       subtitle = "Takes into account all registered users", 
       x = "", 
       y = "Number of Bikes Rented")

bikeData %>% group_by(weathersit) %>% summarise(Total = mean(cnt,na.rm = T),
                                                Casual = mean(casual, na.rm = T),
                                                Registered = mean(registered, na.rm = T))



humReg = lm(cnt~hum,bikeData)
plot(cnt~hum, bikeData)
abline(lm(cnt~hum,bikeData))
summary(lm(cnt~hum, bikeData))

windReg = lm(cnt~windspeed, bikeData)
plot(cnt~windspeed, bikeData)
abline(lm(cnt~windspeed,bikeData))
summary(lm(cnt~windspeed, bikeData))


weatherReg = lm(cnt~weathersit2, bikeData)

weatherRegAll = lm(cnt~windspeed+hum+factor(weathersit), bikeData)
weatherSum = summary(weatherRegAll)
weatherSum






#Question 5 Linear Regression of temp and bike rentals
tempReg = lm(cnt~temp, bikeData)
plot(cnt~temp, bikeData,
     pch = 1, # pch = lot character
     col = "lightblue",
     cex = .5, # Size parameter
     xlab = "Temperature(Formula:(t-tmin)/(tmax-tmin))",
     ylab = "Bikes Rented",
     main = "Bikes Rented by Temperature")
abline(tempReg, col = "red")
summary(tempReg)

#Question 6 Linear Regression of "feels like" temp and bike rentals
atempReg = lm(cnt~atemp, bikeData)
plot(cnt~atemp, bikeData,
     pch = 1, # pch = lot character
     col = "lightgreen",
     cex = .5, # Size parameter
     xlab = "'Feels Like' Temperature(Formula:(t-tmin)/(tmax-tmin))",
     ylab = "Bikes Rented",
     main = "Bikes Rented by 'Feels Like' Temperature")
abline(atempReg, col = "blue")
summary(atempReg)


#weather regression chart, used for Q4,5,6
library(stargazer)
stargazer(windReg, humReg, tempReg, atempReg, type="text",
          dep.var.caption = "", dep.var.labels.include = F,
          report = "c*", df = F, model.numbers = F,
          keep.stat = c("ser","rsq","adj.rsq"),
          column.labels = c("Wind Reg","Humidity Reg", 
                            "Temperature Reg", "'Feels Like' Temperature Reg"))


