library(depmixS4)
library(tidyverse)

#setwd("C:/Users/24312/Documents/code/R") is Issac's path
setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Group_Assignment_3_Dataset.txt", header = TRUE, sep = ",")

#Question 1

scaled_data <- df[1:2]
vari_list <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global Intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

for (i in 3:9) {
  tempdata <- scale(df[i])
  colname <- vari_list[i]
  scaled_data <- scaled_data %>% mutate(tempcol = tempdata)
  colnames(scaled_data)[i] <- colname
}

#Look for a pattern in consumption (Global active power)
p <- scaled_data %>% filter(Date == "1/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>% ggplot(mapping = aes(x = Time, y = Global_active_power))
p + geom_point()

p2 <- scaled_data %>% filter(Date == "8/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>% ggplot(mapping = aes(x = Time, y = Global_active_power))
p2 + geom_point()

p3 <- scaled_data %>% filter(Date == "15/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>% ggplot(mapping = aes(x = Time, y = Global_active_power))
p3 + geom_point()

#From the plots we can see that from 07:00:00 - 11:00:00 on Mondays the data often has a spike before going back down, with occasional smaller spikes after
TimeWindowDF <- data.frame()

#60*24 = 1440 minutes per day, *7 = 10080 minutes in a week
for (i in 1:52) {
  temp <- data.frame()
  temp <- scaled_data[(1 + 10080*(i-1)):(10080*(i-1)+(1440)),]
  temp <- temp %>% filter(Time >= "07:00:00" & Time <= "11:00:00") %>% dplyr::select(Date,Time,Global_active_power)
  TimeWindowDF <- rbind(TimeWindowDF,temp)
}

#Question 2
#12532 entries in the data set divided by 52 weeks = 241
#We need to try multiple nstates to find the best fit, may not need to do all 13 possibilities if smart with it

model1 <- depmix(response = Global_active_power ~ 1, data = TimeWindowDF, nstates = 10, ntimes = rep(241,52))
fitmodel1 <- fit(model1)
summary(fitmodel1)
