library(tidyverse)
library(zoo)

#setwd("C:/Users/24312/Documents/code/R") is Issac's path
setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")

#Question 2
#number of minutes in a week: 60*24*7 = 10080
#total number of rows: 525600

weeks <- c()

#Set up week names -> week1:week52
for (i in 1:52) {
  weeks <- c(weeks,paste("week",i,sep = ""))
}

smoothavg <- data.frame()
weeklymeans <- c()

#Assign 7 days to each week, create smooth average, bind all smooth averages together to find mean
for (i in 1:52) {
  temp <- data.frame()
  temp <- df[(1+10080*(i-1)):(10080*i),]
  assign(weeks[i],temp)
  smoothtemp <- temp %>% mutate("Moving_Average" = rollmean(Global_intensity,7,fill = 0))
  assign(paste("Smoothened",weeks[i]),smoothtemp)
  smoothavg <- rbind(smoothavg,smoothtemp)
  weeklymeans <- c(weeklymeans,mean(smoothtemp$Moving_Average,na.rm = TRUE))
}

#After last week, still one more day to include, 1440 minutes in a day
week53 <- tail(df,1440)
assign("Smoothened week53",week53 %>% mutate("Moving_Average" = rollmean(Global_intensity,7,fill = 0)))
smoothavg <- rbind(smoothavg,`Smoothened week53`)
weeklymeans <- c(weeklymeans,mean(`Smoothened week53`$Moving_Average,na.rm = TRUE))

Average_Smoothened_Week <- smoothavg %>% group_by(Time) %>% summarise(Average_Smoothened_Week_at_Time_t = mean(Moving_Average,na.rm = TRUE))

avgsmoothweek <- mean(Average_Smoothened_Week$Average_Smoothened_Week_at_Time_t,na.rm = TRUE)

comparedsmoothweeks <- c()

avgweeksd <- sd(Average_Smoothened_Week$Average_Smoothened_Week_at_Time_t)

#scoring is determined by calculating the standard deviation of the 
for (i in 1:53) {
  comparedsmoothweeks <- c(comparedsmoothweeks,weeklymeans[i]/avgweeksd)
}

Most_Anomalous <- weeks[match(max(comparedsmoothweeks),comparedsmoothweeks)]
Least_Anomalous <- weeks[match(min(comparedsmoothweeks),comparedsmoothweeks)]

