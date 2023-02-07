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
for (i in 1:53) {
  weeks <- c(weeks,paste("week",i,sep = ""))
}

smoothavg <- data.frame()
weeklymeans <- c()

#Assign 7 days to each week, create smooth average, bind all smooth averages together to find mean
for (i in 1:52) {
  temp <- data.frame()
  temp <- df[(1+10080*(i-1)):(10080*i),]
  assign(weeks[i],temp)
  smoothtemp <- temp %>% mutate("Moving_Average" = rollmean(Global_intensity,7,fill = NA))
  assign(paste("Smoothened",weeks[i]),smoothtemp)
  smoothavg <- rbind(smoothavg,smoothtemp)
  weeklymeans <- c(weeklymeans,mean(smoothtemp$Moving_Average,na.rm = TRUE))
}

#After last week, still one more day to include, 1440 minutes in a day
week53 <- tail(df,1440)
assign("Smoothened week53",week53 %>% mutate("Moving_Average" = rollmean(Global_intensity,7,fill = NA)))
smoothavg <- rbind(smoothavg,`Smoothened week53`)
weeklymeans <- c(weeklymeans,mean(`Smoothened week53`$Moving_Average,na.rm = TRUE))

Average_Smoothened_Week <- smoothavg %>% group_by(Time) %>% summarise(Moving_Average = mean(Moving_Average,na.rm = TRUE))

avgsmoothweek <- mean(Average_Smoothened_Week$Moving_Average,na.rm = TRUE)

comparedsmoothweeks <- c()

avgweeksd <- sd(Average_Smoothened_Week$Moving_Average)

#scoring is determined by calculating the standard deviation of the mean for the average week and determining each week's distance from the average week in terms of standard deviations
for (i in 1:53) {
  comparedsmoothweeks <- c(comparedsmoothweeks,abs((avgsmoothweek - weeklymeans[i])/avgweeksd))
}

maxpos <- match(max(comparedsmoothweeks),comparedsmoothweeks)
minpos <- match(min(comparedsmoothweeks),comparedsmoothweeks)

Most_Anomalous <- paste("Week",maxpos)
Least_Anomalous <- paste("Week",minpos)

print(paste("Most anomalous week is week",paste(maxpos,paste("and least anomalous week is week",minpos))))
  
Anomaly_Score_Table <- data.frame(weeks,comparedsmoothweeks)
colnames(Anomaly_Score_Table) <- c("Week","Score (Number of Standard Deviations Away From Mean)")

Anomaly_Score_Table

Average_Smoothened_Week <- Average_Smoothened_Week %>% mutate(Week = "Average Smoothened Week")
minweekdata <- smoothavg[(1+10080*(minpos-1)):(10080*minpos),] %>% group_by(Time) %>% summarise(Moving_Average = mean(Moving_Average,na.rm = TRUE)) %>% mutate(Week = paste("Least Anomalous Week:",Least_Anomalous))
maxweekdata <- smoothavg[(1+10080*(maxpos-1)):(10080*maxpos),] %>% group_by(Time) %>% summarise(Moving_Average = mean(Moving_Average,na.rm = TRUE)) %>% mutate(Week = paste("Most Anomalous Week:",Most_Anomalous))

graphdata <- rbind(rbind(Average_Smoothened_Week,minweekdata),maxweekdata)

p <- graphdata %>% ggplot(mapping = aes(x = Time, y = Moving_Average, color = Week))

p + geom_point(alpha = 0.5) + labs(title = "Most/Least Anomalous Weeks VS Average Week (00:00-23:59)",y="Moving Average (Global Intensity)",x = "Time") + theme(axis.text.x = element_blank())
