library(tidyverse)
library(stats)

#setwd("C:/Users/24312/Documents/code/R") is Issac's path
setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")

#Removing NA values
df[is.na(df)] = 0

#Selecting subset for PCA, specifically Mondays in 2007 from 2:00pm - 7:00pm

df1 <- df %>% filter(substr(Date,str_length(Date)-3,str_length(Date))=="2007" & Time >= "13:59:59" & Time <= "19:00:00") 

#We know that in a 5 hour interval, there will be 60*5+1=301 observations per day (the first minute of the next hour is included)
#Along with 52 weeks, that means we need to find 52 Mondays

df2 <- data.frame()

for (i in 0:51) {
  temp <- df1[(1+(301*7*i)):(301+(301*7*i)),]
  df2 <- rbind(df2,temp)
}

selecteddata <- df2

df2 <- df2 %>% group_by(Date) %>% summarise(Global_Active_Power = mean(Global_active_power), Global_Reactive_Power = mean(Global_reactive_power), Voltage = mean(Voltage), Global_Intensity = mean(Global_intensity), Sub_Metering_1 = mean(Sub_metering_1), Sub_Metering_2 = mean(Sub_metering_2), Sub_Metering_3 = mean(Sub_metering_3))

dfpca <- as.data.frame(scale(df2[2:8]))

pca <- prcomp(dfpca)
pcasum <- summary(pca)

biplot(pca, cex = c(0.5,0.5), col = c("black","red"), arrow.len = 0.1, ylim = c(-0.4,0.4), xlim = c(-0.4,0.4))

pcasum
#From the importance, we can see that using 4 principal components allows the data to explain 89-90% of the variance. Therefore we will use 4 components.
#The 4 components will be selected by comparing magnitudes in each principal component

pca
#Component 1: Global_Intensity
#Component 2: Sub_Metering_3
#Component 3: Voltage
#Component 4: Sub_Metering_2

dfresult <- selecteddata %>% select(Date,Time,Global_intensity,Sub_metering_3,Voltage,Sub_metering_2)
