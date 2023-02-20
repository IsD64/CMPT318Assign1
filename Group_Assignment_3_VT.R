library(tidyverse)
library(zoo)

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