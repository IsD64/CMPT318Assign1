library(tidyverse)

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

#Assign 7 days to each week
for (i in 1:52) {
  temp <- data.frame()
  temp <- df[(1+10080*(i-1)):(10080*i),]
  assign(weeks[i],temp)
}