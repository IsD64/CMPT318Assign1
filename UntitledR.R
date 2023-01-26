library(ggplot2)
library(dplyr)
setwd("C:/Users/24312/Documents/code/R")
df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

# find the dates of week 20, append in week20,
# week20 starts with the date of Monday, and ends with Sunday
week20 <- c("14/3/2007")
for (i in 1:6) {
    temp <- sprintf("%d/3/2007", i + 14)
    week20 <- c(week20, temp)
}

# find the rows in the data associated with week20
dfWeek20 <- data.frame()
for (i in week20) {
    temp <- df[df$Date == i, ]
    dfWeek20 <- rbind(dfWeek20, temp)
}


# Question 1
## A. Global_active_power
arithMeanGlobal_active_power <- mean(dfWeek20$Global_active_power)
### geometric mean is e^(mean(ln(i)))
geomMeanGlobal_active_power <- exp(mean(log(dfWeek20$Global_active_power)))
medianGlobal_active_power <- median(dfWeek20$Global_active_power)
### TODO mode skipped
### (dfWeek20 %>% group_by(Global_active_power) %>%
### summarise(total = n())) %>% filter(total == max(total))

minGlobal_active_power <- min(dfWeek20$Global_active_power)
maxGlobal_active_power <- max(dfWeek20$Global_active_power)

## B. Global_reactive_power
arithMeanGlobal_reactive_power <- mean(dfWeek20$Global_reactive_power)
geomMeanGlobal_reactive_power <- exp(mean(log(dfWeek20$Global_reactive_power)))
medianGlobal_reactive_power <- median(dfWeek20$Global_reactive_power)
### TODO mode skipped
### (dfWeek20 %>% group_by(Global_active_power) %>%
### summarise(total = n())) %>% filter(total == max(total))

minGlobal_reactive_power <- min(dfWeek20$Global_reactive_power)
maxGlobal_reactive_power <- max(dfWeek20$Global_reactive_power)

## C. Voltage
arithMeanVoltage <- mean(dfWeek20$Voltage)
geomMeanVoltage <- exp(mean(log(dfWeek20$Voltage)))
medianVoltage <- median(dfWeek20$Voltage)
### TODO mode skipped
### (dfWeek20 %>% group_by(Global_active_power) %>%
### summarise(total = n())) %>% filter(total == max(total))


# Question 2

##labels for parameters the correlation
nameList <- c("Global_Active_Power",
              "Global_Reactive_Power",
              "Voltage",
              "Global_Intensity",
              "Sub_Metering_1",
              "Sub_Metering_2",
              "Sub_Metering_3")

## computes correlations of each column j with the current column i,
## 3:9 refers to the columns in nameList
newmatrix <- matrix(nrow = 0, ncol = 3)

for (i in 3:9) {
    for (j in 3:9) {
        newline <- c(nameList[i - 2], nameList[j - 2],
                     cor(dfWeek20[i], dfWeek20[j]))
        newmatrix <- rbind(newmatrix, newline)
    }
}

## assign colnames, prepare the correlation matrix visualization
colnames(newmatrix) <- c("Var1", "Var2", "Cor")
dfcorrMatrix <- data.frame(newmatrix)
dfcorrMatrix$Cor <- as.double(dfcorrMatrix$Cor)
p <- dfcorrMatrix %>% ggplot(aes(x = Var1, y = Var2))
p + geom_tile(aes(fill = Cor)) +
theme(axis.text.x = element_text(angle = 75, vjust = 0.45, hjust = 0.43))


# Question 3

time <- dfWeek20$Time
Global_intensity <- dfWeek20$Global_intensity
## Defined day time as between 08:00 to 16:00,
## Night time as 16:01 to 7:59

## find the entries correlated to day time, and keep only the needed columns
## 1,2,6 refers to the columns Date, Time, and Global_intensity
dfGIDay <- dfWeek20[dfWeek20$Time >= "08:00:00" & dfWeek20$Time <= "16:00:00", ]
dfGIDay <- dfGIDay[c(1, 2, 6)]

## find the columns related to weekdays and weekends
dfGIDaywd <- dfGIDay[!(dfGIDay$Date %in% week20[6:7]), ]
dfGIDaywe <- dfGIDay[(dfGIDay$Date %in% week20[6:7]), ]

dfGIN <- dfWeek20[dfWeek20$Time < "08:00:00" | dfWeek20$Time > "16:00:00", ]
dfGIN <- dfGIN[c(1, 2, 6)]

dfGINwd <- dfGIN[!(dfGIN$Date %in% week20[6:7]), ]
dfGINwe <- dfGIN[(dfGIN$Date %in% week20[6:7]), ]

dfGIDaywd <- dfGIDaywd %>%
             group_by(Time) %>%
             summarise(avg = mean(Global_intensity))
dfGIDaywe <- dfGIDaywe %>%
             group_by(Time) %>%
             summarise(avg = mean(Global_intensity))
dfGINwd <- dfGINwd %>%
           group_by(Time) %>%
           summarise(avg = mean(Global_intensity))
dfGINwe <- dfGINwe %>%
           group_by(Time) %>%
           summarise(avg = mean(Global_intensity))
## TODO finish the work and add comment
