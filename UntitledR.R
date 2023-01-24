setwd("C:/Users/24312/Documents/code/R")
df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

# find the dates of week 20, append in week20
week20 <- c("14/3/2007")
for (i in (1:6)) {
    temp <- sprintf("%d/3/2007", i+14)
    week20 <- c(week20, temp)
}

# find the rows associated with week20
dfWeek20 <- data.frame()

for (i in week20) {
    temp <- df[df$Date == i, ]
    dfWeek20 <- rbind(dfWeek20, temp)
}

# Question 1
# A. Global_active_power
arithMeanGlobal_active_power <- mean(dfWeek20$Global_active_power)
# geometric mean is e^(mean(ln(i)))
geomMeanGlobal_active_power <- exp(mean(log(dfWeek20$Global_active_power)))
medianGlobal_active_power <- median(dfWeek20$Global_active_power)
# TODO mode skipped
counter <- 

minGlobal_active_power <- min(dfWeek20$Global_active_power)
maxGlobal_active_power <- max(dfWeek20$Global_active_power)

# B. Global_reactive_power
arithMeanGlobal_reactive_power <- mean(dfWeek20$Global_reactive_power)
geomMeanGlobal_reactive_power <- exp(mean(log(dfWeek20$Global_reactive_power)))
medianGlobal_reactive_power <- median(dfWeek20$Global_reactive_power)
# TODO mode skipped

minGlobal_reactive_power <- min(dfWeek20$Global_reactive_power)
maxGlobal_reactive_power <- max(dfWeek20$Global_reactive_power)

#C. Voltage
arithMeanVoltage <- mean(dfWeek20$Voltage)
geomMeanVoltage <- exp(mean(log(dfWeek20$Voltage)))
medianVoltage <- median(dfWeek20$Voltage)
# TODO mode skipped

class(df)                                       # tests the type of variable df
dfDay20 <- df[df$Date == "20/1/2007", ]         # date in format "DD/MM/YYYY"
                                                # need week 20, not day 20