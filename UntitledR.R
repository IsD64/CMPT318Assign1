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

class(df)                                       # tests the type of variable df
dfDay20 <- df[df$Date == "20/1/2007", ]          # date in format "DD/MM/YYYY"
                                                # need week 20, not day 20