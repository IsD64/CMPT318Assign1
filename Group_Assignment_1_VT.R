#setwd("C:/Users/24312/Documents/code/R")
setwd("C:/Users/thund/Desktop/Cmpt 318")
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

#(dfWeek20 %>% group_by(Global_active_power) %>% summarise(total = n())) %>% filter(total == max(total))

# Question 2

corMatrix <- matrix(nrow=0,ncol=7)

for (i in 3:9) {
  corlst <- c(cor(dfWeek20[i],dfWeek20[3]),cor(dfWeek20[i],dfWeek20[4]),cor(dfWeek20[i],dfWeek20[5]),cor(dfWeek20[i],dfWeek20[6]),cor(dfWeek20[i],dfWeek20[7]),cor(dfWeek20[i],dfWeek20[8]),cor(dfWeek20[i],dfWeek20[9]))
  corMatrix <- rbind(corMatrix,corlst)
}

namelst <- c("Global_Active_Power","Global_Reactive_Power","Voltage","Global_Intensity","Sub_Metering_1","Sub_Metering_2","Sub_Metering_3")

colnames(corMatrix) <- namelst
rownames(corMatrix) <- namelst

newmatrix <- matrix(nrow=0,ncol=3)

for (i in 3:9) {
  for (j in 3:9) {
    newline <- c(namelst[i-2],namelst[j-2],cor(dfWeek20[i],dfWeek20[j]))
    newmatrix <- rbind(newmatrix,newline)
  }
}

colnames(newmatrix) <- c("Var1","Var2","Cor")

dfcorMatrix <- data.frame(newmatrix)
dfcorMatrix$Cor <- as.double(dfcorMatrix$Cor)

p <- dfcorMatrix %>% ggplot(aes(x = Var1 ,y = Var2))
p + geom_tile(aes(fill=Cor)) + ylab("") + xlab("") + theme(axis.text.x = element_text(angle = 75,vjust = 0.45,hjust = 0.43))

#Question 3

time <- dfWeek20$Time
Global_intensity <- dfWeek20$Global_intensity

dfGIDay <- dfWeek20[dfWeek20$Time >= "08:00:00"&dfWeek20$Time <= "16:00:00",] 
dfGIDay <- dfGIDay[c(1,2,6)]

dfGIDaywd <- dfGIDay[!(dfGIDay$Date %in% week20[6:7]),]
dfGIDaywe <- dfGIDay[(dfGIDay$Date %in% week20[6:7]),]

dfGIN <- dfWeek20[dfWeek20$Time < "08:00:00"|dfWeek20$Time > "16:00:00",] 
dfGIN <- dfGIN[c(1,2,6)]

dfGINwd <- dfGIN[!(dfGIN$Date %in% week20[6:7]),]
dfGINwe <- dfGIN[(dfGIN$Date %in% week20[6:7]),]

dfGIDaywd <- dfGIDaywd %>% group_by(Time) %>% summarise(avg = mean(Global_intensity))
dfGIDaywe <- dfGIDaywe %>% group_by(Time) %>% summarise(avg = mean(Global_intensity))
dfGINwd <- dfGINwd %>% group_by(Time) %>% summarise(avg = mean(Global_intensity))
dfGINwe <- dfGINwe %>% group_by(Time) %>% summarise(avg = mean(Global_intensity))