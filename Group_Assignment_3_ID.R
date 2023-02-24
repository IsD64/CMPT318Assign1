library(ggplot2)
library(dplyr)
library(depmixS4)
# library(tidyverse)

setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
# setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Group_Assignment_3_Dataset.txt", header = TRUE, sep = ",")

#Question 1

scaled_data <- df[1: 2]
var_list <- c("Date",
              "Time",
              "Global_active_power",
              "Global_reactive_power",
              "Voltage",
              "Global Intensity",
              "Sub_metering_1",
              "Sub_metering_2",
              "Sub_metering_3")

for (i in 3:9) {
    temp_data <- scale(df[i])
    col_name <- var_list[i]
    scaled_data <- scaled_data %>% mutate(tempcol = temp_data)
    colnames(scaled_data)[i] <- col_name
}

#Look for a pattern in consumption (Global active power)
p <- scaled_data %>%
     filter(Date == "1/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>%
     ggplot(mapping = aes(x = Time, y = Global_active_power))
p + geom_point()

p2 <- scaled_data %>%
      filter(Date == "8/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>%
      ggplot(mapping = aes(x = Time, y = Global_active_power))
p2 + geom_point()

p3 <- scaled_data %>%
      filter(Date == "15/1/2007" & Time > "07:00:00" & Time < "11:00:00") %>%
      ggplot(mapping = aes(x = Time, y = Global_active_power))
p3 + geom_point()

# From the plots we can see that from 07:00:00 - 11:00:00 on Mondays the data
# often has a spike before going back down, with occasional smaller spikes after
time_window_df <- data.frame()

# 60*24 = 1440 minutes per day, 1440*7 = 10080 minutes in a week
for (i in 1:52) {
    temp <- data.frame()
    temp <- scaled_data[(1 + 10080 * (i - 1)):(10080 * (i - 1) + (1440)), ]
    temp <- temp %>%
        filter(Time >= "07:00:00" & Time <= "11:00:00") %>%
        dplyr::select(Date, Time, Global_active_power)
    time_window_df <- rbind(time_window_df, temp)
}

# Question 2
# 12532 entries in the data set divided by 52 weeks = 241
# We need to try multiple nstates to find the best fit
# may not need to do all 13 possibilities if smart with it

# strategy: starting at 3 and 10, and use binary search to reduce search space


model_1state <- depmix(response = Global_active_power ~ 1,
                 data = time_window_df,
                 nstates = 1,
                 ntimes = rep(241, 52))
fit_model_1state <- fit(model_1state)
summary(fit_model_1state)
log_lik_1state <- logLik(fit_model_1state)
BIC_1state <- BIC(fit_model_1state)

model_3states <- depmix(response = Global_active_power ~ 1,
                 data = time_window_df,
                 nstates = 3,
                 ntimes = rep(241, 52))
fit_model_3states <- fit(model_3states)
summary(fit_model_3states)
log_lik_3states <- logLik(fit_model_3states)
BIC_3states <- BIC(fit_model_3states)

model_7states <- depmix(response = Global_active_power ~ 1,
                 data = time_window_df,
                 nstates = 7,
                 ntimes = rep(241, 52))
fit_model_7states <- fit(model_7states)
summary(fit_model_7states)
log_lik_7states <- logLik(fit_model_7states)
BIC_7states <- BIC(fit_model_7states)

model_10states <- depmix(response = Global_active_power ~ 1,
                 data = time_window_df,
                 nstates = 10,
                 ntimes = rep(241, 52))
fit_model_10states <- fit(model_10states)
summary(fit_model_10states)
log_lik_10states <- logLik(fit_model_10states)
BIC_10states <- BIC(fit_model_10states)

model_13states <- depmix(response = Global_active_power ~ 1,
                 data = time_window_df,
                 nstates = 13,
                 ntimes = rep(241, 52))
fit_model_13states <- fit(model_13states)
summary(fit_model_13states)
log_lik_13states <- logLik(fit_model_13states)
BIC_13states <- BIC(fit_model_13states)

model_performance <- data.frame(
    state_count = c(1, 3, 7, 10, 13),
    log_liks = c(log_lik_1state,
                 log_lik_3states,
                 log_lik_7states,
                 log_lik_10states,
                 log_lik_13states),
    BICs = c(BIC_1state, BIC_3states, BIC_7states, BIC_10states, BIC_13states)
)

# we want low BIC (complexity) and high likelihood(accuracy)

performance_plot <- model_performance %>%
                    ggplot() +
                    geom_line(aes(x = state_count,
                                  y = log_liks,
                                  colour = "red")) +
                    geom_line(aes(x = state_count,
                                  y = BICs,
                                  colour = "blue"))

# it seems the likelihood keeps increasing and BIC keeps dropping as we increase
# the number of states, maybe the time interval choice is incorrect, or
# implementation incorrect?
# I can't seem to find a correct way to add labels to the performance_plot
