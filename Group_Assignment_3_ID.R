library(ggplot2)
library(dplyr)
library(depmixS4)
# library(tidyverse)

setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
# setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Group_Assignment_3_Dataset.txt", header = TRUE, sep = ",")

# Question 1

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

# strategy: we know that the log likelihood and BIC have
# effective maximum and minimum values, therefore we need to find a
# point that is the critical point for both measurements.
# a critical point has a negative derivative to its one side, and
# positive derivative to it's other. using this and knowing we want a
# minimum / maximum value of the BIC / log likelihood, we can eliminate
# intervals using a binary-search like approach.

# 0. for each interval considered:
# 1. take the middle point, and check the slope from the left endpoint and right
# endpoint to that middle point.
# 2. if the left half and right half both have a positive/negative slope for an
# performance measure, choose the interval has the better performance and run
# this method again from step 0.
# 3. if the intervals do not agree in sign of slope for the performance measures
# consider both intervals and perform this method again from step 0 until a
# critcal point is found.
# 4. I'm not sure I thought this through correctly, may need improvising between
# iterations.

#Iteration 1: endpoints are 3 and 16 number of states, choose 10 as middle point

model_3states <- depmix(response = Global_active_power ~ 1,
                        data = time_window_df,
                        nstates = 3,
                        ntimes = rep(241, 52))
fit_model_3states <- fit(model_3states)
summary(fit_model_3states)
log_lik_3states <- logLik(fit_model_3states)
BIC_3states <- BIC(fit_model_3states)

model_16states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 16,
                         ntimes = rep(241, 52))
fit_model_16states <- fit(model_16states)
summary(fit_model_16states)
log_lik_16states <- logLik(fit_model_16states)
BIC_16states <- BIC(fit_model_16states)

model_10states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 10,
                         ntimes = rep(241, 52))
fit_model_10states <- fit(model_10states)
summary(fit_model_10states)
log_lik_10states <- logLik(fit_model_10states)
BIC_10states <- BIC(fit_model_10states)

model_performance <- data.frame(
    state_count = c(3, 10, 16),
    log_liks = c(log_lik_3states,
                 log_lik_10states,
                 log_lik_16states),
    BICs = c(BIC_3states, BIC_10states, BIC_16states)
)

performance_plot <- model_performance %>%
                    ggplot() +
                    geom_line(aes(x = state_count,
                                  y = log_liks,
                                  colour = "red")) +
                    geom_line(aes(x = state_count,
                                  y = BICs,
                                  colour = "blue"))

# Iteration 2: endpoints 10 and 16, choose middlepoint 13

model_13states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 13,
                         ntimes = rep(241, 52))
fit_model_13states <- fit(model_13states)
summary(fit_model_13states)
log_lik_13states <- logLik(fit_model_13states)
BIC_13states <- BIC(fit_model_13states)

model_performance <- data.frame(
    state_count = c(3, 10, 13, 16),
    log_liks = c(log_lik_3states,
                 log_lik_10states,
                 log_lik_13states,
                 log_lik_16states),
    BICs = c(BIC_3states, BIC_10states, BIC_13states, BIC_16states)
)

performance_plot <- model_performance %>%
                    ggplot() +
                    geom_line(aes(x = state_count,
                                  y = log_liks,
                                  colour = "red")) +
                    geom_line(aes(x = state_count,
                                  y = BICs,
                                  colour = "blue"))

# Iteration 3: the slopes on left/right don't agree
# choose intervals [10, 13] with midpoint 12, [13, 16] with midpoint 14

model_12states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 12,
                         ntimes = rep(241, 52))
fit_model_12states <- fit(model_12states)
summary(fit_model_12states)
log_lik_12states <- logLik(fit_model_12states)
BIC_12states <- BIC(fit_model_12states)

model_14states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 14,
                         ntimes = rep(241, 52))
fit_model_14states <- fit(model_14states)
summary(fit_model_14states)
log_lik_14states <- logLik(fit_model_14states)
BIC_14states <- BIC(fit_model_14states)

model_performance <- data.frame(
    state_count = c(3, 10, 12, 13, 14, 16),
    log_liks = c(log_lik_3states,
                 log_lik_10states,
                 log_lik_12states,
                 log_lik_13states,
                 log_lik_14states,
                 log_lik_16states),
    BICs = c(BIC_3states,
             BIC_10states,
             BIC_12states,
             BIC_13states,
             BIC_14states,
             BIC_16states)
)

performance_plot <- model_performance %>%
                    ggplot() +
                    geom_line(aes(x = state_count,
                                  y = log_liks,
                                  colour = "red")) +
                    geom_line(aes(x = state_count,
                                  y = BICs,
                                  colour = "blue"))

# Iteration 4: found disagreeing interval at 14, test [14, 16] with midpoint 15

model_15states <- depmix(response = Global_active_power ~ 1,
                         data = time_window_df,
                         nstates = 15,
                         ntimes = rep(241, 52))
fit_model_15states <- fit(model_15states)
summary(fit_model_15states)
log_lik_15states <- logLik(fit_model_15states)
BIC_15states <- BIC(fit_model_15states)

model_performance <- data.frame(
    state_count = c(3, 10, 12, 13, 14, 15, 16),
    log_liks = c(log_lik_3states,
                 log_lik_10states,
                 log_lik_12states,
                 log_lik_13states,
                 log_lik_14states,
                 log_lik_15states,
                 log_lik_16states),
    BICs = c(BIC_3states,
             BIC_10states,
             BIC_12states,
             BIC_13states,
             BIC_14states,
             BIC_15states,
             BIC_16states)
)

performance_plot <- model_performance %>%
                    ggplot() +
                    geom_line(aes(x = state_count,
                                  y = log_liks,
                                  colour = "red")) +
                    geom_line(aes(x = state_count,
                                  y = BICs,
                                  colour = "blue"))

# conclusion: found critical point at #of states 14 for both BIC and log likelihood.
