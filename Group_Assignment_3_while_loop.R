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

# calculate the BICs and log likelihoods at the endpoints first

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

model_performance <- data.frame(
    state_count = c(3, 16),
    log_liks = c(log_lik_3states, log_lik_16states),
    BICs = c(BIC_3states, BIC_16states)
)
loop_count <- 0
running <- TRUE
intervals <- matrix(c(3, 16), nrow = 1, ncol = 2)
while (running) {
    loop_count <- loop_count + 1
    if (length(intervals) > 2) {
        next_interval <- intervals[1, ]
    } else {
        next_interval <- intervals
    }
    start <- next_interval[1]
    end <- next_interval[2]
    # if the two endpoints are next to each other, skip this interval
    if ((end - start) <= 1) {
        if (length(intervals) > 2) {
            # remove this current interval if it is not the last
            intervals <- intervals[-1, ]
        } else {
            # if we are in this branch, this means we skipped the last item in
            # our intervals list, end the while loop
            running <- FALSE
        }
        next
    }

    # compute the log likelihood and BIC at the midpoint # of states
    midpoint <- ceiling((start + end) / 2)
    model_midpoint_states <- depmix(response = Global_active_power ~ 1,
                                    data = time_window_df,
                                    nstates = midpoint,
                                    ntimes = rep(241, 52))
    fit_model_midpoint_states <- fit(model_midpoint_states)
    summary(fit_model_midpoint_states)
    log_lik_midpoint_states <- logLik(fit_model_midpoint_states)
    BIC_midpoint_states <- BIC(fit_model_midpoint_states)

    new_point <- data.frame(
    state_count = midpoint,
    log_liks    = log_lik_midpoint_states,
    BICs        = BIC_midpoint_states
    )

    # find the index to insert the new point in the performance_plot
    index <- 1
    found <- FALSE
    while ((index <= nrow(model_performance)) && (!found)) {
        if (model_performance[index, 1] >= midpoint) {
            found <- TRUE
        } else {
            index <- index + 1
        }
    }

    model_performance <- rbind(model_performance[1:index - 1, ],
                               new_point,
                               model_performance[-(1:index - 1), ])

    check_left_subinterval <- FALSE
    check_right_subinterval <- FALSE

    prev_log_lik <- model_performance[index - 1, 2]
    next_log_lik <- model_performance[index + 1, 2]
    prev_BIC <- model_performance[index - 1, 3]
    next_BIC <- model_performance[index + 1, 3]

    # if the log likelihood keeps ascending from prev point to midpoint to
    # next point, only check right subinterval, else check both
    if ((log_lik_midpoint_states - prev_log_lik >= 0) &&
        (next_log_lik - log_lik_midpoint_states >= 0)) {
        check_right_subinterval <- TRUE
    } else {
        check_right_subinterval <- TRUE
        check_left_subinterval <- TRUE
    }

    # if the BIC keeps decending from prev point to midpoint to
    # next point, only check right subinterval, else check both
    if ((prev_BIC - BIC_midpoint_states >= 0) &&
        (BIC_midpoint_states - next_BIC >= 0)) {
        check_right_subinterval <- TRUE
    } else {
        check_right_subinterval <- TRUE
        check_left_subinterval <- TRUE
    }

    # add the subintervals we need to calculate into intervals
    # to deal with them in the later while loops
    remove_first <- TRUE
    if (check_right_subinterval) {
        if (length(intervals) <= 2) {
            # reassigning a matrix effectively removes first item in original
            # intervals
            remove_first <- FALSE
            intervals <- matrix(c(midpoint, end), nrow = 1, ncol = 2)
        } else {
            intervals <- rbind(intervals, c(midpoint, end))
        }
    }
    if (check_left_subinterval) {
        intervals <- rbind(intervals, c(start, midpoint))
    }
    if (remove_first) {
        intervals <- intervals[-1, ]
    }
}

p4 <- model_performance %>%
  ggplot()
performance_plot <- p4 +
  geom_line(aes(x = state_count,
                y = log_liks,
                colour = "red")) +
  geom_line(aes(x = state_count,
                y = BICs,
                colour = "blue")) + 
  scale_color_discrete(name = "",
                       labels = c("BICs","Log_Likelihood")) +
  labs(title = "Performance of Log Likelihood and BIC per Number of States",
       x = "Number of States",
       y = "")

performance_plot

# find the smallest point with the highest log likelihood and lowest BIC
# between the point where the BICs' slope goes from negative to positive
# and the point where the log likelihood's slope goes from positive to 
# negative