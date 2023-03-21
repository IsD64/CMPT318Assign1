library(tidyverse)
library(ggplot2)
library(dplyr)
library(depmixS4)
library(stats)
library(stringr)

#setwd("C:/Users/24312/Documents/code/R") is Issac's path
setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path
df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")

# Removing NA values
df[is.na(df)] = 0


# Selecting subset for PCA, specifically Saturdays from 16/12/2006 to 1/12/2009 from 2:00pm - 7:00pm
df1 <- df %>% filter(Time >= "13:59:59" & Time <= "19:00:00")

#We know that in a 5 hour interval, there will be 60*5+1 = 301 observations per day (the first minute of the next hour is included)
#Along with 52 weeks in a year, from 16/12/2006 to 1/12/2009 that means we need to find 3 + 52 + 52 + 47 = 154 Saturdays

df2 <- df1[1:97,]

tempdf <- tail(df1,325185-1903)

for (i in 0:153) {
  temp <- tempdf[(1 + (301 * 7 * i)):(301 + (301 * 7 * i)), ]
  df2 <- rbind(df2, temp)
}

selecteddata <- df2

df2 <- df2 %>%
  group_by(Date) %>%
  summarise(Global_Active_Power = mean(Global_active_power),
            Global_Reactive_Power = mean(Global_reactive_power),
            Voltage = mean(Voltage),
            Global_Intensity = mean(Global_intensity),
            Sub_Metering_1 = mean(Sub_metering_1),
            Sub_Metering_2 = mean(Sub_metering_2),
            Sub_Metering_3 = mean(Sub_metering_3))

dfpca <- as.data.frame(scale(df2[2:8]))

pca <- prcomp(dfpca)
pcasum <- summary(pca)

biplot(pca, cex = c(0.3, 0.5), col = c("black", "red"),
       arrow.len = 0.1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1))

pcasum
# From the importance, we can see that using 5 principal components allows
# the data to explain 95% of the variance. Therefore we will use 5 components.
# The 5 components will be selected 
# by comparing magnitudes in each principal component

pca
# Component 1: Global_Intensity
# Component 2: Voltage
# Component 3: Sub_Metering_2
# Component 4: Sub_Metering_3
# Component 5: Global_Reactive_Power

dfresult <- selecteddata %>% select(Date,
                                    Time,
                                    Global_intensity,
                                    Voltage,
                                    Sub_metering_2,
                                    Sub_metering_3,
                                    Global_reactive_power)
weeks_before_2009 = (154 - 47)
obs_per_week = 301
df_train <- dfresult[1 : (weeks_before_2009 * obs_per_week), ]
df_test <- setdiff(dfresult, df_train)

model_4states <- depmix(response = Global_intensity ~ 1,
                        data = df_train,
                        nstates = 4,
                        ntimes = rep(obs_per_week, weeks_before_2009))
fit_model_4states <- fit(model_4states)
summary(fit_model_4states)
log_lik_4states <- logLik(fit_model_4states)
BIC_4states <- BIC(fit_model_4states)

model_24states <- depmix(response = Global_intensity ~ 1,
                         data = df_train,
                         nstates = 24,
                         ntimes = rep(obs_per_week, weeks_before_2009))
fit_model_24states <- fit(model_24states)
summary(fit_model_24states)
log_lik_24states <- logLik(fit_model_24states)
BIC_24states <- BIC(fit_model_24states)

running <- TRUE
loop_count <- 0
intervals <- matrix(c(4, 24), nrow = 1, ncol = 2)
model_performance <- data.frame(
  state_count = c(4, 24),
  log_liks = c(log_lik_4states, log_lik_24states),
  BICs = c(BIC_4states, BIC_24states)
)

# TODO
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
  model_midpoint_states <- depmix(response = Global_intensity ~ 1,
                                  data = df_train,
                                  nstates = midpoint,
                                  ntimes = rep(obs_per_week,
                                               weeks_before_2009))
  
  error_happened <- FALSE
  tryCatch({ fit_model_midpoint_states <- fit(model_midpoint_states) },
           error = function(e) { error_happened <- TRUE })
  if(error_happened) {
    intervals <- intervals[-1, ]
    next
  }
  
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