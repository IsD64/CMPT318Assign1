library(ggplot2)
library(dplyr)
library(depmixS4)
library(stats)
library(stringr)
library(zoo)

# TODO:
# test HMM learning with 3 features
# learn how to use fitted model on new data
# test the trained model, find almost equal log_lik
# use tested training model to find anamolous dataset

# setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path

# a random seed setup ensures reproducable results
set.seed(3)

df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")

## Part 1

# interpolate N/A values
df_time_info_header <- df[, 1 : 2]
df_no_time_info <- df[, 3 : 9]
df_no_time_info <- na.approx(df_no_time_info)

# the first row has a N/A value that cannot be interpolated
# so I manually set it the same as that of the second row
df_no_time_info[1, 1] <- df_no_time_info[2, 1]
df <- cbind(df_time_info_header, df_no_time_info)

# Selecting subset for PCA,
# specifically Saturdays from 16/12/2006 to 1/12/2009 from 2:00pm - 7:00pm
df1 <- df %>% filter(Time >= "13:59:59" & Time <= "19:00:00")

# We know that in a 5 hour interval,
# there will be 60*5+1 = 301 observations per day
# (the first minute of the next hour is included)
# Along with 52 weeks in a year, from 16/12/2006 to 1/12/2009,
# that means we need to find 3 + 52 + 52 + 47 = 154 Saturdays
df2 <- df1[1:97, ]

tempdf <- tail(df1, 325185 - 1903)

for (i in 0:153) {
  temp <- tempdf[(1 + (301 * 7 * i)) : (301 + (301 * 7 * i)), ]
  df2 <- rbind(df2, temp)
}

selecteddata <- df2

dfpca <- as.data.frame(selecteddata[3 : 8])

pca <- prcomp(dfpca, scale = TRUE)
pcasum <- summary(pca)

biplot(pca,
       cex = c(0.3, 0.5),
       col = c("black", "red"),
       arrow.len = 0.1,
       ylim = c(-0.03, 0.03),
       xlim = c(-0.03, 0.015))

# sort the different fields by their importance to PC1
# PCn denotes the Principal Component with nth highest portion of variance
# the Principle component can explain in the data 
# the importance of a field in the data to the Principal Component is measured
# by the absolute value of of its loading score in that Principal Component 
# pick the top 2 or 3 as our features to train HMM with
importance <- sort(abs(pca$rotation[, 1]), decreasing = TRUE)
importanceFeatures <- names(importance[1:3])
pca$rotation[importanceFeatures, 1]
# results show that Global_active_power, Global_intensity and Sub_metering_1
# are the most important features in PC1

pcasum
# we can see that the first two PCs can explain about 60% of the data's
# variance, and thus we do not consider the rest PCs, as it brings
# unnecessary extra complexity to training the model, and possibly
# also more noise to be introduced
# Global_active_power, Global_intensity and Sub_metering_1 are the most
# important features in PC1, and since they are still very important
# considering their weighted average loading score, they are a good choice for
# features when training the HMM model.
# we may want to remove the feature with least importance to PC1 among the
# three, Sub_metering_1, later if doing so improves the performance of the model

## Part 2, Work In Progress

dfresult <- selecteddata[, c(3, 6)]
weeks_before_2009 <- 154 - 47
obs_per_week <- 301
obs_first_week <- 97
entries_before_2009 <- obs_first_week + (obs_per_week * (weeks_before_2009 - 1))
df_train <- dfresult[1: entries_before_2009, ]
#I think this is an issue because setdiff removes all the rows in dfresult that are also in df_train. 
#However, these data frames only contain two columns, meaning it is completely possible for them to be equal by coincidence
#By running tail(dfresult,obs_per_week*47), I found an additional 5000 entries than in df_test
df_test <- setdiff(dfresult, df_train)

model_4states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_train,
                        nstates = 4,
                        ntimes = c(obs_first_week,
                                   rep(obs_per_week, weeks_before_2009 - 1)),
                        family = list(gaussian(), gaussian()))
fit_model_4states <- fit(model_4states)
summary(fit_model_4states)
log_lik_4states <- logLik(fit_model_4states)
BIC_4states <- BIC(fit_model_4states)

model_8states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_train,
                        nstates = 8,
                        ntimes = c(obs_first_week,
                                   rep(obs_per_week, weeks_before_2009 - 1)),
                        family = list(gaussian(), gaussian()))
fit_model_8states <- fit(model_8states)
summary(fit_model_8states)
log_lik_8states <- logLik(fit_model_8states)
BIC_8states <- BIC(fit_model_8states)

model_12states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 12,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_12states <- fit(model_12states)
summary(fit_model_12states)
log_lik_12states <- logLik(fit_model_12states)
BIC_12states <- BIC(fit_model_12states)

model_14states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 14,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_14states <- fit(model_14states)
summary(fit_model_14states)
log_lik_14states <- logLik(fit_model_14states)
BIC_14states <- BIC(fit_model_14states)

model_16states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 16,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_16states <- fit(model_16states)
summary(fit_model_16states)
log_lik_16states <- logLik(fit_model_16states)
BIC_16states <- BIC(fit_model_16states)

model_18states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 18,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_18states <- fit(model_18states)
summary(fit_model_18states)
log_lik_18states <- logLik(fit_model_18states)
BIC_18states <- BIC(fit_model_18states)

# model_20states <- depmix(response = list(Global_intensity ~ 1,
#                                          Global_active_power ~ 1,
#                                          Sub_metering_1 ~ 1),
#                          data = df_train,
#                          nstates = 20,
#                          ntimes = c(obs_first_week,
#                                     rep(obs_per_week, weeks_before_2009 - 1)),
#                          family = list(gaussian(), gaussian(), gaussian()))
# fit_model_20states <- fit(model_20states)
# summary(fit_model_20states)
# log_lik_20states <- logLik(fit_model_20states)
# BIC_20states <- BIC(fit_model_20states)
# 
# model_24states <- depmix(response = list(Global_intensity ~ 1,
#                                          Global_active_power ~ 1,
#                                          Sub_metering_1 ~ 1),
#                          data = df_train,
#                          nstates = 24,
#                          ntimes = c(obs_first_week,
#                                     rep(obs_per_week, weeks_before_2009 - 1)),
#                          family = list(gaussian(), gaussian(), gaussian()))
# fit_model_24states <- fit(model_24states)
# summary(fit_model_24states)
# log_lik_24states <- logLik(fit_model_24states)
# BIC_24states <- BIC(fit_model_24states)

model_performance <- data.frame(
  state_count = c(4, 8, 12, 14, 16, 18, 20, 24),
  log_liks = c(log_lik_4states,
               log_lik_8states,
               log_lik_12states,
               log_lik_14states,
               log_lik_16states,
               log_lik_18states,
               log_lik_20states,
               log_lik_24states),
  BICs = c(BIC_4states,
           BIC_8states,
           BIC_12states,
           BIC_14states,
           BIC_16states,
           BIC_18states,
           BIC_20states,
           BIC_24states)
)

p4 <- model_performance %>% ggplot()
performance_plot <- p4 +
  geom_line(aes(x = state_count,
                y = log_liks,
                colour = "red")) +
  geom_line(aes(x = state_count,
                y = BICs,
                colour = "blue")) +
  scale_color_discrete(name = "",
                       labels = c("BICs", "Log_Likelihood")) +
  labs(title = "Performance of Log Likelihood and BIC per Number of States",
       x = "Number of States",
       y = "")

performance_plot

obs_last_week = 9935 - (301 * 33)

test_14states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 14,
                        ntimes = c(rep(obs_per_week,33),
                                   obs_last_week),
                        family = list(gaussian(), gaussian()))
fit_test_14states <- setpars(test_14states, getpars(fit_model_14states))
log_lik_test_14states <- forwardbackward(fit_test_14states)$logLike

test_16states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 16,
                        ntimes = c(rep(obs_per_week,33),
                                   obs_last_week),
                        family = list(gaussian(), gaussian()))
fit_test_16states <- setpars(test_16states, getpars(fit_model_16states))
log_lik_test_16states <- forwardbackward(fit_test_16states)$logLike

test_18states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 18,
                        ntimes = c(rep(obs_per_week,33),
                                   obs_last_week),
                        family = list(gaussian(), gaussian()))
fit_test_18states <- setpars(test_18states, getpars(fit_model_18states))
log_lik_test_18states <- forwardbackward(fit_test_18states)$logLike

normalized_train_14states_loglik <- log_lik_14states / 32003
normalized_train_16states_loglik <- log_lik_16states / 32003
normalized_train_18states_loglik <- log_lik_18states / 32003
normalized_test_14states_loglik <- log_lik_test_14states / 9935
normalized_test_16states_loglik <- log_lik_test_16states / 9935
normalized_test_18states_loglik <- log_lik_test_18states / 9935
log_lik_diff_14states <- abs(normalized_test_14states_loglik -
                               normalized_train_14states_loglik)
log_lik_diff_16states <- abs(normalized_test_16states_loglik -
                               normalized_train_16states_loglik)
log_lik_diff_18states <- abs(normalized_test_18states_loglik -
                               normalized_train_18states_loglik)