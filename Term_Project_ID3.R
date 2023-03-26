library(ggplot2)
library(dplyr)
library(depmixS4)
library(stats)
library(stringr)
library(zoo)

setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
# setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path

## a random seed setup ensures reproducable results
set.seed(3)

df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")

anomalous1 <- read.table("Dataset_with_Anomalies_1.txt",
                        header = TRUE,
                        sep = ",")
anomalous2 <- read.table("Dataset_with_Anomalies_2.txt",
                         header = TRUE,
                         sep = ",")
anomalous3 <- read.table("Dataset_with_Anomalies_3.txt",
                         header = TRUE,
                         sep = ",")

### Part 1 choosing the features for the model

## interpolate N/A values
df_time_info_header <- df[, 1 : 2]
df_no_time_info <- df[, 3 : 9]
df_no_time_info <- na.approx(df_no_time_info)

## the first row has a N/A value that cannot be interpolated
## so I manually set it the same as that of the second row
df_no_time_info[1, 1] <- df_no_time_info[2, 1]
df <- cbind(df_time_info_header, df_no_time_info)

## Selecting subset for PCA,
## specifically Saturdays from 16/12/2006 to 1/12/2009 from 2:00pm - 7:00pm
df1 <- df %>% filter(Time >= "13:59:59" & Time <= "19:00:00")

## We know that in a 5 hour interval,
## there will be 60*5+1 = 301 observations per day
## (the first minute of the next hour is included)
## Along with 52 weeks in a year, from 16/12/2006 to 1/12/2009,
## that means we need to find 3 + 52 + 52 + 47 = 154 Saturdays
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

## sort the different fields by their importance to PC1
## PCn denotes the Principal Component with nth highest portion of variance
## the Principle component can explain in the data
## the importance of a field in the data to the Principal Component is measured
## by the absolute value of of its loading score in that Principal Component
## pick the top 2 or 3 as our features to train HMM with
importance <- sort(abs(pca$rotation[, 1]), decreasing = TRUE)
importanceFeatures <- names(importance[1:3])
pca$rotation[importanceFeatures, 1]
## results show that Global_active_power, Global_intensity and Sub_metering_1
## are the most important features in PC1

pcasum
## we can see that the first two PCs can explain about 60% of the data's
## variance, and thus we do not consider the rest PCs, as it brings
## unnecessary extra complexity to training the model, and possibly
## also more noise to be introduced
## Global_active_power, Global_intensity and Sub_metering_1 are the most
## important features in PC1, and since they are still very important
## considering their weighted average loading score, they are a good choice for
## features when training the HMM model.
## we may want to remove the feature with least importance to PC1 among
## the three, later if doing so improves the performance of the model

### Part 2 training and testing the model, finding the best # of states

## in terms of # of states selection, used hand-picked number of states equally
## spaced to find an approximate best range, then tried all possible # of states
## in that range to get the best parameter selection

dfresult <- selecteddata[, c(3, 6)]
weeks_before_2009 <- 154 - 47
obs_per_week <- 301
obs_first_week <- 97
entries_before_2009 <- obs_first_week + (obs_per_week * (weeks_before_2009 - 1))
df_train <- dfresult[1: entries_before_2009, ]
df_test <- dfresult[-(1: entries_before_2009), ]
total_obs_train <- nrow(df_train)
total_obs_test <- nrow(df_test)

## first pass: starting from 4 states, pick next # of states as previous + 4
## until our upper limit 24

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

## state numbers 20 and 24 failed to converge
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

## plot the model performances to choose the best range of # of states
model_performance <- data.frame(
    state_count = c(4, 8, 12, 14, 16),
    log_liks = c(log_lik_4states,
                 log_lik_8states,
                 log_lik_12states,
                 log_lik_14states,
                 log_lik_16states),
    BICs = c(BIC_4states,
             BIC_8states,
             BIC_12states,
             BIC_14states,
             BIC_16states)
)
temp <- model_performance %>% ggplot()
performance_plot <- temp +
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

## from the plot we can see that the model performance keeps improving
## from 4 to 16 states, but failed to converge at 20 states

## second pass, test each state in range [16, 20), and create new plot

model_17states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 17,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_17states <- fit(model_17states)
summary(fit_model_17states)
log_lik_17states <- logLik(fit_model_17states)
BIC_17states <- BIC(fit_model_17states)

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

## following model failed to converge
# model_19states <- depmix(response = list(Global_intensity ~ 1,
#                                          Global_active_power ~ 1),
#                          data = df_train,
#                          nstates = 19,
#                          ntimes = c(obs_first_week,
#                                     rep(obs_per_week, weeks_before_2009 - 1)),
#                          family = list(gaussian(), gaussian()))
# fit_model_19states <- fit(model_19states)
# summary(fit_model_19states)
# log_lik_19states <- logLik(fit_model_19states)
# BIC_19states <- BIC(fit_model_19states)

## plot the new model performances
## we can see that the performance plot has no turning point, 
## so the model with 18 states performs the best
## we will compare the # of states 16, 17 and 18 in with test set loglik
## just to be safe
model_performance2 <- data.frame(
    state_count = c(4, 8, 12, 14, 16, 17, 18),
    log_liks = c(log_lik_4states,
                 log_lik_8states,
                 log_lik_12states,
                 log_lik_14states,
                 log_lik_16states,
                 log_lik_17states,
                 log_lik_18states),
    BICs = c(BIC_4states,
             BIC_8states,
             BIC_12states,
             BIC_14states,
             BIC_16states,
             BIC_17states,
             BIC_18states)
)
temp <- model_performance2 %>% ggplot()
performance_plot2 <- temp +
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

performance_plot2

## testing log-likelihoods of the chosen models with the test set data

test_16states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 16,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_16states <- setpars(test_16states, getpars(fit_model_16states))
log_lik_test_16states <- forwardbackward(fit_test_16states)$logLike

test_17states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 17,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_17states <- setpars(test_17states, getpars(fit_model_17states))
log_lik_test_17states <- forwardbackward(fit_test_17states)$logLike

test_18states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 18,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_18states <- setpars(test_18states, getpars(fit_model_18states))
log_lik_test_18states <- forwardbackward(fit_test_18states)$logLike

normed_16states_loglik_train <- log_lik_16states / total_obs_train
normed_17states_loglik_train <- log_lik_17states / total_obs_train
normed_18states_loglik_train <- log_lik_18states / total_obs_train
normed_16states_loglik_test <- log_lik_test_16states / total_obs_test
normed_17states_loglik_test <- log_lik_test_17states / total_obs_test
normed_18states_loglik_test <- log_lik_test_18states / total_obs_test
log_lik_diff_16states <- abs(normed_16states_loglik_test -
                             normed_16states_loglik_train)
log_lik_diff_17states <- abs(normed_17states_loglik_test -
                             normed_17states_loglik_train)
log_lik_diff_18states <- abs(normed_18states_loglik_test -
                             normed_18states_loglik_train)

## TODO: draw a graph comparing the train and test log-likelihood
## of the three models using data above

## model with 18 states got least difference in log-likelihood between
## the train set and test set, and since it has also the best train set
## performance, we will use it for anomaly detection

### Part 3, use the best model from part 2 to test for anomalies

## assumes the three data spans the same range of time and date
anomalous1 <- anomalous1 %>% filter(Time >= "13:59:59" & Time <= "19:00:00")
anomalous2 <- anomalous2 %>% filter(Time >= "13:59:59" & Time <= "19:00:00")
anomalous3 <- anomalous3 %>% filter(Time >= "13:59:59" & Time <= "19:00:00")
beginning_week <- nrow(anomalous1[anomalous1$Date == "1/12/2009", ])
ending_week <- nrow(anomalous1[anomalous1$Date == "26/11/2010", ])
weeks_in_middle <- (nrow(anomalous1) - beginning_week - ending_week) /
                    obs_per_week
model_anomalous1 <- depmix(response = list(Global_intensity ~ 1,
                                           Global_active_power ~ 1),
                           data = anomalous1,
                           nstates = 18,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous1 <- setpars(model_anomalous1, getpars(fit_model_18states))
log_lik_anomalous1 <- forwardbackward(fit_anomalous1)$logLike
normed_loglik_anomalous1 <- log_lik_anomalous1 / 
                            (beginning_week + ending_week + nrow(anomalous1))

model_anomalous2 <- depmix(response = list(Global_intensity ~ 1,
                                           Global_active_power ~ 1),
                           data = anomalous2,
                           nstates = 18,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous2 <- setpars(model_anomalous2, getpars(fit_model_18states))
log_lik_anomalous2 <- forwardbackward(fit_anomalous2)$logLike
normed_loglik_anomalous2 <- log_lik_anomalous2 /
    (beginning_week + ending_week + nrow(anomalous2))

model_anomalous3 <- depmix(response = list(Global_intensity ~ 1,
                                           Global_active_power ~ 1),
                           data = anomalous3,
                           nstates = 18,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous3 <- setpars(model_anomalous3, getpars(fit_model_18states))
log_lik_anomalous3 <- forwardbackward(fit_anomalous3)$logLike
normed_loglik_anomalous3 <- log_lik_anomalous3 /
    (beginning_week + ending_week + nrow(anomalous3))

## TODO: draw a graph of the log_likelihoods of the three datasets above

## we can see that whereas the datasets 1 and 3 had about -2.29
## normalized log likelihood, dataset 2 had about -8.48
## this means the data in dataset 2 is e^(8.48-2.29) = 487.85 times lesser
## likely to happen given our trained model
## thus we can conclude that the dataset 2 is the anomalous one
