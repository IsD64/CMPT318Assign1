library(ggplot2)
library(dplyr)
library(depmixS4)
library(stats)
library(stringr)
library(zoo)

setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
# setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path

## a random seed setup ensures reproducible results
## 3 had 23 fail to converge
set.seed(4)

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

model_20states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 20,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_20states <- fit(model_20states)
summary(fit_model_20states)
log_lik_20states <- logLik(fit_model_20states)
BIC_20states <- BIC(fit_model_20states)

model_24states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 24,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_24states <- fit(model_24states)
summary(fit_model_24states)
log_lik_24states <- logLik(fit_model_24states)
BIC_24states <- BIC(fit_model_24states)

## plot the model performances to choose the best range of # of states
model_performance <- data.frame(
  state_count = c(4, 8, 12, 14, 16, 20, 24),
  log_liks = c(log_lik_4states,
               log_lik_8states,
               log_lik_12states,
               log_lik_14states,
               log_lik_16states,
               log_lik_20states,
               log_lik_24states),
  BICs = c(BIC_4states,
           BIC_8states,
           BIC_12states,
           BIC_14states,
           BIC_16states,
           BIC_20states,
           BIC_24states)
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
## from 4 to 24 states, so the best interval is (20, 24)
## so we check the interval to find the best model
## hidden states in range (20, 24)
model_21states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 21,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_21states <- fit(model_21states)
summary(fit_model_21states)
log_lik_21states <- logLik(fit_model_21states)
BIC_21states <- BIC(fit_model_21states)

model_22states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 22,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_22states <- fit(model_22states)
summary(fit_model_22states)
log_lik_22states <- logLik(fit_model_22states)
BIC_22states <- BIC(fit_model_22states)

# needs a different seed for the following model to converge
set.seed(3)
model_23states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 23,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), gaussian()))
fit_model_23states <- fit(model_23states)
summary(fit_model_23states)
log_lik_23states <- logLik(fit_model_23states)
BIC_23states <- BIC(fit_model_23states)

## plot the new model performances
## we can see that the performance plot has no turning point, 
## so the model with 24 states performs the best
## we will compare the # of states 22, 23 and 24 in with test set loglik
## just to be safe
model_performance2 <- data.frame(
  state_count = c(4, 8, 12, 14, 16, 20, 21, 22, 23, 24),
  log_liks = c(log_lik_4states,
               log_lik_8states,
               log_lik_12states,
               log_lik_14states,
               log_lik_16states,
               log_lik_20states,
               log_lik_21states,
               log_lik_22states,
               log_lik_23states,
               log_lik_24states
               ),
  BICs = c(BIC_4states,
           BIC_8states,
           BIC_12states,
           BIC_14states,
           BIC_16states,
           BIC_20states,
           BIC_21states,
           BIC_22states,
           BIC_23states,
           BIC_24states)
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

## testing log-likelihoods of our 3 best models,
## the models with 22, 23, and 24 hidden states, and find the one
## with best consistency between train and test dataset to use for
## anomaly detection
## best consistency means lowest difference between
## normalized train log-likelihood and normalized test log-likelihood

test_22states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 22,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_22states <- setpars(test_22states, getpars(fit_model_22states))
log_lik_test_22states <- forwardbackward(fit_test_22states)$logLike

test_23states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 23,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_23states <- setpars(test_23states, getpars(fit_model_23states))
log_lik_test_23states <- forwardbackward(fit_test_23states)$logLike

test_24states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_test,
                        nstates = 24,
                        ntimes = c(rep(obs_per_week, 48)),
                        family = list(gaussian(), gaussian()))
fit_test_24states <- setpars(test_24states, getpars(fit_model_24states))
log_lik_test_24states <- forwardbackward(fit_test_24states)$logLike

normed_22states_loglik_train <- log_lik_22states / total_obs_train
normed_23states_loglik_train <- log_lik_23states / total_obs_train
normed_24states_loglik_train <- log_lik_24states / total_obs_train
normed_22states_loglik_test <- log_lik_test_22states / total_obs_test
normed_23states_loglik_test <- log_lik_test_23states / total_obs_test
normed_24states_loglik_test <- log_lik_test_24states / total_obs_test
log_lik_diff_22states <- abs(normed_22states_loglik_test -
                                 normed_22states_loglik_train)
log_lik_diff_23states <- abs(normed_23states_loglik_test -
                                 normed_23states_loglik_train)
log_lik_diff_24states <- abs(normed_24states_loglik_test -
                                 normed_24states_loglik_train)


Num_Stats <- c("22 States", "23 States", "24 States")
Log_Likelihood_Test <- c(normed_22states_loglik_test,
                         normed_23states_loglik_test,
                         normed_24states_loglik_test)
Log_Likelihood_Train <- c(normed_22states_loglik_train,
                          normed_23states_loglik_train,
                          normed_24states_loglik_train)
Log_Likelihood_Diff <- c(log_lik_diff_22states,
                         log_lik_diff_23states,
                         log_lik_diff_24states)
log_like_df_test <- data.frame(Num_Stats)
log_like_df_train <- data.frame(Num_Stats)
log_like_df_diff <- data.frame(Num_Stats, Log_Likelihood_Diff)
log_like_df_test <- log_like_df_test %>%
                    mutate("Log-Likelihood" = abs(Log_Likelihood_Test)) %>%
                    mutate("Dataset" = "Test")
log_like_df_train <- log_like_df_train %>%
                     mutate("Log-Likelihood" = abs(Log_Likelihood_Train)) %>%
                     mutate("Dataset" = "Train")
log_like_df <- rbind(log_like_df_test, log_like_df_train)

p <- log_like_df %>%
     ggplot(mapping = aes(x = Num_Stats, y = `Log-Likelihood`, fill = Dataset))
p + geom_bar(position = "dodge", stat = "identity") +
    ylim(0, 2) +
    labs(y = "|Log-Likelihood|",
         fill = "Number of States",
         x = "Number of States",
         title = "Test and Train Log-Likelihood per Number of States")
p2 <- log_like_df_diff %>%
      ggplot(mapping = aes(x = Num_Stats, y = Log_Likelihood_Diff))
p2 + geom_bar(stat = "identity", fill = "blue") +
     ylim(0, 0.3) +
     labs(x = "Number of States",
          title = "Difference in Log-Likelihood Between Test and Train",
          y = "|Log-Likelihood|")

## model with 24 states got least difference in log-likelihood between
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
                           nstates = 24,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous1 <- setpars(model_anomalous1, getpars(fit_model_24states))
log_lik_anomalous1 <- forwardbackward(fit_anomalous1)$logLike
normed_loglik_anomalous1 <- log_lik_anomalous1 / 
  (beginning_week + ending_week + nrow(anomalous1))

model_anomalous2 <- depmix(response = list(Global_intensity ~ 1,
                                           Global_active_power ~ 1),
                           data = anomalous2,
                           nstates = 24,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous2 <- setpars(model_anomalous2, getpars(fit_model_24states))
log_lik_anomalous2 <- forwardbackward(fit_anomalous2)$logLike
normed_loglik_anomalous2 <- log_lik_anomalous2 /
  (beginning_week + ending_week + nrow(anomalous2))

model_anomalous3 <- depmix(response = list(Global_intensity ~ 1,
                                           Global_active_power ~ 1),
                           data = anomalous3,
                           nstates = 24,
                           ntimes = c(beginning_week,
                                      rep(obs_per_week, weeks_in_middle),
                                      ending_week),
                           family = list(gaussian(), gaussian()))
fit_anomalous3 <- setpars(model_anomalous3, getpars(fit_model_24states))
log_lik_anomalous3 <- forwardbackward(fit_anomalous3)$logLike
normed_loglik_anomalous3 <- log_lik_anomalous3 /
  (beginning_week + ending_week + nrow(anomalous3))

Anoma_list <- c("Dataset 1", "Dataset 2", "Dataset 3")
Anoma_log_lik <- c(log_lik_anomalous1,
                   log_lik_anomalous2,
                   log_lik_anomalous3)
Anoma_norm_log_lik <- c(normed_loglik_anomalous1,
                        normed_loglik_anomalous2,
                        normed_loglik_anomalous3)

Anoma_log_df <- data.frame(Anoma_list) %>%
                mutate(log_lik = Anoma_log_lik, type = "")
Anoma_norm_log_df <- data.frame(Anoma_list) %>%
                     mutate(log_lik = Anoma_norm_log_lik, type = "")

p3 <- Anoma_norm_log_df %>%
      ggplot(mapping = aes(x = type, y = abs(log_lik), fill = Anoma_list))
p3 + geom_bar(position = "dodge",
             stat = "identity") +
     labs(x = "", 
          fill = "Anamalous Dataset",
          y = "|Log-Likelihood|",
          title = "Normalized Log-Likelihood of Anamalous Datasets")

## we can see that whereas the datasets 1 and 3 had about -2.5
## normalized log likelihood, dataset 2 had about -9
## this means the data in dataset 2 is e^(9-2.5) = 665 times lesser
## likely to happen given our trained model
## thus we can conclude that the dataset 2 is the anomalous one