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


setwd("C:/Users/24312/Documents/code/R") # is Isaac's path
# setwd("C:/Users/thund/Desktop/Cmpt 318") # is Vincent's path

# a random seed setup ensures reproducable results
set.seed(3)

df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")

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

dfpca <- as.data.frame(selecteddata[3:8]) # df2[2:8]

pca <- prcomp(dfpca, scale = TRUE)
pcasum <- summary(pca)

plot(pca$x[, 1], pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)
barplot(pca.var.per,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

biplot(pca, cex = c(0.3, 0.5), col = c("black", "red"),
       arrow.len = 0.1, ylim = c(-0.1, 0.1), xlim = c(-0.1, 0.1))

importance <- sort(abs(pca$rotation[, 1]), decreasing = TRUE)
importanceFeatures <- names(importance[1:3])
pca$rotation[importanceFeatures, 1]

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

dfresult <- selecteddata[, c(3,6)]
weeks_before_2009 = 154 - 47
obs_per_week <- 301
obs_first_week <- 97
entries_before_2009 <- obs_first_week + (obs_per_week * (weeks_before_2009 - 1))
df_train <- dfresult[1: entries_before_2009, ]
df_test <- setdiff(dfresult, df_train)

model_4states <- depmix(response = list(Global_intensity ~ 1,
                                        Global_active_power ~ 1),
                        data = df_train,
                        nstates = 4,
                        ntimes = c(obs_first_week,
                                   rep(obs_per_week, weeks_before_2009 - 1)),
                        family = list(gaussian(), multinomial()))
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
                        family = list(gaussian(), multinomial()))
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
                         family = list(gaussian(), multinomial()))
fit_model_12states <- fit(model_12states)
summary(fit_model_12states)
log_lik_12states <- logLik(fit_model_12states)
BIC_12states <- BIC(fit_model_12states)

# training more than 14 states seems to diverge
model_14states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 14,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), multinomial()))
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
                         family = list(gaussian(), multinomial()))
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
                         family = list(gaussian(), multinomial()))
fit_model_18states <- fit(model_18states)
summary(fit_model_18states)
log_lik_18states <- logLik(fit_model_18states)
BIC_18states <- BIC(fit_model_18states)

model_20states <- depmix(response = list(Global_intensity ~ 1,
                                         Global_active_power ~ 1),
                         data = df_train,
                         nstates = 20,
                         ntimes = c(obs_first_week,
                                    rep(obs_per_week, weeks_before_2009 - 1)),
                         family = list(gaussian(), multinomial()))
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
                         family = list(gaussian(), multinomial()))
fit_model_24states <- fit(model_24states)
summary(fit_model_24states)
log_lik_24states <- logLik(fit_model_24states)
BIC_24states <- BIC(fit_model_24states)

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