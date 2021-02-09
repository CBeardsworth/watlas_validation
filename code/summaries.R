Sys.setenv(TZ='UTC') # change time zone for session

library(dplyr)

# Stationary: Raw ----

stat <- read.csv("data/stat_raw.csv")%>%
    mutate(start = as.POSIXct(start, format = "%Y/%m/%d %H:%M:%S"), 
           end = as.POSIXct(end, format = "%Y/%m/%d %H:%M:%S"),
           Timestamp = as.POSIXct(start, format = "%Y/%m/%d %H:%M:%S"))

stat %>% #overall error
    group_by(ID)%>%
    summarize(n = length(error_dist), fix_rate= round((n/300)*100,1),mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

stat %>% #error by site ID
    summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

#Stationary: Filtered----

stat_filt <- read.csv("data/stat_filt.csv")%>%
    mutate(start = as.POSIXct(start, format = "%Y/%m/%d %H:%M:%S"), 
           end = as.POSIXct(end, format = "%Y/%m/%d %H:%M:%S"),
           Timestamp = as.POSIXct(start, format = "%Y/%m/%d %H:%M:%S"))


stat_filt %>% #overall error
        summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

stat_filt %>% #error by site ID
    group_by(ID)%>%
    summarize(n = length(error_dist), fix_rate= round((n/300)*100,1),mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
#Moving: Raw ----

movingPoints <- read.csv("data/moving_raw.csv")

movingPoints %>% #overall error
    filter(time_diff < 2)%>%
    summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

movingPoints %>% #error by number of receivers
    filter(time_diff < 2)%>%
    group_by(NBS)%>%
    summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

# Moving: Filtered -----

movingPoints_filt <- read.csv("data/moving_filt.csv")

movingPoints_filt %>% #overall error
    filter(time_diff < 2)%>%
    summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

movingPoints_filt %>% #error by number of receivers
    filter(time_diff < 2)%>%
    group_by(NBS)%>%
    summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))

