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

# GPS moving: mean speed (for likely error)

gps_moving <- read_sf("data/gps_moving.gpkg")%>%
    mutate(time = as.numeric(as.POSIXct(time, tz="Paris/Europe")),type = ifelse(track_fid ==3, "boat", "walk"))

gps_moving$speed <- atl_get_speed(gps_moving, x = "gps_x", y = "gps_y", time = "time")

gps <- gps_moving %>%
    group_by(type) %>%
    mutate(speed = replace(speed, row_number() == 1, NA))%>%
    summarise(mean_speed = mean(speed, na.rm=T), SD_speed = sd(speed, na.rm=T))
gps

# Case study - fix rate

fixes <- read.csv("data/griend_respatches2020_fixrates.csv", stringsAsFactors = F)

length(unique(fixes$id)) # n birds
length(unique(fixes$tideID)) # n birds

mean(fixes$fix_rate)
sd(fixes$fix_rate)

