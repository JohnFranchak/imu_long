library(timetk)
library(lubridate)
library(here)
library(janitor)
library(slider)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
source(here("step1-synchronization","motion_features.R"))

id <- 102
session <- 1
start_time <- "2021-07-12 12:00:00"
end_time <- "2021-07-12 20:00:00"

#READ IN DATA
lankle_acc <- read_csv(here(id,session, "imu", "left_ankle","accel.csv"), skip = 1,  col_names = c("time", "laacc_x", "laacc_y", "laacc_z")) 
rankle_acc <- read_csv(here(id,session, "imu", "right_ankle","accel.csv"), skip = 1, col_names = c("time", "raacc_x", "raacc_y", "raacc_z")) 
lhip_acc <- read_csv(here(id,session, "imu", "left_hip","accel.csv"), skip = 1, col_names = c("time", "lhacc_x", "lhacc_y", "lhacc_z")) 
rhip_acc <- read_csv(here(id,session, "imu", "right_hip","accel.csv"), skip = 1, col_names = c("time", "rhacc_x", "rhacc_y", "rhacc_z")) 
lankle_gyr <- read_csv(here(id,session, "imu", "left_ankle","gyro.csv"), skip = 1,  col_names = c("time", "lagyr_x", "lagyr_y", "lagyr_z")) 
rankle_gyr <- read_csv(here(id,session, "imu", "right_ankle","gyro.csv"), skip = 1, col_names = c("time", "ragyr_x", "ragyr_y", "ragyr_z")) 
lhip_gyr <- read_csv(here(id,session, "imu", "left_hip","gyro.csv"), skip = 1, col_names = c("time", "lhgyr_x", "lhgyr_y", "lhgyr_z")) 
rhip_gyr <- read_csv(here(id,session, "imu", "right_hip","gyro.csv"), skip = 1, col_names = c("time", "rhgyr_x", "rhgyr_y", "rhgyr_z")) 

sensor_data <- c("lankle_acc", "rankle_acc", "lhip_acc", "rhip_acc", "lankle_gyr", "rankle_gyr", "lhip_gyr","rhip_gyr")

filter_and_fix_time <- function(data_string, start_time, end_time) {
  temp_ds <- get(data_string)
  fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")
  
  temp_ds <- temp_ds %>% 
      mutate(time = fix_biostamp_time(time)) %>% 
      filter_by_time(time, start_time, end_time) 
  assign(paste0(data_string,"_filt"), temp_ds, envir = .GlobalEnv)
}

sdfilt <- map_chr(sensor_data, ~paste0(.x,"_filt"))
walk(sensor_data, ~ filter_and_fix_time(.x, start_time, end_time))

ds <-  full_join(get(sdfilt[1]), get(sdfilt[2])) 
for (i in 3:length(sdfilt)){
  ds <- full_join(ds, get(sdfilt[i]))
}
ds <- ds %>% arrange(time)

ds  <- ds %>% mutate(across(-time, ~ts_impute_vec(.x)))

#ds %>% plot_time_series(time, laacc_x, .smooth = F, .interactive = F)


#IMPORT ACTIVITY AND SCALE TIMES
activity <- read_csv(here(id,session, "coding", "activity.csv"), col_names = c("onset", "offset", "code"))
activity <- activity %>% mutate(across(onset:offset, ~ .x/1000))
valid_codes <- c("d","u","s","sr","ss","sc","w","c","p","hs","hw","l")
activity_special <- activity %>% filter(!code %in% valid_codes)
activity <- activity %>% filter(code %in%valid_codes)

#Offset based on jump time

anno <- read_csv(here(id,session, "coding", "biostamp_annotations.csv")) %>% 
  rename_with(~ janitor::make_clean_names(.x)) %>% 
  mutate(across(start_timestamp_ms:stop_timestamp_ms, ~as_datetime((round(.x/1000, 2)), tz = "America/Los_Angeles")))

sync_point <- anno %>% filter(value == "parent sync") %>% pull(start_timestamp_ms)

activity  <-  activity %>% mutate(across(onset:offset, ~ sync_point + seconds(.x)))

#Apply activity codes to ds
ds$code <- NA
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 

#Make ds subset that has entire coded period, not rest of day
start_time_coded <- activity %>% slice_head %>% pull(onset)
end_time_coded <- activity %>% slice_tail %>% pull(offset)
ds_coded <-ds %>% filter_by_time(time, start_time_coded, end_time_coded)

ds_coded %>% plot_time_series(time, laacc_x, .color_var = code, .smooth = F)


#BY MINUTES FOR DEBUGGING
# slide <- slide_period_dfr(ds, .i = ds$time, .period = "minute", .every = 2, .after = 1, ~ motion_features(.x, "parent"))

#ACTUAL ONE (sliding 4 second windows every 2 seconds)
slide <- slide_period_dfr(ds, .i = ds$time, .period = "second", .every = 2, .after = 1, ~ motion_features(.x, "parent"))

save(slide, file = "slide_inf.RData")



#-----------
#IF MORE CONTROL OVER TIME IS NEEDED, I CAN ADJUST SECONDS BY A MULTIPLIER

#DIVIDE TIME BY 4 TO MAKE EACH SECOND A 4 SECOND WINDOW
#ds$time_compressed <- as.POSIXct(as_date(seconds(ds$time)/4))

# THEN RUN THE SLIDER

#MULTIPLY TIME BY 4 TO RETURN TO ORIGINAL TIMESCALE
#slide$time <- as.POSIXct(as_date(seconds(slide$time)*4))
#------------